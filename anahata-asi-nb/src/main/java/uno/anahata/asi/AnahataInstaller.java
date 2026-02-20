/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.modules.ModuleInstall;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.nb.util.ElementHandleModule;
import uno.anahata.asi.tool.schema.SchemaProvider;
import uno.anahata.asi.nb.tools.project.nb.AnahataProjectIconAnnotator;
import uno.anahata.asi.nb.ui.render.TextFileUpdateRenderer;
import uno.anahata.asi.swing.chat.render.ParameterRendererFactory;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.toolkit.files.TextFileUpdate;

/**
 * Installer for the Anahata ASI V2 module.
 * Handles lifecycle management and global UI synchronization.
 * <p>
 * This class leverages NetBeans' native window system persistence for 
 * TopComponents, eliminating the need for manual handoff files.
 * </p>
 * 
 * @author anahata
 */
public class AnahataInstaller extends ModuleInstall {

    private static final Logger log = Logger.getLogger(AnahataInstaller.class.getName());
    
    /** The singleton container instance. */
    private static AsiContainer container;

    /**
     * Gets the global ASI container for NetBeans.
     * @return The container instance.
     */
    public static synchronized AsiContainer getContainer() {
        if (container == null) {
            container = new NetBeansAsiContainer();
        }
        return container;
    }

    /**
     * {@inheritDoc}
     * Performs module initialization and sets up global listeners for UI updates.
     */
    @Override
    public void restored() {
        log.info("Anahata ASI V2 Module Restored");
        
        // Register specialized parameter renderers
        ParameterRendererFactory.register(TextFileUpdate.class, TextFileUpdateRenderer.class);
        
        // Register the ElementHandle module for global JSON support in the IDE
        SchemaProvider.OBJECT_MAPPER.registerModule(new ElementHandleModule());
        
        // Load active sessions from disk. This must happen before TopComponents are restored.
        int failed = getContainer().loadSessions();
        if (failed > 0) {
            log.log(Level.WARNING, "{0} sessions failed to load due to incompatibility.", failed);
        }
        
        // Trigger initial refresh of project icons
        //AnahataProjectIconAnnotator.fireRefresh();
    }

    /**
     * {@inheritDoc}
     * Shuts down the container and closes all TopComponents when the module is uninstalled.
     * This is critical to prevent classloader leaks during nbmreload.
     */
    @Override
    public void uninstalled() {
        log.log(Level.INFO, "Anahata ASI V2 Module Uninstalled - Thread: {0}", Thread.currentThread().getName());
        
        try {
            SwingUtils.runInEDTAndWait(() -> {
                Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
                for (TopComponent tc : opened) {
                    String clazz = tc.getClass().getName();
                    if (clazz.startsWith("uno.anahata.asi")) {
                        log.log(Level.INFO, "Closing TopComponent to prevent leak: {0}", clazz);
                        tc.close();
                    }
                }
            });
        } catch (Exception ex) {
            log.log(Level.SEVERE, "Failed to close TopComponents during uninstall", ex);
        }

        if (container != null) {
            container.shutdown();
            log.info("AsiContainer shutdown complete.");
        }
    }
}
