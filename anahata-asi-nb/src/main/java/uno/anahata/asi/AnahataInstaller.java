/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.modules.ModuleInstall;
import uno.anahata.asi.nb.util.ElementHandleModule;
import uno.anahata.asi.tool.schema.SchemaProvider;
import uno.anahata.asi.nb.tools.project.nb.AnahataProjectAnnotator;

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
        
        // Register the ElementHandle module for global JSON support in the IDE
        SchemaProvider.OBJECT_MAPPER.registerModule(new ElementHandleModule());
        
        // Load active sessions from disk. This must happen before TopComponents are restored.
        int failed = getContainer().loadSessions();
        if (failed > 0) {
            log.log(Level.WARNING, "{0} sessions failed to load due to incompatibility.", failed);
        }
        
        // Trigger initial refresh of project icons
        AnahataProjectAnnotator.fireRefresh(null);
    }

    /**
     * {@inheritDoc}
     * Shuts down the container when the module is uninstalled.
     */
    @Override
    public void uninstalled() {
        log.info("Anahata ASI V2 Module Uninstalled");
        if (container != null) {
            container.shutdown();
        }
    }
}
