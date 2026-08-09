/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openide.modules.ModuleInstall;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.nb.ui.resources.NbResourceUI;
import uno.anahata.asi.nb.util.ElementHandleModule;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Installer for the Anahata ASI NetBeans module. Handles lifecycle management
 * and global UI synchronization.
 * <p>
 * This class leverages NetBeans' native window system persistence for
 * TopComponents, eliminating the need for manual handoff files.
 * </p>
 *
 * @author anahata
 */
public class AnahataInstaller extends ModuleInstall {

    /**
     * Logger instance for module lifecycle events.
     */
    private static final Logger log = Logger.getLogger(AnahataInstaller.class.getName());

    /**
     * The singleton container instance.
     */
    private static NetBeansAsiContainer container;

    /**
     * Gets the global ASI container for NetBeans.
     *
     * @return The container instance.
     */
    public static synchronized NetBeansAsiContainer getContainer() {
        if (container == null) {
            container = new NetBeansAsiContainer();
        }
        return container;
    }

    public static synchronized void logLifecycle(String message) {
        try {
            Path logFile = NetBeansAsiContainer.getWorkDirSubDir("netbeans").resolve("lifecycle.log");
            String timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
            String threadName = Thread.currentThread().getName();
            String line = String.format("[%s] [%s] %s%n", timestamp, threadName, message);
            Files.writeString(logFile, line, StandardOpenOption.CREATE, StandardOpenOption.APPEND);
        } catch (Exception e) {
            log.log(Level.SEVERE, "Failed to write to lifecycle log", e);
        }
    }

    /**
     * {@inheritDoc} Performs module initialization and sets up global listeners
     * for UI updates.
     */
    @Override
    public void restored() {
        logLifecycle("AnahataInstaller.restored() ENTER");
        log.info("Anahata ASI NetBeans Module Restored");

        // Register the NetBeans-native resource UI strategy
        ResourceUiRegistry.getInstance().setResourceUI(new NbResourceUI());

        // Register the ElementHandle module for global JSON support in the IDE
        SchemaProvider.OBJECT_MAPPER.registerModule(new ElementHandleModule());

        // Load active sessions from disk. This must happen before TopComponents are restored.
        logLifecycle("AnahataInstaller.restored() calling loadSessions()");
        int failed = getContainer().loadSessions();
        logLifecycle("AnahataInstaller.restored() loadSessions() finished. Failed count=" + failed + ", activeAgis count=" + getContainer().getActiveAgis().size());
        if (failed > 0) {
            log.log(Level.WARNING, "{0} sessions failed to load due to incompatibility.", failed);
        }

        boolean isNbmReload = "true".equals(System.getProperty("anahata.nbmreload.pending"));

        if (isNbmReload) {
            logLifecycle("AnahataInstaller.restored() detected nbmreload. Reopening open session tabs.");
            for (Agi agi : getContainer().getOpenAgis()) {
                logLifecycle("AnahataInstaller.restored() reopening tab after nbmreload for session: " + agi.getShortId());
                getContainer().open(agi);
            }
        } else {
            logLifecycle("AnahataInstaller.restored() detected IDE boot. Deferring window restoration to NetBeans WindowManager.");
        }

        System.clearProperty("anahata.nbmreload.pending");

        logLifecycle("AnahataInstaller.restored() EXIT");

    }

    /**
     * {@inheritDoc}
     * <p>
     * Shuts down the container and closes/detaches all TopComponents when the
     * module is uninstalled. This is critical to prevent classloader leaks
     * during nbmreload.</p>
     */
    @Override
    public void uninstalled() {
        logLifecycle("AnahataInstaller.uninstalled() ENTER");
        log.log(Level.INFO, "Anahata ASI V2 Module Uninstalled - Thread: {0}", Thread.currentThread().getName());

        System.setProperty("anahata.nbmreload.pending", "true");

        try {
            SwingUtils.runInEDTAndWait(() -> {
                Set<TopComponent> allTCs = new HashSet<>(WindowManager.getDefault().getRegistry().getOpened());
                for (Mode mode : WindowManager.getDefault().getModes()) {
                    allTCs.addAll(Arrays.asList(mode.getTopComponents()));
                }

                for (TopComponent tc : allTCs) {
                    if (tc instanceof AgiTopComponent atc) {
                        log.log(Level.INFO, "Detaching AgiTopComponent for reload: {0}", atc.getName());
                        atc.detachForNbmReload();
                    } else if (tc.getClass().getName().startsWith("uno.anahata.asi")) {
                        log.log(Level.INFO, "Closing TopComponent to prevent leak: {0}", tc.getClass().getName());
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
        logLifecycle("AnahataInstaller.uninstalled() EXIT");
    }
}
