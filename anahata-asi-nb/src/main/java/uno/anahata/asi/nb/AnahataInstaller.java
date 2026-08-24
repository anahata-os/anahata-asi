/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.core.windows.persistence.PersistenceManager;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.modules.ModuleInstall;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.nb.ui.resources.NbResourceUI;
import uno.anahata.asi.nb.util.AnahataUpdateCenterUtils;
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

    /**
     * Appends a timestamped lifecycle diagnostic entry to {@code ~/.anahata/asi/netbeans/lifecycle.log}.
     * <p>
     * Includes the active {@link ClassLoader}, thread name, and message to provide full traceability
     * during module initialization, restoration, and {@code nbmreload} cycles.
     * </p>
     *
     * @param message The diagnostic message to record in the lifecycle log.
     */
    public static synchronized void logLifecycle(String message) {
        try {
            Path logFile = NetBeansAsiContainer.getWorkDirSubDir("netbeans").resolve("lifecycle.log");
            String timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
            String threadName = Thread.currentThread().getName();
            String line = String.format("[%s] [%s] [%s] %s%n", AnahataInstaller.class.getClassLoader(), timestamp, threadName, message);
            Files.writeString(logFile, line, StandardOpenOption.CREATE, StandardOpenOption.APPEND);
            log.info("Logged to lifecycle file: " + line);
        } catch (Exception e) {
            log.log(Level.SEVERE, "Failed to write to lifecycle log", e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Performs module initialization, auto-registers the official Anahata
     * Update Center, and sets up global listeners for UI updates.
     * </p>
     */
    @Override
    public void restored() {
        logLifecycle("AnahataInstaller.restored() ENTER");
        log.info("Anahata ASI NetBeans Module Restored");

        // Auto-register the official Anahata Update Center if not present
        AnahataUpdateCenterUtils.registerDefaultUpdateCenter();

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

        //dumpTopComponents("restored()");
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
     * Shuts down the container and detaches all {@link ReloadableTopComponent}s
     * when the module is uninstalled. This is critical to prevent classloader
     * leaks during nbmreload.
     * </p>
     */
    @Override
    public void uninstalled() {
        logLifecycle("AnahataInstaller.uninstalled() ENTER");
        System.setProperty("anahata.nbmreload.pending", "true");

        try {
            SwingUtils.runInEDTAndWait(() -> {
                // 1. Detach all open TopComponents (stops timers, nulls panels, closes tabs)
                Set<TopComponent> allTCs = new HashSet<>(WindowManager.getDefault().getRegistry().getOpened());
                
                for (TopComponent tc : allTCs) {
                    if (tc instanceof ReloadableTopComponent rtc) {
                        logLifecycle("calling detachForNbmReload on opened " + tc);
                        rtc.detachForNbmReload();
                    }
                }

                // 2. Invalidate cached DataObjects in Windows2Local/Components so DataObjectPool releases SoftReferences
                invalidateWindows2LocalComponentDataObjects();

                // 3. Clear 'AGI' from PersistenceManager globalIDSet to prevent _1, _2 suffixes
                try {
                    Field globalIdField = PersistenceManager.class.getDeclaredField("globalIDSet");
                    globalIdField.setAccessible(true);
                    Set<String> globalIdSet = (Set<String>) globalIdField.get(PersistenceManager.getDefault());
                    globalIdSet.remove("AGI");
                } catch (Exception ex) {
                    log.log(Level.SEVERE, "Exception clearing AGI from persistence managers globalIdSet", ex);
                }
            });
        } catch (Exception ex) {
            log.log(Level.SEVERE, "Failed to detach TopComponents during uninstall", ex);
        }

        if (container != null) {
            container.shutdown();
        }
        logLifecycle("AnahataInstaller.uninstalled() EXIT");
    }

    /**
     * Invalidates all cached DataObjects in {@code Windows2Local/Components} corresponding to
     * Anahata TopComponents (e.g. {@code agi.settings}, {@code AsiCardsTopComponent.settings},
     * {@code AsiTableTopComponent.settings}).
     * <p>
     * Calling {@link DataObject#setValid} on these settings DataObjects forces NetBeans'
     * global {@code DataObjectPool} to evict the {@link org.openide.loaders.InstanceDataObject}s
     * and their cached {@link java.lang.ref.SoftReference}s, which in turn causes NetBeans'
     * {@link org.netbeans.core.windows.persistence.PersistenceManager} to automatically purge the
     * old module ClassLoader instances from {@code id2TopComponentMap} and {@code topComponent2IDMap}.
     * </p>
     */
    private static void invalidateWindows2LocalComponentDataObjects() {
        try {
            FileObject configRoot = FileUtil.getConfigRoot();
            FileObject localComps = configRoot.getFileObject("Windows2Local/Components");
            if (localComps != null) {
                for (FileObject fo : localComps.getChildren()) {
                    String name = fo.getName();
                    if (name.startsWith("Asi") || name.startsWith("agi")) {
                        try {
                            DataObject dob = DataObject.find(fo);
                            if (dob != null) {
                                logLifecycle("Invalidating " + fo + "/" + dob);
                                dob.setValid(false);
                            } else {
                                logLifecycle("No dob for " + fo);
                            }
                        } catch (Exception ex) {
                            log.log(Level.SEVERE, "Exception invalidating data object for " + fo, ex);
                        }
                    }
                }
            } else {
                logLifecycle("No local components to invalidate");
            }
            System.gc();
        } catch (Exception ex) {
            log.log(Level.SEVERE, "Could not invalidate component data objects in Windows2Local/Components", ex);
        }
    }
    
}
