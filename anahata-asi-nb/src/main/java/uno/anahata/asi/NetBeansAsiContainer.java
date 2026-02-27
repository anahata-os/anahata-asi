/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ui.OpenProjects;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.cookies.EditorCookie;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.nb.module.NetBeansModuleUtils;
import uno.anahata.asi.nb.tools.files.nb.AnahataAnnotationProvider;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.resource.AbstractResource;

/**
 * NetBeans-specific configuration for the Anahata ASI.
 * Handles IDE-specific initialization, such as building the comprehensive
 * module classpath for the Java toolkit and managing global UI synchronization.
 * 
 * @author anahata
 */
@Slf4j
public class NetBeansAsiContainer extends AsiContainer {

    /**
     * Default constructor for the NetBeans container.
     */
    public NetBeansAsiContainer() {
        super("netbeans");
        
        // Setup global listener for active agi changes to refresh annotations
        this.addPropertyChangeListener("activeAgis", evt -> {
            log.info("Active agis changed, triggering global annotation refresh.");
            
            // Refresh UI annotations for all remaining active agis to update labels (1 vs >1)
            for (Agi agi : getActiveAgis()) {
                refreshAgiAnnotations(agi);
            }
        });
    }

    /**
     * {@inheritDoc}
     * Attaches a nickname listener to each newly created agi to ensure 
     * IDE annotations are refreshed when the session name changes.
     */
    @Override
    public void onAgiCreated(Agi agi) {
        log.info("Initializing NetBeans environment for agi session: {}", agi.getConfig().getSessionId());
        
        // Listen for nickname changes to refresh UI annotations
        agi.addPropertyChangeListener("nickname", evt -> {
            log.info("Nickname changed for agi {}, triggering surgical annotation refresh.", agi.getShortId());
            refreshAgiAnnotations(agi);
        });

        // Only set default model if not already set (e.g. during restoration)
        if (agi.getSelectedModel() == null) {
            agi.setProviderAndModel("Gemini", "models/gemini-3-flash-preview");
        }

        // 1. Initialize Java toolkit classpath for this specific agi instance
        // This is transient and must be re-initialized even for restored sessions.
        agi.getToolManager().getToolkitInstance(Java.class).ifPresent(java -> {
            String nbClasspath = NetBeansModuleUtils.getNetBeansClasspath();
            log.info("Setting NetBeans classpath on Java toolkit instance.");
            java.setDefaultClasspath(nbClasspath);
        });
        
    }

    
    /**
     * Performs a surgical refresh of the IDE UI annotations (badges and labels) 
     * for all file resources currently in the context of the specified agi.
     * <p>
     * This method does NOT reload file content from disk; it only notifies the 
     * IDE that the visual status of these files has changed.
     * </p>
     * 
     * @param agi The agi session whose resources should be refreshed in the UI.
     */
    private void refreshAgiAnnotations(Agi agi) {
        Map<FileSystem, Set<FileObject>> toRefresh = new HashMap<>();
        
        agi.getResourceManager().getResources().stream()
            .filter(r -> r instanceof AbstractPathResource)
            .map(r -> (AbstractPathResource<?>) r)
            .forEach(r -> {
                FileObject fo = FileUtil.toFileObject(new File(r.getPath()));
                if (fo != null) {
                    try {
                        FileSystem fs = fo.getFileSystem();
                        toRefresh.computeIfAbsent(fs, k -> new HashSet<>()).add(fo);
                    } catch (Exception ex) {
                        log.warn("Failed to get filesystem for resource: " + r.getPath(), ex);
                    }
                }
            });
            
        toRefresh.forEach(AnahataAnnotationProvider::fireRefresh);
    }

    /**
     * Finds an existing active agi by its session ID, or creates a new one
     * if the ID is null or not found.
     * 
     * @param sessionId The session ID to find.
     * @return The found or newly created agi session.
     */
    public Agi findOrCreateAgi(String sessionId) {
        if (sessionId != null) {
            for (Agi agi : getActiveAgis()) {
                if (agi.getConfig().getSessionId().equals(sessionId)) {
                    return agi;
                }
            }
        }
        return createNewAgi();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Agi createNewAgi() {
        return new Agi(new NetBeansAgiConfig(this));
    }

    /**
     * {@inheritDoc}
     * Opens the file associated with the resource in the NetBeans editor.
     */
    @Override
    public void openResource(AbstractResource<?, ?> resource) {
        if (resource instanceof AbstractPathResource<?> apr) {
            File file = new File(apr.getPath());
            FileObject fo = FileUtil.toFileObject(file);
            if (fo != null) {
                try {
                    DataObject dobj = DataObject.find(fo);
                    EditorCookie ec = dobj.getLookup().lookup(EditorCookie.class);
                    if (ec != null) {
                        ec.open();
                    }
                } catch (Exception ex) {
                    log.error("Failed to open resource in NetBeans editor: " + apr.getPath(), ex);
                }
            }
        }
    }
}
