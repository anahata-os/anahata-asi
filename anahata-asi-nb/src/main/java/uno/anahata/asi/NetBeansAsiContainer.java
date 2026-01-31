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
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.nb.module.NetBeansModuleUtils;
import uno.anahata.asi.nb.tools.files.nb.FileAnnotationProvider;
import uno.anahata.asi.nb.tools.project.nb.AnahataProjectAnnotator;
import uno.anahata.asi.model.resource.AbstractPathResource;

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
        
        // Setup global listener for active chat changes to refresh annotations
        this.addPropertyChangeListener("activeChats", evt -> {
            log.info("Active chats changed, triggering global annotation refresh.");
            
            // Refresh UI annotations for all remaining active chats to update labels (1 vs >1)
            for (Chat chat : getActiveChats()) {
                refreshChatAnnotations(chat);
            }
            refreshAllProjectAnnotations();
        });
    }

    /**
     * {@inheritDoc}
     * Attaches a nickname listener to each newly created chat to ensure 
     * IDE annotations are refreshed when the session name changes.
     */
    @Override
    public void onChatCreated(Chat chat) {
        log.info("Initializing NetBeans environment for chat session: {}", chat.getConfig().getSessionId());
        
        // Listen for nickname changes to refresh UI annotations
        chat.addPropertyChangeListener("nickname", evt -> {
            log.info("Nickname changed for chat {}, triggering surgical annotation refresh.", chat.getShortId());
            refreshChatAnnotations(chat);
            refreshAllProjectAnnotations();
        });

        // Only set default model if not already set (e.g. during restoration)
        if (chat.getSelectedModel() == null) {
            chat.setProviderAndModel("Gemini", "models/gemini-3-flash-preview");
        }

        // 1. Initialize Java toolkit classpath for this specific chat instance
        // This is transient and must be re-initialized even for restored sessions.
        chat.getToolManager().getToolkitInstance(Java.class).ifPresent(java -> {
            String nbClasspath = NetBeansModuleUtils.getNetBeansClasspath();
            log.info("Setting NetBeans classpath on Java toolkit instance.");
            java.setDefaultClasspath(nbClasspath);
        });
        
    }

    /**
     * Triggers a comprehensive refresh of all project icons and name annotations 
     * for all currently open projects in the IDE.
     */
    private void refreshAllProjectAnnotations() {
        for (Project p : OpenProjects.getDefault().getOpenProjects()) {
            AnahataProjectAnnotator.fireRefreshAll(p);
        }
    }

    /**
     * Performs a surgical refresh of the IDE UI annotations (badges and labels) 
     * for all file resources currently in the context of the specified chat.
     * <p>
     * This method does NOT reload file content from disk; it only notifies the 
     * IDE that the visual status of these files has changed.
     * </p>
     * 
     * @param chat The chat session whose resources should be refreshed in the UI.
     */
    private void refreshChatAnnotations(Chat chat) {
        Map<FileSystem, Set<FileObject>> toRefresh = new HashMap<>();
        
        chat.getResourceManager().getResources().stream()
            .filter(r -> r instanceof AbstractPathResource)
            .map(r -> (AbstractPathResource<?, ?>) r)
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
            
        toRefresh.forEach(FileAnnotationProvider::fireRefresh);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Chat createNewChat() {
        return new Chat(new NetBeansChatConfig(this));
    }
}
