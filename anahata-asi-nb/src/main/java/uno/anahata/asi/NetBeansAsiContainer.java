/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.util.Collections;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ui.OpenProjects;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.nb.module.NetBeansModuleUtils;
import uno.anahata.asi.nb.tools.project.nb.AnahataProjectAnnotator;
import uno.anahata.asi.nb.tools.files.nb.FileAnnotationProvider;

/**
 * NetBeans-specific configuration for the Anahata ASI.
 * Handles IDE-specific initialization, such as building the comprehensive
 * module classpath for the Java toolkit and managing global UI synchronization.
 * 
 * @author anahata
 */
@Slf4j
public class NetBeansAsiContainer extends AsiContainer {

    public NetBeansAsiContainer() {
        super("netbeans");
        
        // Setup global listener for active chat changes to refresh annotations
        this.addPropertyChangeListener("activeChats", evt -> {
            log.info("Active chats changed, triggering global annotation refresh.");
            AnahataProjectAnnotator.fireRefresh(null);
            
            // Refresh file annotations for all open project roots to update [n] suffix
            for (Project p : OpenProjects.getDefault().getOpenProjects()) {
                FileAnnotationProvider.fireRefresh(Collections.singleton(p.getProjectDirectory()));
            }
        });
    }

    @Override
    public void onChatCreated(Chat chat) {
        log.info("Initializing NetBeans environment for chat session: {}", chat.getConfig().getSessionId());
        
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

    @Override
    public Chat createNewChat() {
        return new Chat(new NetBeansChatConfig(this));
    }
}
