/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.nb.module.NetBeansModuleUtils;
import uno.anahata.asi.nb.tools.java.CodeModel;
import uno.anahata.asi.nb.tools.maven.Maven;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * NetBeans-specific configuration for the Anahata ASI.
 * Handles IDE-specific initialization, such as building the comprehensive
 * module classpath for the Java toolkit.
 * 
 * @author anahata
 */
@Slf4j
public class NetBeansAsiContainer extends AsiContainer {

    public NetBeansAsiContainer() {
        super("netbeans");
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
