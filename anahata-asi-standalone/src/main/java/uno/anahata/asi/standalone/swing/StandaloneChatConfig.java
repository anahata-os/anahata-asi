/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.standalone.swing;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.swing.chat.SwingChatConfig;

/**
 * The default {@link uno.anahata.ai.config.ChatConfig} implementation for the 
 * standalone Swing application. It pre-registers the {@link GeminiAiProvider}.
 * 
 * @author anahata
 */
@Slf4j
public class StandaloneChatConfig extends SwingChatConfig {
    
    {
        // Automatically register the Gemini provider for standalone use.
        getProviderClasses().add(GeminiAiProvider.class);
    }
    
    /**
     * Constructs a new configuration for a fresh chat session.
     * 
     * @param asiConfig The parent ASI container.
     */
    public StandaloneChatConfig(AsiContainer asiConfig) {
        super(asiConfig);
    }

    /**
     * Constructs a new configuration for a restored chat session.
     * 
     * @param asiConfig The parent ASI container.
     * @param sessionId The unique ID of the session being restored.
     */
    public StandaloneChatConfig(AsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
        log.info("StandaloneChatConfig registering: {}", GeminiAiProvider.class);
    }
}
