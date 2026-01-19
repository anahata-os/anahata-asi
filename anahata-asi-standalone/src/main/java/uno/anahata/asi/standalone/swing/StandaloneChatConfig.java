/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.standalone.swing;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.swing.chat.SwingChatConfig;

/**
 *
 * @author anahata
 */
@Slf4j
public class StandaloneChatConfig extends SwingChatConfig{
    {
        getProviderClasses().add(GeminiAiProvider.class);
    }
    public StandaloneChatConfig(AsiContainer asiConfig) {
        super(asiConfig);
    }

    public StandaloneChatConfig(AsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
        log.info("StandaloneChatConfig registering: " + GeminiAiProvider.class);
    }
}
