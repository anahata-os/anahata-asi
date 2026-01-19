/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.standalone.swing;

import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.swing.chat.SwingChatConfig;

/**
 *
 * @author pablo
 */
public class StandaloneChatConfig extends SwingChatConfig{
    public StandaloneChatConfig(AsiContainer asiConfig) {
        super(asiConfig);
    }

    public StandaloneChatConfig(AsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
        getProviderClasses().add(GeminiAiProvider.class);
    }
}
