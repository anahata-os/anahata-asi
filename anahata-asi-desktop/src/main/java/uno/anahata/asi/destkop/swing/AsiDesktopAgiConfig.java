/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.destkop.swing;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.gemini.GeminiAgiProvider;
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * The default {@link uno.anahata.ai.config.AgiConfig} implementation for the 
 * standalone Swing application. It pre-registers the {@link GeminiAgiProvider}.
 * 
 * @author anahata
 */
@Slf4j
public class AsiDesktopAgiConfig extends SwingAgiConfig {
    
    {
        setSelectedProviderUuid("Gemini");
        setSelectedModelId("models/gemini-flash-latest");
    }
    
    /**
     * Constructs a new configuration for a fresh agi session.
     * 
     * @param asiConfig The parent ASI container.
     */
    public AsiDesktopAgiConfig(AbstractAsiContainer asiConfig) {
        super(asiConfig);
    }

    /**
     * Constructs a new configuration for a restored agi session.
     * 
     * @param asiConfig The parent ASI container.
     * @param sessionId The unique ID of the session being restored.
     */
    public AsiDesktopAgiConfig(AbstractAsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
        log.info("StandaloneAgiConfig registering: {}", GeminiAgiProvider.class);
    }
}
