/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.standalone.swing;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.gemini.GeminiAgiProvider;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.render.editorkit.DefaultEditorKitProvider;

/**
 * The default {@link uno.anahata.ai.config.AgiConfig} implementation for the 
 * standalone Swing application. It pre-registers the {@link GeminiAgiProvider}.
 * 
 * @author anahata
 */
@Slf4j
public class StandaloneAgiConfig extends SwingAgiConfig {
    
    {
        // Automatically register the Gemini provider for standalone use.
        getProviderClasses().add(GeminiAgiProvider.class);
    }
    
    /**
     * Constructs a new configuration for a fresh agi session.
     * 
     * @param asiConfig The parent ASI container.
     */
    public StandaloneAgiConfig(AsiContainer asiConfig) {
        super(asiConfig);
        super.setEditorKitProvider(new DefaultEditorKitProvider());
    }

    /**
     * Constructs a new configuration for a restored agi session.
     * 
     * @param asiConfig The parent ASI container.
     * @param sessionId The unique ID of the session being restored.
     */
    public StandaloneAgiConfig(AsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
        log.info("StandaloneAgiConfig registering: {}", GeminiAgiProvider.class);
    }
}
