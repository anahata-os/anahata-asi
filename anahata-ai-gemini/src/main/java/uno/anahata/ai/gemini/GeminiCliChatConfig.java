package uno.anahata.ai.gemini;

import uno.anahata.ai.AiConfig;
import uno.anahata.ai.config.ChatConfig;

/**
 * A production-ready ChatConfig for the Gemini CLI that registers the GeminiAiProvider.
 * The core CLI launcher will find this class via reflection to create a runnable
 * CLI for the gemini provider.
 * 
 * @author Anahata
 */
public class GeminiCliChatConfig extends ChatConfig {
    public GeminiCliChatConfig(AiConfig aiConfig) {
        super(aiConfig, "gemini-cli-session");
        // Register the provider that this module implements
        getProviderClasses().add(GeminiAiProvider.class);
    }
}
