/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini;

import uno.anahata.ai.AiConfig;
import uno.anahata.ai.chat.ChatConfig;

/**
 * A production-ready {@code ChatConfig} for the Gemini CLI that registers the {@code GeminiAiProvider}.
 * The core CLI launcher will find this class via reflection to create a runnable
 * CLI for the Gemini provider.
 *
 * @author anahata-gemini-pro-2.5
 */
public class GeminiCliChatConfig extends ChatConfig {
    public GeminiCliChatConfig(AiConfig aiConfig) {
        super(aiConfig, "gemini-cli-session");
        // Register the provider that this module implements
        getProviderClasses().add(GeminiAiProvider.class);
    }
}
