/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai.gemini;

import uno.anahata.ai.AiConfig;
import uno.anahata.ai.config.ChatConfig;

/**
 * A production-ready ChatConfig for the Gemini CLI that registers the GeminiAiProvider.
 * The core CLI launcher will find this class via reflection to create a runnable
 * CLI for the gemini provider.
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
