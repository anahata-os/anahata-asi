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

/**
 * A simple main class in the gemini provider module that launches the core,
 * provider-agnostic command-line interface.
 * 
 * @author anahata-gemini-pro-2.5
 */
public class Main {
    public static void main(String[] args) {
        // Delegate directly to the core CLI's main method.
        // The CLI will use reflection to find the GeminiCliChatConfig in this module's
        // classpath, which in turn registers the GeminiAiProvider.
        uno.anahata.ai.Cli.main(args);
    }
}
