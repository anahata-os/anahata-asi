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
 * Fora Bara!
 */
package uno.anahata.ai.context.provider;

/**
 * Defines the position where a ContextProvider's content should be injected into the request.
 * This is a direct port of the proven V1 enum.
 * 
 * @author pablo
 */
public enum ContextPosition {
    /**
     * The content should be treated as system instructions, which are typically
     * prepended to the conversation or sent as a separate parameter to the API.
     * Most models only support TextParts in this position.
     */
    SYSTEM_INSTRUCTIONS,
    
    /**
     * The content should be appended to the end of the user's prompt as a synthetic
     * UserMessage, providing just-in-time context or workspace information.
     * This position supports both TextParts and BlobParts.
     */
    PROMPT_AUGMENTATION;
}
