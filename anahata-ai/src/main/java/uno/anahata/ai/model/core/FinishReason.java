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
package uno.anahata.ai.model.core;

/**
 * A standardized, model-agnostic enum representing the reason why a model
 * stopped generating content.
 */
public enum FinishReason {
    /** Natural stop point of the model or provided stop sequence. */
    STOP,
    /** The maximum number of tokens as specified in the request was reached. */
    MAX_TOKENS,
    /** The content was flagged for safety reasons. */
    SAFETY,
    /** The content was flagged for recitation reasons. */
    RECITATION,
    /** The model decided to call a tool. */
    TOOL_EXECUTION,
    /** The reason is unknown or not specified. */
    OTHER;
}
