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
package uno.anahata.ai.model.provider;

import java.util.List;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;

/**
 * The abstract base class for a specific AI model (e.g., "gemini-1.5-pro-latest").
 * In the V2 architecture, this class is the definitive entry point for generating
 * content, creating a clean, object-oriented API where the model itself is the
 * actor.
 *
 * @author anahata-gemini-pro-2.5
 */
public abstract class AbstractModel {

    public abstract AbstractAiProvider getProvider();

    public abstract String getModelId();

    public abstract String getDisplayName();

    public abstract String getDescription();

    public abstract String getVersion();

    public abstract int getMaxInputTokens();

    public abstract int getMaxOutputTokens();

    public abstract List<String> getSupportedActions();

    public abstract String getRawDescription();

    /**
     * Delegate method to get the id of this models provider.
     * 
     * @return the provider id.
     */
    public final String getProviderId() {
        return getProvider().getProviderId();
    }
    
    // --- Abstract Capability Methods ---
    public abstract boolean isSupportsFunctionCalling();

    public abstract boolean isSupportsContentGeneration();

    public abstract boolean isSupportsBatchEmbeddings();

    public abstract boolean isSupportsEmbeddings();

    public abstract boolean isSupportsCachedContent();

    /**
     * The core method for interacting with an AI model. It takes a configuration
     * object and a list of messages and returns a standardized Response.
     *
     * @param config The configuration for this specific request.
     * @param history The list of messages forming the conversation history.
     * @return A standardized {@link Response} object.
     */
    public abstract Response generateContent(RequestConfig config, List<AbstractMessage> history);
}
