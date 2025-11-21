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
import uno.anahata.ai.model.core.Request;
import uno.anahata.ai.model.core.Response;

/**
 * Base class for all AI models, defining a common set of capabilities.
 * Provider-specific implementations will extend this class.
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

    /**
     * Gets a detailed, provider-specific raw description of the model,
     * typically for UI tooltips or debugging.
     * @return A formatted string (e.g., HTML) with the model's raw details.
     */
    public abstract String getRawDescription();
    
    public String getProviderId() {
        return getProvider().getProviderId();
    }
    
    // --- Abstract Capability Methods ---
    public abstract boolean isSupportsFunctionCalling();
    public abstract boolean isSupportsContentGeneration();
    public abstract boolean isSupportsBatchEmbeddings();
    public abstract boolean isSupportsEmbeddings();
    public abstract boolean isSupportsCachedContent();
    
    /**
     * Generates content based on a model-agnostic request.
     * This is the core method that model implementations must override.
     *
     * @param request The standardized model request.
     * @return The standardized model response.
     */
    public abstract Response generateContent(Request request);
}
