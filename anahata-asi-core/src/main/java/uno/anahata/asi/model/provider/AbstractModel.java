/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.model.provider;

import java.util.List;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.GenerationRequest;
import uno.anahata.asi.model.core.RequestConfig;
import uno.anahata.asi.model.core.Response;
import uno.anahata.asi.model.core.StreamObserver;
import uno.anahata.asi.model.tool.AbstractTool;

/**
 * The abstract base class for a specific AI model (e.g., "gemini-1.5-pro-latest").
 * In the V2 architecture, this class is the definitive entry point for generating
 * content, creating a clean, object-oriented API where the model itself is the
 * actor.
 *
 * @author anahata-gemini-pro-2.5
 */
public abstract class AbstractModel {

    /**
     * Gets the provider that owns this model.
     * @return The parent AI provider.
     */
    public abstract AbstractAgiProvider getProvider();

    /**
     * Gets the unique identifier for this model (e.g., "models/gemini-1.5-pro").
     * @return The model ID.
     */
    public abstract String getModelId();

    /**
     * Gets the human-readable display name for this model.
     * @return The display name.
     */
    public abstract String getDisplayName();

    /**
     * Gets a detailed description of the model's capabilities and limitations.
     * @return The model description.
     */
    public abstract String getDescription();

    /**
     * Gets the version string for this model.
     * @return The version.
     */
    public abstract String getVersion();

    /**
     * Gets the maximum number of input tokens supported by this model.
     * @return The input token limit.
     */
    public abstract int getMaxInputTokens();

    /**
     * Gets the maximum number of output tokens this model can generate in a single turn.
     * @return The output token limit.
     */
    public abstract int getMaxOutputTokens();

    /**
     * Gets the list of supported API actions for this model (e.g., "generateContent").
     * @return A list of supported actions.
     */
    public abstract List<String> getSupportedActions();

    /**
     * Gets a rich, potentially HTML-formatted description of the model, 
     * including all its metadata.
     * @return The raw description string.
     */
    public abstract String getRawDescription();

    /**
     * Delegate method to get the id of this models provider.
     * 
     * @return The unique ID of the provider.
     */
    public final String getProviderId() {
        return getProvider().getProviderId();
    }
    
    // --- Abstract Capability Methods ---
    /**
     * Checks if this model supports native function calling (tools).
     * @return true if supported.
     */
    public abstract boolean isSupportsFunctionCalling();

    /**
     * Checks if this model supports content generation.
     * @return true if supported.
     */
    public abstract boolean isSupportsContentGeneration();

    /**
     * Checks if this model supports batch embedding generation.
     * @return true if supported.
     */
    public abstract boolean isSupportsBatchEmbeddings();

    /**
     * Checks if this model supports single content embedding generation.
     * @return true if supported.
     */
    public abstract boolean isSupportsEmbeddings();

    /**
     * Checks if this model supports content caching.
     * @return true if supported.
     */
    public abstract boolean isSupportsCachedContent();

    /**
     * Gets the list of response modalities supported by this model (e.g., "TEXT", "IMAGE", "AUDIO").
     * 
     * @return A list of supported response modalities.
     */
    public abstract List<String> getSupportedResponseModalities();

    /**
     * Gets the list of server-side tools available for this model.
     * 
     * @return A list of available server tools.
     */
    public abstract List<ServerTool> getAvailableServerTools();
    
    /**
     * Gets the list of server-side tools that should be enabled by default for this model.
     * 
     * @return The list of default server tools.
     */
    public abstract List<ServerTool> getDefaultServerTools();

    /**
     * Gets the default temperature for this model.
     * 
     * @return The default temperature, or null if not specified.
     */
    public abstract Float getDefaultTemperature();

    /**
     * Gets the default topK for this model.
     * 
     * @return The default topK, or null if not specified.
     */
    public abstract Integer getDefaultTopK();

    /**
     * Gets the default topP for this model.
     * 
     * @return The default topP, or null if not specified.
     */
    public abstract Float getDefaultTopP();

    /**
     * The core method for interacting with an AI model. It takes a configuration
     * object and a list of messages and returns a standardized Response.
     *
     * @param request The generation request containing config and history.
     * @return A standardized {@link Response} object.
     */
    public abstract Response generateContent(GenerationRequest request);

    /**
     * Generates content asynchronously using token streaming.
     *
     * @param request The generation request containing config and history.
     * @param observer The observer that will receive the streaming response chunks.
     */
    public abstract void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer);

    /**
     * Gets the provider-specific JSON representation of a tool's declaration.
     * This is used by the UI to show exactly what is being sent to the model.
     * 
     * @param tool The tool to inspect.
     * @param config The request configuration (e.g. to check useNativeSchemas).
     * @return The JSON string representing the tool declaration.
     */
    public abstract String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config);
}
