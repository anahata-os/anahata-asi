/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.model.provider;

import java.util.List;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.GenerationRequest;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.StreamObserver;

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
}
