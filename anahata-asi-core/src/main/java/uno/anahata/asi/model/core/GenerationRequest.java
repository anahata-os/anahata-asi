/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.util.List;
import lombok.NonNull;

/**
 * A simple, immutable record that encapsulates all the information required for
 * a model to generate content: the configuration and the conversation history.
 * <p>
 * This record serves as the primary input for the {@code generateContent} and 
 * {@code generateContentStream} methods in {@code AbstractModel}. It decouples 
 * the model implementation from the complex {@code Chat} orchestrator by 
 * providing a snapshot of the request parameters and the relevant message history.
 * </p>
 *
 * @author anahata-ai
 * @param config The configuration for the generation request, including 
 *               behavioral parameters (temperature, topK, etc.) and tool 
 *               definitions.
 * @param history The list of messages forming the conversation history to be 
 *                sent to the model. This list is typically filtered and 
 *                processed by the {@code ContextManager} before being 
 *                encapsulated in this request.
 */
public record GenerationRequest(
    /**
     * The configuration for the generation request.
     * @return the request configuration.
     */
    @NonNull RequestConfig config,
    /**
     * The list of messages forming the conversation history.
     * @return the message history.
     */
    @NonNull List<AbstractMessage> history
) {}
