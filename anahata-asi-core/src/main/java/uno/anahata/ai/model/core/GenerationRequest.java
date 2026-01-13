/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import java.util.List;
import lombok.NonNull;

/**
 * A simple, immutable record that encapsulates all the information required for
 * a model to generate content: the configuration and the conversation history.
 * This replaces the internal RequestContext in Chat and provides a standardized
 * input for the AbstractModel methods.
 *
 * @author anahata-ai
 * @param config The configuration for the generation request.
 * @param history The list of messages forming the conversation history.
 */
public record GenerationRequest(
    @NonNull RequestConfig config,
    @NonNull List<AbstractMessage> history
) {}
