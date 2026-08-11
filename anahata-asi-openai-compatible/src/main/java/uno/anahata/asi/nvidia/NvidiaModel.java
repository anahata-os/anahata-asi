/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nvidia;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.ThinkingLevel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;

/**
 * Concrete model implementation for NVIDIA NIM microservices endpoints.
 * <p>
 * Handles reasoning content extraction for NVIDIA NIM models, auto-detecting
 * whether thoughts are returned via {@code reasoning_content} fields or {@code <think>} tags.
 * </p>
 *
 * @author anahata
 */
public class NvidiaModel extends OpenAiCompatibleModel {

    /**
     * Constructs a new NvidiaModel instance from a JSON metadata node.
     *
     * @param provider The parent NVIDIA AI provider.
     * @param node The JSON node containing model metadata.
     */
    public NvidiaModel(NvidiaAiProvider provider, JsonNode node) {
        super(provider, node);
    }

    /**
     * Constructs a new NvidiaModel instance with explicit model ID and display name.
     *
     * @param provider The parent NVIDIA AI provider.
     * @param modelId The unique model ID.
     * @param displayName The human-readable display name.
     */
    public NvidiaModel(NvidiaAiProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
    }

    @Override
    protected void enrichPayload(ObjectNode payload, GenerationRequest request) {
        super.enrichPayload(payload, request);

        boolean includeThoughts = request.config().getAgi().getConfig().isIncludeThoughts();
        ThinkingLevel level = request.config().getThinkingLevel();

        boolean enableThinking = includeThoughts && level != ThinkingLevel.NONE;
        payload.putObject("chat_template_kwargs").put("enable_thinking", enableThinking);

        if (enableThinking && !payload.has("reasoning_effort")) {
            payload.put("reasoning_effort", "high");
        }
    }
}
