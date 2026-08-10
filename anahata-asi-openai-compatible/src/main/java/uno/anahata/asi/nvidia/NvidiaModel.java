/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nvidia;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleReasoningStyle;

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
        configureReasoning();
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
        configureReasoning();
    }

    /**
     * Configures default reasoning style and tags/field name based on model ID heuristics.
     */
    private void configureReasoning() {
        String lowerId = getModelId().toLowerCase();
        if (lowerId.contains("deepseek") || lowerId.contains("r1") || lowerId.contains("qwq") || lowerId.contains("reasoning") || lowerId.contains("nemotron")) {
            setReasoningStyle(OpenAiCompatibleReasoningStyle.FIELD);
            setReasoningFieldName("reasoning_content");
        } else if (lowerId.contains("think")) {
            setReasoningStyle(OpenAiCompatibleReasoningStyle.TAGS);
            setReasoningTags(List.of("<think>", "</think>"));
        }
    }
}
