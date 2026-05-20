/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.modal;

import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleReasoningStyle;


/**
 * Specialized model implementation for Modal's GLM-5 endpoint.
 * Configures the specific context limits and reasoning field defaults.
 * 
 * @author anahata
 */
public class ModalModel extends OpenAiCompatibleModel {

    /**
     * Constructs a new Modal model instance from discrete ID and display name.
     * @param provider    The owning Modal provider.
     * @param modelId     The stable model ID.
     * @param displayName The user-facing name.
     */
    public ModalModel(OpenAiChatCompletionsProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
        configure();
    }

    /**
     * Constructs a new Modal model instance from an OpenAI-compatible JSON discovery node.
     * @param provider The owning Modal provider.
     * @param node     The JSON metadata for the model.
     */
    public ModalModel(OpenAiChatCompletionsProvider provider, com.fasterxml.jackson.databind.JsonNode node) {
        super(provider, node);
        configure();
    }

    /**
     * Internal configuration pipeline that applies Modal-specific overrides 
     * for context window limits and reasoning field detection.
     */
    private void configure() {
        // Modal GLM-5 has a specific context limit around 200K
        setMaxInputTokens(202752);
        setMaxOutputTokens(65536);

        // Modal's GLM-5 uses the reasoning_content field
        setReasoningStyle(OpenAiCompatibleReasoningStyle.FIELD);
        setReasoningFieldName("reasoning_content");
    }
}
