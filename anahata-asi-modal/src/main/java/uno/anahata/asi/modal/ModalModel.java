/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.modal;

import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleProvider;
import uno.anahata.asi.openai.compatible.ReasoningStyle;


/**
 * Specialized model implementation for Modal's GLM-5 endpoint.
 * Configures the specific context limits and reasoning field defaults.
 * 
 * @author anahata
 */
public class ModalModel extends OpenAiCompatibleModel {

    public ModalModel(OpenAiCompatibleProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
        configure();
    }

    public ModalModel(OpenAiCompatibleProvider provider, com.fasterxml.jackson.databind.JsonNode node) {
        super(provider, node);
        configure();
    }

    private void configure() {
        // Modal GLM-5 has a specific context limit around 200K
        setMaxInputTokens(202752);
        setMaxOutputTokens(65536);

        // Modal's GLM-5 uses the reasoning_content field
        setReasoningStyle(ReasoningStyle.FIELD);
        setReasoningFieldName("reasoning_content");
    }
}
