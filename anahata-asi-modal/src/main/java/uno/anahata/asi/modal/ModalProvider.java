/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.modal;

import com.fasterxml.jackson.databind.JsonNode;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleProvider;

/**
 * A pre-configured provider for Modal's GLM-5 inference endpoint.
 * 
 * <p>Modal provides high-performance inference for GLM-5 models with native
 * reasoning support. The API returns reasoning content in a dedicated
 * {@code reasoning_content} field, which is automatically detected by the
 * {@link uno.anahata.asi.openai.OpenAiModel} autodetection logic.</p>
 * 
 * <p><b>Key Features:</b></p>
 * <ul>
 *   <li>GLM-5 model with native function calling support</li>
 *   <li>Reasoning content exposed via {@code reasoning_content} field</li>
 *   <li>OpenAI-compatible API endpoint</li>
 * </ul>
 * 
 * <p><b>API Key Acquisition:</b></p>
 * Get your API key at: <a href="https://modal.com/glm-5-endpoint">https://modal.com/glm-5-endpoint</a>
 * 
 * @author anahata
 */
public class ModalProvider extends OpenAiCompatibleProvider {

    /**
     * Constructs a new Modal provider with pre-configured defaults.
     * 
     * <p>Sets the stable UUID to "Modal" for consistent session persistence
     * across restarts.</p>
     */
    public ModalProvider() {
        super(
            "Modal",
            "Modal - GLM-5",
            "https://api.us-west-2.modal.direct/v1",
            "Modal",
            "https://modal.com/glm-5-endpoint"
        );
    }

    @Override
    protected OpenAiCompatibleModel createModel(JsonNode node) {
        return new ModalModel(this, node);
    }
}
