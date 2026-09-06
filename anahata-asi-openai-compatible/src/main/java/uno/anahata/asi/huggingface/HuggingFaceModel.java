/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.huggingface;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;

/**
 * A model instance that carries additional metadata discovered from the Hugging Face Hub.
 */
@Getter
@Setter
public class HuggingFaceModel extends OpenAiCompatibleModel {

    /**
     * The raw JSON from the model's 'config.json', containing architectural 
     * details and token limits.
     */
    private JsonNode hubConfig;
    /**
     * The raw JSON from the model's 'tokenizer_config.json', used to 
     * discover chat templates and tool support markers.
     */
    private JsonNode tokenizerConfig;
    /**
     * The raw JSON from the model's 'generation_config.json', used to 
     * extract default sampling parameters like temperature and top-p.
     */
    private JsonNode generationConfig;
    
    /**
     * Flag derived from the chat template or config indicating if the model 
     * supports tool calling.
     */
    private boolean supportsFunctionCalling = false;

    /**
     * Constructs a new Hugging Face model instance.
     * @param provider    The owning HF provider.
     * @param modelId     The full repo ID.
     * @param displayName The display name.
     */
    public HuggingFaceModel(HuggingFaceProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Returns the value discovered during the Hub 
     * inspection phase.</p>
     */
    @Override
    public boolean isSupportsFunctionCalling() {
        return supportsFunctionCalling;
    }

    /**
     * Builds a rich HTML view combining architecture, tokenizer class, and deep-inspected sampling parameters.
     *
     * @return The formatted HTML summary string.
     */
    public String buildHubHtmlDescription() {
        StringBuilder sb = new StringBuilder("<html>");
        sb.append("<b>Hugging Face Model:</b> ").append(getModelId()).append("<br>");
        if (hubConfig != null) {
            sb.append("<b>Architecture:</b> ").append(hubConfig.path("model_type").asText("unknown")).append("<br>");
            if (hubConfig.has("architectures") && hubConfig.get("architectures").isArray()) {
                 sb.append("<b>Class:</b> ").append(hubConfig.get("architectures").get(0).asText()).append("<br>");
            }
            sb.append("<b>DType:</b> ").append(hubConfig.path("torch_dtype").asText("unknown")).append("<br>");
            sb.append("<b>Context Window:</b> ").append(getMaxInputTokens()).append(" tokens<br>");
        }
        if (tokenizerConfig != null) {
            sb.append("<b>Tokenizer:</b> ").append(tokenizerConfig.path("tokenizer_class").asText("standard")).append("<br>");
        }
        sb.append("<b>Function Calling:</b> ").append(isSupportsFunctionCalling()).append("<br>");
        sb.append("<b>Reasoning Style:</b> ").append(getReasoningStyle()).append("<br>");
        
        if (generationConfig != null) {
            sb.append("<hr><b>Default Sampling:</b><br>");
            sb.append("&nbsp;&nbsp;Temperature: ").append(getDefaultTemperature()).append("<br>");
            sb.append("&nbsp;&nbsp;Top P: ").append(getDefaultTopP()).append("<br>");
            if (generationConfig.has("max_new_tokens")) {
                sb.append("&nbsp;&nbsp;Max Output Tokens: ").append(getMaxOutputTokens()).append("<br>");
            }
        }
        
        if (hubConfig != null && hubConfig.has("transformers_version")) {
            sb.append("<hr><font size='-2'>Built with Transformers ").append(hubConfig.get("transformers_version").asText()).append("</font>");
        }
        
        sb.append("</html>");
        return sb.toString();
    }
}
