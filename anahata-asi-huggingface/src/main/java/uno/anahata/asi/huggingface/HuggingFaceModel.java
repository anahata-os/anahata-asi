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

    private JsonNode hubConfig;
    private JsonNode tokenizerConfig;
    private JsonNode generationConfig;
    
    private boolean supportsFunctionCalling = false;

    public HuggingFaceModel(HuggingFaceProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
    }

    @Override
    public boolean isSupportsFunctionCalling() {
        return supportsFunctionCalling;
    }

    @Override
    public int getMaxInputTokens() {
        if (hubConfig != null && hubConfig.has("max_position_embeddings")) {
            return hubConfig.get("max_position_embeddings").asInt();
        }
        return super.getMaxInputTokens();
    }

    @Override
    public int getMaxOutputTokens() {
        if (generationConfig != null && generationConfig.has("max_new_tokens")) {
            return generationConfig.get("max_new_tokens").asInt();
        }
        return super.getMaxOutputTokens();
    }

    @Override
    public Float getDefaultTemperature() {
        if (generationConfig != null && generationConfig.has("temperature")) {
            return (float) generationConfig.get("temperature").asDouble();
        }
        return super.getDefaultTemperature();
    }

    @Override
    public Integer getDefaultTopK() {
        if (generationConfig != null && generationConfig.has("top_k")) {
            return generationConfig.get("top_k").asInt();
        }
        return super.getDefaultTopK();
    }

    @Override
    public Float getDefaultTopP() {
        if (generationConfig != null && generationConfig.has("top_p")) {
            return (float) generationConfig.get("top_p").asDouble();
        }
        return super.getDefaultTopP();
    }

    @Override
    public String getVersion() {
        if (hubConfig != null && hubConfig.has("version")) {
            return hubConfig.get("version").asText();
        }
        return "";
    }

    @Override
    public String getDescription() {
        if (hubConfig != null && hubConfig.has("model_type")) {
            return "[" + hubConfig.get("model_type").asText() + "] ";
        }
        return super.getDescription();
    }
    
    @Override
    public String getRawDescription() {
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
