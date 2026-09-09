/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openrouter;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.ResponseModality;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleReasoningStyle;

/**
 * Specialized model implementation for OpenRouter (openrouter.ai).
 * <p>
 * Parses rich architectural metadata from OpenRouter's {@code /models} endpoint,
 * including context lengths, provider-specific max completion ceilings, output
 * response modalities, function calling support, pricing, and tokenizer types.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class OpenRouterModel extends OpenAiCompatibleModel {

    /**
     * Constructs a new OpenRouter model instance from a raw JSON discovery node.
     * 
     * @param provider The owning OpenRouter provider.
     * @param node The raw JSON model descriptor returned by OpenRouter's /models endpoint.
     */
    public OpenRouterModel(OpenRouterAiProvider provider, JsonNode node) {
        super(provider, node);

        // 1. Display Name
        String name = node.path("name").asText(null);
        if (name != null && !name.isBlank()) {
            setDisplayName(name.trim());
        }

        // 2. Context Window (max input tokens)
        int ctx = node.path("context_length").asInt(0);
        if (ctx <= 0) {
            ctx = node.path("top_provider").path("context_length").asInt(0);
        }
        if (ctx > 0) {
            setMaxInputTokens(ctx);
        }

        // 3. Provider Output Ceiling (max completion tokens)
        int maxOut = node.path("top_provider").path("max_completion_tokens").asInt(0);
        if (maxOut <= 0) {
            maxOut = node.path("max_completion_tokens").asInt(0);
        }
        if (maxOut > 0) {
            setMaxOutputTokens(maxOut);
        }

        // 4. Output Modalities (Response Modalities)
        JsonNode outMods = node.path("architecture").path("output_modalities");
        if (outMods.isArray() && outMods.size() > 0) {
            List<ResponseModality> modalities = new ArrayList<>();
            for (JsonNode m : outMods) {
                String mStr = m.asText("").toLowerCase();
                if (mStr.contains("text")) {
                    modalities.add(ResponseModality.TEXT);
                } else if (mStr.contains("image")) {
                    modalities.add(ResponseModality.IMAGE);
                } else if (mStr.contains("audio")) {
                    modalities.add(ResponseModality.AUDIO);
                } else if (mStr.contains("video")) {
                    modalities.add(ResponseModality.VIDEO);
                }
            }
            if (!modalities.isEmpty()) {
                setSupportedResponseModalities(modalities);
            }
        }

        // 5. Native Function Calling (Tools) Support
        JsonNode params = node.path("supported_parameters");
        if (params.isArray()) {
            boolean tools = false;
            for (JsonNode p : params) {
                if ("tools".equalsIgnoreCase(p.asText(""))) {
                    tools = true;
                    break;
                }
            }
            setSupportsFunctionCalling(tools);
        }

        // 6. Tokenizer Detection
        String tok = node.path("architecture").path("tokenizer").asText("");
        if ("Claude".equalsIgnoreCase(tok)) {
            setTokenizerType(TokenizerType.CL100K_BASE);
        } else if ("GPT".equalsIgnoreCase(tok) || "OpenAI".equalsIgnoreCase(tok)) {
            setTokenizerType(TokenizerType.O200K_BASE);
        } else if ("Gemini".equalsIgnoreCase(tok)) {
            setTokenizerType(TokenizerType.GEMINI);
        }

        // 7. Reasoning
        JsonNode reasoning = node.path("reasoning");
        if (reasoning.isObject()) {
            setReasoningStyle(OpenAiCompatibleReasoningStyle.FIELD);
            setReasoningFieldName("reasoning");
        }

        // 8. Default Sampling
        JsonNode defParams = node.path("default_parameters");
        if (defParams.isObject()) {
            if (defParams.has("temperature")) {
                setDefaultTemperature((float) defParams.get("temperature").asDouble());
            }
            if (defParams.has("top_p")) {
                setDefaultTopP((float) defParams.get("top_p").asDouble());
            }
            if (defParams.has("top_k")) {
                setDefaultTopK(defParams.get("top_k").asInt());
            }
        }

        // 9. Description & Pricing Smuggle
        String baseDesc = node.path("description").asText("").trim();
        JsonNode pricing = node.path("pricing");
        String pricingTag = "";
        if (pricing.isObject()) {
            double promptPrice = pricing.path("prompt").asDouble(0) * 1_000_000.0;
            double completionPrice = pricing.path("completion").asDouble(0) * 1_000_000.0;
            if (promptPrice > 0 || completionPrice > 0) {
                pricingTag = String.format(" [In: $%.2f/M | Out: $%.2f/M]", promptPrice, completionPrice);
            } else {
                pricingTag = " [Free]";
            }
        }
        setDescription(baseDesc + pricingTag);
        setRawDescription(node.toPrettyString());
    }

    /**
     * Constructs an OpenRouter model with explicit identifier and display name.
     * 
     * @param provider The owning OpenRouter provider.
     * @param modelId The unique model ID.
     * @param displayName The human-readable display name.
     */
    public OpenRouterModel(OpenRouterAiProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the typed parent {@link OpenRouterAiProvider} instance.
     * </p>
     * 
     * @return The OpenRouter provider instance.
     */
    @Override
    public OpenRouterAiProvider getProvider() {
        return (OpenRouterAiProvider) provider;
    }
}
