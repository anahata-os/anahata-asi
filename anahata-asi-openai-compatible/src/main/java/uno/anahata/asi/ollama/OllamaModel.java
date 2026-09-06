/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ollama;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleReasoningStyle;

/**
 * A specialized model implementation for Ollama that parses parameter sizes,
 * quantization formats, true context window limits, and capability tags from Ollama's native API.
 * <p>
 * Captures 100% of the raw JSON metadata in {@link #getRawDescription()} without deceptive fallback defaults.
 * Automatically injects the model's reported context length via {@code options.num_ctx} so that Ollama
 * does not truncate requests to its default 2,048 token window.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class OllamaModel extends OpenAiCompatibleModel {

    /**
     * Parameter scale string (e.g., "9.0B", "35B"), or null if unspecified by the server.
     */
    private String parameterSize;

    /**
     * GGUF quantization format (e.g., "Q4_K_M", "IQ4_XS"), or null if unspecified by the server.
     */
    private String quantizationLevel;

    /**
     * The model architecture family (e.g., "qwen2", "llama"), or null if unspecified by the server.
     */
    private String family;

    /**
     * Constructs a new Ollama model instance from a JSON node returned by {@code /api/tags} or {@code /v1/models}.
     * 
     * @param provider the parent OllamaAiProvider instance
     * @param node the JSON node containing raw model metadata
     */
    public OllamaModel(OllamaAiProvider provider, JsonNode node) {
        super(provider, node);

        // Capture 100% of the raw JSON without truncating or synthetic defaults
        this.rawDescription = node.toPrettyString();

        JsonNode details = node.path("details");
        if (details.isObject()) {
            this.parameterSize = details.hasNonNull("parameter_size") ? details.get("parameter_size").asText() : null;
            this.quantizationLevel = details.hasNonNull("quantization_level") ? details.get("quantization_level").asText() : null;
            this.family = details.hasNonNull("family") ? details.get("family").asText() : null;

            if (details.hasNonNull("context_length")) {
                setMaxInputTokens(details.get("context_length").asInt());
            }
        }

        JsonNode caps = node.path("capabilities");
        if (caps.isArray()) {
            boolean tools = false;
            boolean thinking = false;
            for (JsonNode cap : caps) {
                String text = cap.asText("");
                if ("tools".equalsIgnoreCase(text)) {
                    tools = true;
                } else if ("thinking".equalsIgnoreCase(text)) {
                    thinking = true;
                }
            }
            setSupportsFunctionCalling(tools);
            if (thinking) {
                setReasoningStyle(OpenAiCompatibleReasoningStyle.FIELD);
                setReasoningFieldName("reasoning");
            }
        }

        // Build a rich display name if specs are available
        String id = getModelId();
        if (parameterSize != null && quantizationLevel != null) {
            setDisplayName(id + " (" + parameterSize + " " + quantizationLevel + ")");
        } else if (parameterSize != null) {
            setDisplayName(id + " (" + parameterSize + ")");
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Ensures Ollama utilizes the model's reported context limit by injecting {@code options: {"num_ctx": ...}}.
     * If the model reports a context limit (e.g. 262,144 tokens), it allocates that limit bounded by the user's
     * session token threshold. If the model did not report a context limit, it omits {@code num_ctx} to allow
     * the server's default configuration to apply.
     * </p>
     * 
     * @param payload the JSON payload being constructed for the API request
     * @param request the generation request containing active configuration
     */
    @Override
    protected void enrichPayload(ObjectNode payload, GenerationRequest request) {
        super.enrichPayload(payload, request);

        ObjectNode options = payload.has("options") && payload.get("options").isObject()
                ? (ObjectNode) payload.get("options")
                : payload.putObject("options");

        if (!options.has("num_ctx") && getMaxInputTokens() != null && getMaxInputTokens() > 0) {
            int threshold = request.config().getAgi().getConfig().getTokenThreshold();
            int numCtx = (threshold > 0 && threshold < getMaxInputTokens()) ? threshold : getMaxInputTokens();
            options.put("num_ctx", numCtx);
        }
    }
}
