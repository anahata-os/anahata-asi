/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.huggingface;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.ResponseModality;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleReasoningStyle;

/**
 * A specialized model instance that encapsulates its own metadata discovery
 * from the Hugging Face Hub.
 * <p>
 * Fetches and parses architectural specifications ({@code config.json}), chat
 * templates and tokenizer settings ({@code tokenizer_config.json}), and
 * sampling parameters ({@code generation_config.json}) in parallel, enriching
 * generation requests with explicit {@code max_tokens} limits.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class HuggingFaceModel extends OpenAiCompatibleModel {

    /**
     * The raw JSON from the model's 'config.json', containing architectural
     * details and token limits.
     */
    private JsonNode hubConfig;

    /**
     * The raw JSON from the model's 'tokenizer_config.json', used to discover
     * chat templates and tool support markers.
     */
    private JsonNode tokenizerConfig;

    /**
     * The raw JSON from the model's 'generation_config.json', used to extract
     * default sampling parameters like temperature and top-p.
     */
    private JsonNode generationConfig;

    /**
     * Constructs a new Hugging Face model instance.
     *
     * @param provider The owning HF provider.
     * @param modelId The full repo ID.
     * @param displayName The display name.
     */
    public HuggingFaceModel(HuggingFaceProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
        setSupportsFunctionCalling(false);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the typed parent {@link HuggingFaceProvider} owning this model.
     * </p>
     */
    @Override
    public HuggingFaceProvider getProvider() {
        return (HuggingFaceProvider) provider;
    }

    /**
     * Performs deep inspection of this model by fetching its configuration files
     * from the Hugging Face Hub sequentially in a single thread, using a
     * pre-resolved API key.
     *
     * @param apiKey The pre-resolved API key to avoid lock contention with the
     * provider.
     */
    public void inspect(String apiKey) {
        log.info("Inspecting Hugging Face model {}", getModelId());

        // 1. config.json
        JsonNode config = fetchHubJson("config.json", apiKey);
        if (config != null) {
            log.info("Got config.json for {}", getModelId());
            this.hubConfig = config;
            if (config.has("max_position_embeddings")) {
                setMaxInputTokens(config.get("max_position_embeddings").asInt());
            } else if (config.has("max_sequence_length")) {
                setMaxInputTokens(config.get("max_sequence_length").asInt());
            } else if (config.has("seq_length")) {
                setMaxInputTokens(config.get("seq_length").asInt());
            } else if (config.has("context_length")) {
                setMaxInputTokens(config.get("context_length").asInt());
            }

            if (config.has("version")) {
                setVersion(config.get("version").asText());
            }
            if (config.has("model_type")) {
                setDescription("[" + config.get("model_type").asText() + "]");
            }

            // Response modalities: Hugging Face router chat/completions endpoints generate text responses
            setSupportedResponseModalities(new ArrayList<>(List.of(ResponseModality.TEXT)));
        } else {
            log.warn("Could not fetch config.json for {}", getModelId());
        }

        // 2. tokenizer_config.json
        JsonNode tokConfig = fetchHubJson("tokenizer_config.json", apiKey);
        if (tokConfig != null) {
            log.info("Got tokenizer_config.json for {}", getModelId());
            this.tokenizerConfig = tokConfig;
            String chatTemplate = tokConfig.path("chat_template").asText("");
            if (!chatTemplate.isBlank()) {
                if (chatTemplate.contains("tools") || chatTemplate.contains("tool_calls")
                        || chatTemplate.contains("<tool_call>") || chatTemplate.contains("[TOOL_CALLS]")
                        || chatTemplate.contains("[AVAILABLE_TOOLS]") || chatTemplate.contains("observation")) {
                    setSupportsFunctionCalling(true);
                }
                if (chatTemplate.contains("<think>")) {
                    setReasoningStyle(OpenAiCompatibleReasoningStyle.TAGS);
                    setReasoningTags(List.of("<think>", "</think>"));
                } else if (chatTemplate.contains("<|thought|>")) {
                    setReasoningStyle(OpenAiCompatibleReasoningStyle.TAGS);
                    setReasoningTags(List.of("<|thought|>", "<|assistant|>"));
                }
            }
            if (getMaxInputTokens() == null && tokConfig.has("model_max_length")) {
                int mml = tokConfig.get("model_max_length").asInt();
                if (mml > 0 && mml < 10_000_000) {
                    setMaxInputTokens(mml);
                }
            }
        } else {
            log.warn("Could not fetch tokenizer_config.json for {}", getModelId());
        }

        // 3. generation_config.json
        JsonNode genConfig = fetchHubJson("generation_config.json", apiKey);
        if (genConfig != null) {
            log.info("Got generation_config.json for {}", getModelId());
            this.generationConfig = genConfig;
            // Note: max_new_tokens in generation_config is an author demo default, not a model ceiling.
            // We leave maxOutputTokens unconstrained so generation can fill available context.
            if (genConfig.has("temperature")) {
                setDefaultTemperature((float) genConfig.get("temperature").asDouble());
            }
            if (genConfig.has("top_k")) {
                setDefaultTopK(genConfig.get("top_k").asInt());
            }
            if (genConfig.has("top_p")) {
                setDefaultTopP((float) genConfig.get("top_p").asDouble());
            }
        } else {
            log.warn("Could not fetch generation_config.json for {}", getModelId());
        }

        setRawDescription(buildHubHtmlDescription());
        setSupportedActions(List.of("chat/completions"));
    }

    /**
     * Helper to fetch a JSON file from the Hugging Face Hub's main branch.
     *
     * @param filename The config filename (e.g. 'config.json').
     * @param apiKey The pre-resolved Bearer token.
     * @return The parsed JSON or null if not found/error.
     */
    private JsonNode fetchHubJson(String filename, String apiKey) {
        String url = HuggingFaceProvider.HF_HUB_BASE + getModelId() + "/resolve/main/" + filename;
        log.info("Fetching Hub metadata: {}", url);

        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(15))
                .GET();

        if (apiKey != null && !apiKey.isBlank()) {
            builder.header("Authorization", "Bearer " + apiKey);
        } else {
            log.warn("fetching hf json files without api key.... could this lead to hugging face denying the request due to too much parallelism?");
        }

        try {
            HttpResponse<String> resp = HuggingFaceProvider.HUB_CLIENT.send(builder.build(), HttpResponse.BodyHandlers.ofString());
            if (resp.statusCode() == 200) {
                return JacksonUtils.parse(resp.body(), JsonNode.class);
            } else {
                log.debug("HTTP {} fetching {} for {}", resp.statusCode(), filename, getModelId());
            }
        } catch (Exception e) {
            log.warn("Exception fetching Hub metadata for {} {}", getModelId(), filename, e);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Ensures that {@code max_tokens} is populated in
     * the request payload so Hugging Face router does not truncate generations
     * at its low server default.
     * </p>
     */
    @Override
    protected void enrichPayload(ObjectNode payload, GenerationRequest request) {
        super.enrichPayload(payload, request);

        if (!payload.has("max_tokens")) {
            if (getMaxInputTokens() != null && getMaxInputTokens() > 0) {
                int roughPromptTokens = TokenizerUtils.countTokens(payload.toString(), getTokenizerType());
                // 1% tokenizer discrepancy forgiveness + 64 tokens safety buffer to prevent router context overflow
                int safePromptBudget = (int) Math.ceil(roughPromptTokens * 1.01d) + 64;
                int maxOut = Math.max(1, getMaxInputTokens() - safePromptBudget);
                // Cap single generation at 128K tokens (131,072) to prevent serverless router rejects on 1M+ context models
                maxOut = Math.min(maxOut, 131072);
                payload.put("max_tokens", maxOut);
                log.info("Worked out dynamic max_tokens: {} (context: {}, safe prompt budget: {}) for model {}",
                        maxOut, getMaxInputTokens(), safePromptBudget, getModelId());
            } else {
                payload.put("max_tokens", 131072);
                log.info("Defaulting max_tokens to 128K as model does not report max input tokens for {}", getModelId());
            }
        }
    }

    /**
     * Builds a rich HTML view combining architecture, tokenizer class, and
     * deep-inspected sampling parameters.
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
        sb.append("<b>Modalities:</b> ").append(getSupportedResponseModalities().stream().map(Enum::name).collect(java.util.stream.Collectors.joining(", "))).append("<br>");

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
