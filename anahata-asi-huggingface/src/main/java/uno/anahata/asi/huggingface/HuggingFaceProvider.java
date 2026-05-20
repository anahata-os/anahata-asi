/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.huggingface;

import com.fasterxml.jackson.databind.JsonNode;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleReasoningStyle;

/**
 * A specialized provider for the Hugging Face Inference API that performs deep
 * model inspection by fetching metadata directly from the HF Hub.
 */
@Slf4j
public class HuggingFaceProvider extends OpenAiChatCompletionsProvider {

    /**
     * The base URL for the Hugging Face Hub metadata API.
     */
    private static final String HF_HUB_BASE = "https://huggingface.co/";
    /**
     * A specialized HTTP client for fetching model configuration files from the Hub.
     * Uses a short timeout and follows redirects to the Hub's CDN.
     */
    private static final HttpClient HUB_CLIENT = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.ALWAYS)
            .connectTimeout(Duration.ofSeconds(5))
            .build();

    /**
     * Constructs a new Hugging Face provider with the stable UUID 'HuggingFace' 
     * and pre-configured endpoint and acquisition URIs.
     */
    public HuggingFaceProvider() {
        super("HuggingFace", "Hugging Face", "https://router.huggingface.co/v1",
                "HuggingFace", "https://huggingface.co/settings/tokens");
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Orchestrates a parallel "Deep Inspection" phase using the 
     * container's shared executor to fetch model metadata from the Hugging Face Hub.</p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        // 1. Get the basic IDs from the router (OpenAI-compatible)
        List<? extends AbstractModel> baseModels = super.listModels();
        if (baseModels.isEmpty()) {
            return baseModels;
        }

        log.info("Performing deep inspection on {} Hugging Face models using container executor", baseModels.size());

        // 2. Perform parallel inspection of the Hub using the shared executor
        List<CompletableFuture<HuggingFaceModel>> futures = baseModels.stream()
                .map(m -> CompletableFuture.supplyAsync(() -> inspectModel(m.getModelId()), getAsiContainer().getExecutor()))
                .collect(Collectors.toList());

        try {
            return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                    .thenApply(v -> futures.stream()
                    .map(CompletableFuture::join)
                    .filter(m -> m.getHubConfig() != null) // Only keep models we successfully inspected
                    .collect(Collectors.toList()))
                    .get(15, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("Deep inspection timed out or failed, returning basic models", e);
            return baseModels;
        }
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Adds specific handling for Hugging Face credit-related 
     * errors (HTTP 402).</p>
     */
    @Override
    public boolean isRetryable(int statusCode, String responseBody) {
        if (statusCode == 402 && responseBody != null && responseBody.contains("credits")) {
            return true;
        }
        return super.isRetryable(statusCode, responseBody);
    }

    /**
     * Performs the deep inspection of a specific model by fetching its configuration 
     * files from the Hub.
     * @param modelId The Hugging Face repo ID.
     * @return A populated HuggingFaceModel instance.
     */
    private HuggingFaceModel inspectModel(String modelId) {
        log.info("Inspecting HF model " + modelId);
        HuggingFaceModel model = new HuggingFaceModel(this, modelId, modelId);

        // Parallel fetch for the "Big Three"
        CompletableFuture<Void> configFuture = fetchHubJson(modelId, "config.json")
                .thenAccept(json -> {
                    if (json != null) {
                        log.info("Got config.json for" + modelId);
                        model.setHubConfig(json);
                        model.setMaxInputTokens(json.path("max_position_embeddings").asInt(model.getMaxInputTokens()));

                        String modelType = json.path("model_type").asText("").toLowerCase();
                        if (modelType.contains("deepseek") || modelType.contains("qwen")) {
                            model.setReasoningStyle(OpenAiCompatibleReasoningStyle.FIELD);
                            model.setReasoningFieldName("reasoning_content");
                        } else if (modelType.contains("glm") || json.path("architectures").toString().toLowerCase().contains("glm")) {
                            // GLM models are top-tier for development and support function calling natively
                            model.setSupportsFunctionCalling(true);
                            model.setReasoningStyle(OpenAiCompatibleReasoningStyle.TAGS);
                            model.setReasoningTags(List.of("<|thought|>", "<|assistant|>"));
                        }
                    } else {
                        log.warn("Could not fetch config.json for" + modelId);
                    }
                });

        CompletableFuture<Void> tokenizerFuture = fetchHubJson(modelId, "tokenizer_config.json")
                .thenAccept(json -> {
                    if (json != null) {
                        log.info("Got tokenizer_config.json for" + modelId);
                        model.setTokenizerConfig(json);
                        String chatTemplate = json.path("chat_template").asText("");
                        if (chatTemplate.contains("tools") || chatTemplate.contains("tool_calls") || chatTemplate.contains("observation")) {
                            model.setSupportsFunctionCalling(true);
                        }
                        // Detect R1 style tags in template or markers
                        if (chatTemplate.contains("<think>")) {
                            model.setReasoningStyle(OpenAiCompatibleReasoningStyle.TAGS);
                            model.setReasoningTags(List.of("<think>", "</think>"));
                        }
                    } else {
                        log.warn("Could not fetch tokenizer_config.json for" + modelId);
                    }
                });

        CompletableFuture<Void> generationFuture = fetchHubJson(modelId, "generation_config.json")
                .thenAccept(json -> {
                    if (json != null) {
                        log.info("Got generation_config.json for " + modelId);
                        model.setGenerationConfig(json);
                        if (json.has("max_new_tokens")) {
                            model.setMaxOutputTokens(json.get("max_new_tokens").asInt());
                        }
                    } else {
                        log.warn("Could not fetch generation_config.json for " + modelId);
                    }
                });

        CompletableFuture.allOf(configFuture, tokenizerFuture, generationFuture).join();
        return model;
    }

    /**
     * Helper to fetch a JSON file from the Hugging Face Hub's main branch.
     * @param modelId  The repo ID.
     * @param filename The config filename (e.g. 'config.json').
     * @return A CompletableFuture resolving to the parsed JSON or null.
     */
    private CompletableFuture<JsonNode> fetchHubJson(String modelId, String filename) {
        String url = HF_HUB_BASE + modelId + "/resolve/main/" + filename;
        log.info("Fetching: " + url);

        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(3))
                .GET();

        String apiKey = getCurrentApiKey();
        if (apiKey != null && !apiKey.isBlank()) {
            builder.header("Authorization", "Bearer " + apiKey);
        }

        return HUB_CLIENT.sendAsync(builder.build(), HttpResponse.BodyHandlers.ofString())
                .thenApply(resp -> {
                    if (resp.statusCode() == 200) {
                        try {
                            String body = resp.body();
                            log.info("Body for: " + modelId + " " + filename + ":" + body);
                            return JacksonUtils.parse(resp.body(), JsonNode.class);
                        } catch (Exception e) {
                            log.error("Exception parsing response body for " + modelId + " " + filename, e);
                        }
                    } else {
                        log.error("Body for: " + modelId + " " + filename + " " + resp.statusCode() + " " + resp.body());
                    }
                    return null;
                }).exceptionally(e -> null);
    }
}
