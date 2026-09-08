/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.huggingface;

import java.net.http.HttpClient;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;

/**
 * A specialized provider for the Hugging Face Inference API that performs deep
 * model inspection by fetching metadata directly from the HF Hub.
 *
 * @author anahata
 */
@Slf4j
public class HuggingFaceProvider extends OpenAiChatCompletionsProvider {

    /**
     * The base URL for the Hugging Face Hub metadata API.
     */
    public static final String HF_HUB_BASE = "https://huggingface.co/";

    /**
     * A specialized HTTP client for fetching model configuration files from the Hub.
     * Uses connection pooling and follows redirects to the Hub's CDN.
     */
    public static final HttpClient HUB_CLIENT = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.ALWAYS)
            .connectTimeout(Duration.ofSeconds(15))
            .build();

    /**
     * Constructs a new Hugging Face provider with the stable UUID 'HuggingFace' 
     * and pre-configured endpoint and acquisition URIs.
     */
    public HuggingFaceProvider() {
        super("HuggingFace", "Hugging Face", "https://router.huggingface.co/v1", "https://huggingface.co/settings/tokens");
        setDescription("Hugging Face Serverless Inference API client for open-source models.");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Fetches basic models from the router and orchestrates
     * parallel deep self-inspection across all models without artificial timeouts.
     * Resolves the API key upfront to prevent thread-monitor lock contention.
     * </p>
     */
    @Override
    @SneakyThrows
    public List<? extends AbstractModel> listModels() {
        List<? extends AbstractModel> baseModels = super.listModels();
        if (baseModels.isEmpty()) {
            return baseModels;
        }

        String apiKey = getCurrentKey();
        log.info("Performing parallel deep inspection on {} Hugging Face models", baseModels.size());

        List<HuggingFaceModel> hfModels = baseModels.stream()
                .map(m -> new HuggingFaceModel(this, m.getModelId(), m.getModelId()))
                .collect(Collectors.toList());

        List<CompletableFuture<Void>> futures = hfModels.stream()
                .map(m -> CompletableFuture.runAsync(() -> m.inspectAsync(apiKey).join(), getAsiContainer().getExecutor()))
                .collect(Collectors.toList());

        // Wait for all model inspections to complete
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

        log.info("Deep inspection completed for {} Hugging Face models", hfModels.size());
        return hfModels;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Adds specific handling for Hugging Face credit-related 
     * errors (HTTP 402).
     * </p>
     */
    @Override
    public boolean isRetryable(int statusCode, String responseBody) {
        if (statusCode == 402 && responseBody != null && responseBody.contains("credits")) {
            return true;
        }
        return super.isRetryable(statusCode, responseBody);
    }
}
