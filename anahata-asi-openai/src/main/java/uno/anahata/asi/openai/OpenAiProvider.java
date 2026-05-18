/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.TokenizerType;

/**
 * Clean-room provider for OpenAI, strictly utilizing the modern Responses API
 * (/v1/responses).
 * <p>
 * This provider is designed for native agentic workflows, supporting built-in
 * tools and stateful item-based history. It handles model discovery and API key
 * management for the OpenAI platform.</p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class OpenAiProvider extends AbstractAiProvider {

    /**
     * Internal Jackson mapper for OpenAI API JSON processing.
     */
    private static final ObjectMapper API_MAPPER = new ObjectMapper();
    /**
     * The HTTP client used for all communication with the OpenAI API.
     * Marked as transient to prevent serialization of the networking stack.
     */
    private transient HttpClient httpClient;

    /**
     * Lazy-initializes and returns the HTTP client for this provider.
     * <p>Ensures standard timeouts and redirect policies are applied.</p>
     * @return The active HTTP client.
     */
    public synchronized HttpClient getHttpClient() {
        if (httpClient == null) {
            httpClient = HttpClient.newBuilder()
                    .connectTimeout(Duration.ofSeconds(10))
                    .followRedirects(HttpClient.Redirect.NORMAL)
                    .build();
        }
        return httpClient;
    }

    /**
     * Constructs a new OpenAiProvider with the 'OpenAI' UUID and 
     * standard tokenizer settings.
     */
    public OpenAiProvider() {
        super("OpenAI");
        setDisplayName("OpenAI (Responses)");
        setTokenizerType(TokenizerType.CL100K_BASE);
        setKeysAcquisitionUri("https://platform.openai.com/api-keys");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Executes a GET request to the {@code /models} 
     * endpoint of the configured {@link #getBaseUrl()}. Discovered models are 
     * filtered to include only those supported by the Responses API (gpt-4o and 
     * successor generations).
     * </p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        log.info("Fetching OpenAI models...");
        try {
            String apiKey = getCurrentKey();
            if (apiKey == null) {
                return List.of();
            }

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(getBaseUrl() + "/models"))
                    .header("Authorization", "Bearer " + apiKey)
                    .GET()
                    .build();

            HttpResponse<String> response = getHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() != 200) {
                log.error("Failed to list OpenAI models: {} - {}", response.statusCode(), response.body());
                hokusPocus(); // Resilience: Rotate key if we hit an auth or quota issue during discovery
                return List.of();
            }

            JsonNode root = API_MAPPER.readTree(response.body());
            JsonNode data = root.get("data");
            List<OpenAiModel> models = new ArrayList<>();

            if (data != null && data.isArray()) {
                for (JsonNode modelNode : data) {
                    String id = modelNode.get("id").asText();
                    // Responses API is supported by gpt-4o and future models.
                    if (id.startsWith("gpt-4") || id.startsWith("gpt-5") || id.startsWith("gpt-6")) {
                        models.add(new OpenAiModel(this, modelNode));
                    }
                }
            }
            return models;
        } catch (Exception e) {
            log.error("Error listing OpenAI models", e);
            hokusPocus();
            return List.of();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Determines if an HTTP status code or response body indicates a transient 
     * failure that should trigger a retry or rotation.
     * </p>
     */
    public boolean isRetryable(int statusCode, String responseBody) {
        return statusCode == 429 || statusCode == 503 || statusCode == 500 || statusCode == 499 || statusCode == 408;
    }

    /**
     * {@inheritDoc}
     * <p>Provides a template for the OpenAI {@code api_keys.txt} file.</p>
     */
    @Override
    public String getApiKeyHint() {
        return "# OpenAI API Key Configuration\n"
                + "# -----------------------------\n"
                + "# Add one key per line.\n"
                + "sk-proj-...\n";
    }
}
