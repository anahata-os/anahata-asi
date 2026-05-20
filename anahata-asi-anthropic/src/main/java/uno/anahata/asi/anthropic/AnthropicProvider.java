/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Provider implementation for the official Anthropic API.
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class AnthropicProvider extends AbstractAiProvider {

    /**
     * The forced Anthropic API version header (e.g., '2023-06-01').
     */
    private String anthropicVersion;
    /**
     * The HTTP client used for communication. transient to avoid serialization.
     */
    private transient HttpClient httpClient;

    public AnthropicProvider() {
        this("Anthropic", "Anthropic", "https://api.anthropic.com/v1", "2023-06-01", "https://console.anthropic.com/settings/keys");
    }

    public AnthropicProvider(String uuid, String displayName, String baseUrl, String anthropicVersion, String keysAcquisitionUri) {
        super(uuid);
        setDisplayName(displayName);
        setBaseUrl(baseUrl);
        setDescription("Anthropic Claude API client supporting Claude, Sonnet and Mythos.");
        this.anthropicVersion = anthropicVersion;
        setTokenizerType(TokenizerType.CL100K_BASE); // Change when Claude tokenizer is available
        setKeysAcquisitionUri(keysAcquisitionUri);
    }

    /**
     * Lazily initializes and returns the HTTP client.
     * @return The active HTTP client.
     */
    public synchronized HttpClient getHttpClient() {
        if (httpClient == null) {
            httpClient = HttpClient.newBuilder()
                    .connectTimeout(Duration.ofSeconds(15))
                    .build();
        }
        return httpClient;
    }

    /**
     * Creates a pre-configured request builder with standard Anthropic headers.
     * @param endpoint The API endpoint (e.g., 'messages').
     * @return A new HttpRequest.Builder.
     */
    public HttpRequest.Builder createRequestBuilder(String endpoint) {
        String url = getBaseUrl();
        String fullUrl = url.endsWith("/") ? url + endpoint : url + "/" + endpoint;
        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(fullUrl))
                .header("content-type", "application/json")
                .timeout(Duration.ofSeconds(120));
                
        String apiKey = getCurrentApiKey();
        if (apiKey != null && !apiKey.isBlank()) {
            builder.header("x-api-key", apiKey);
        }
        if (anthropicVersion != null && !anthropicVersion.isBlank()) {
            builder.header("anthropic-version", anthropicVersion);
        }
        return builder;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Fetches available models from the Anthropic-compatible
     * endpoint. Note that official Anthropic models are typically static, but this
     * discovery allows for dynamic compatibility with Anthropic-compatible proxies
     * (like MiniMax or local routers).
     * </p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        log.info("Fetching Anthropic-compatible models from {}", getBaseUrl());
        try {
            String apiKey = getCurrentApiKey();
            if (apiKey != null) {
                HttpRequest request = createRequestBuilder("models").GET().build();
                java.net.http.HttpResponse<String> response = getHttpClient().send(request, java.net.http.HttpResponse.BodyHandlers.ofString());

                if (response.statusCode() == 200) {
                    com.fasterxml.jackson.databind.JsonNode root = JacksonUtils.parse(response.body(), com.fasterxml.jackson.databind.JsonNode.class);
                    com.fasterxml.jackson.databind.JsonNode data = root.get("data");
                    if (data != null && data.isArray()) {
                        List<AnthropicModel> models = new ArrayList<>();
                        for (com.fasterxml.jackson.databind.JsonNode modelNode : data) {
                            String id = modelNode.path("id").asText();
                            String name = modelNode.path("display_name").asText(id);
                            String version = modelNode.path("created_at").asText("");
                            models.add(new AnthropicModel(this, id, name, version));
                        }
                        return models;
                    }
                } else {
                    log.error("Failed to list models: {} - {}", response.statusCode(), response.body());
                }
            }
        } catch (Exception e) {
            log.error("Error listing Anthropic models", e);
        }
        
        
        
        return List.of();
    }

    @Override
    public String getApiKeyHint() {
        return "# Anthropic API Key Configuration\n"
                + "sk-ant-...\n";
    }
    
    /**
     * Determines if an HTTP status code represents a retryable error.
     * @param statusCode The HTTP status code.
     * @param responseBody The response body.
     * @return true if the request should be retried.
     */
    public boolean isRetryable(int statusCode, String responseBody) {
        return statusCode == 429 || statusCode == 529 || statusCode == 503 || statusCode == 500 || statusCode == 499 || statusCode == 408;
    }
    
    @Override
    public synchronized void hokusPocus() {
        super.hokusPocus();
    }
}