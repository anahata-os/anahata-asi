/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import com.fasterxml.jackson.databind.JsonNode;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import uno.anahata.asi.internal.JacksonUtils;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import lombok.Setter;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.TokenizerType;

/**
 * A universal AI provider for any API endpoint compatible with the OpenAI Chat
 * Completion specification.
 * <p>
 * This provider allows the user to configure a custom {@code baseUrl}, enabling
 * seamless integration with services like Groq, DeepSeek, or local inference
 * servers like Ollama and vLLM.
 * </p>
 *
 * @author anahata
 */
@Getter
@Setter
@Slf4j
public class OpenAiChatCompletionsProvider extends AbstractAiProvider {

    /**
     * Optional custom HTTP headers to be sent with every request. Essential for
     * providers requiring specific vendor headers (e.g., 'X-Title',
     * 'OpenAI-Organization').
     */
    private Map<String, String> customHeaders = new HashMap<>();

    /**
     * Whether to prefer HTTP/1.1 over HTTP/2. Essential for some local
     * inference servers and home routers with port forwarding.
     */
    private boolean preferHttp11 = true;

    /**
     * Constructs a default OpenAI-compatible provider.
     * <p>
     * Initializes the display name to 'Universal' and sets the default
     * tokenizer to {@link TokenizerType#CL100K_BASE}. Required for Kryo
     * deserialization.
     * </p>
     */
    public OpenAiChatCompletionsProvider() {
        super();
        setDisplayName("OpenAI Compatible (Universal)");
        setTokenizerType(TokenizerType.CL100K_BASE);
        setKeysAcquisitionUri("https://platform.openai.com/api-keys");
        this.customHeaders = new HashMap<>();
    }

    /**
     * Constructs a new universal provider with basic configuration.
     * @param uuid The unique provider ID.
     * @param displayName The user-facing name.
     * @param baseUrl The API endpoint URL.
     */
    public OpenAiChatCompletionsProvider(String uuid, String displayName, String baseUrl) {
        this(uuid, displayName, baseUrl, null, null);
    }

    /**
     * Constructs a new universal provider with a custom storage folder and acquisition URI.
     * @param uuid The unique provider ID.
     * @param displayName The user-facing name.
     * @param baseUrl The API endpoint URL.
     * @param folderName The custom folder name for configuration and key storage.
     * @param apiKeyAdquisitionUri The URI where users can obtain API keys for this provider.
     */
    public OpenAiChatCompletionsProvider(String uuid, String displayName, String baseUrl, String folderName, String apiKeyAdquisitionUri) {
        super(uuid);
        setDisplayName(displayName);
        setBaseUrl(baseUrl);
        setKeysAcquisitionUri(apiKeyAdquisitionUri);
        setFolderName(folderName);
        setTokenizerType(TokenizerType.CL100K_BASE);
    }

    /**
     * Creates a pre-configured HttpClient based on the provider's protocol
     * preferences.
     *
     * @return A new HttpClient instance.
     */
    public HttpClient createHttpClient() {
        HttpClient.Builder builder = HttpClient.newBuilder()
                .followRedirects(HttpClient.Redirect.ALWAYS)
                .connectTimeout(Duration.ofSeconds(15));
        if (preferHttp11) {
            builder.version(HttpClient.Version.HTTP_1_1);
        }
        return builder.build();
    }

    /**
     * Creates a pre-configured HttpRequest.Builder with Authorization and
     * custom headers.
     *
     * @param endpoint The relative endpoint path (e.g., "models" or
     * "chat/completions").
     * @return A new HttpRequest.Builder.
     */
    public HttpRequest.Builder createRequestBuilder(String endpoint) {
        String url = getBaseUrl();
        if (url == null) {
            throw new IllegalStateException("Base URL not set for provider: " + getDisplayName());
        }

        String fullUrl = url.endsWith("/") ? url + endpoint : url + "/" + endpoint;
        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(fullUrl))
                .timeout(Duration.ofSeconds(120));

        if (super.isApiKeyRequired()) {
            String apiKey = getCurrentApiKey();
            if (apiKey != null && !apiKey.isBlank()) {
                builder.header("Authorization", "Bearer " + apiKey);
            } else {
                log.warn("Provider {} says api key is required but no api key was found, attempting anyways without Authorization header", getDisplayName());
            }
        }

        getCustomHeaders().forEach(builder::header);
        return builder;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Retrieves the next available API key from the
     * provider's key rotation pool.
     * </p>
     */
    @Override
    public String getCurrentApiKey() {
        return getNextKey();
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Checks for common OpenAI-compatible error codes 
     * (429, 503, 500, 499, 408).</p>
     */
    public boolean isRetryable(int statusCode, String responseBody) {
        return statusCode == 429 || statusCode == 503 || statusCode == 500 || statusCode == 499 || statusCode == 408;
    }

    @Override
    public void hokusPocus() {
        super.hokusPocus(); 
        getNextKey();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs a GET request to the {@code /models}
     * endpoint of the configured {@link #getBaseUrl()}. Parses the standard OpenAI
     * list-response and wraps each ID in an {@link OpenAiCompatibleModel} instance.
     * </p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        log.info("Listing models for {} {}", getDisplayName(), getBaseUrl());
        try {
            HttpRequest httpRequest = createRequestBuilder("models").header("Accept", "application/json").GET().build();
            try (HttpClient client = createHttpClient()) {
                HttpResponse<String> httpResponse = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());
                log.info("Got response from /models endpoint: " + httpResponse);
                if (httpResponse.statusCode() != 200) {
                    log.error("Failed to fetch models from {}. Status: {}. Response: {}", httpRequest.uri(), httpResponse.statusCode(), httpResponse.body());
                    return Collections.emptyList();
                }
                JsonNode root = JacksonUtils.parse(httpResponse.body(), JsonNode.class);
                JsonNode data = root.get("data");
                if (data != null && data.isArray()) {
                    List<OpenAiCompatibleModel> models = new ArrayList<>();
                    for (JsonNode modelNode : data) {
                        String id = modelNode.path("id").asText();
                        if (!id.isBlank()) {
                            models.add(createModel(modelNode));
                        }
}
                    log.info("Successfully discovered {} models from {}", models.size(), getBaseUrl());
                    return models;
                }
            }
        } catch (Exception e) {
            log.error("Error fetching models for provider '{}' baseURL {}", getDisplayName(), getBaseUrl(), e);
        }
        return Collections.emptyList();
    }

    /**
     * Constructs a concrete model instance for this provider.
     * @param node The JSON node containing model metadata.
     * @return A new model instance.
     */
    protected OpenAiCompatibleModel createModel(JsonNode node) {
        return new OpenAiCompatibleModel(this, node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Provides a standard header hint for the multi-key
     * configuration file.
     * </p>
     */
    @Override
    public String getApiKeyHint() {
        return "# OpenAI-Compatible API Key Configuration\n"
                + "# Add your keys below (one per line).";
    }
}
