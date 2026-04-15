/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

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
import uno.anahata.asi.agi.provider.AbstractAgiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.TokenizerType;

/**
 * A universal AI provider for any API endpoint compatible with the OpenAI 
 * Chat Completion specification.
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
public class OpenAiCompatibleProvider extends AbstractAgiProvider {

    /**
     * The base URL of the OpenAI-compatible API endpoint.
     * <p>
     * This allows the provider to target official OpenAI services or
     * alternative backends like Ollama (http://localhost:11434/v1),
     * Groq, or DeepSeek.
     * </p>
     */
    private String baseUrl;
    /**
     * Optional custom HTTP headers to be sent with every request.
     * Essential for providers requiring specific vendor headers (e.g., 'X-Title', 'OpenAI-Organization').
     */
    private Map<String, String> customHeaders = new HashMap<>();

    /**
     * Constructs a default OpenAI-compatible provider.
     * <p>
     * Initializes the display name to 'Universal' and sets the default
     * tokenizer to {@link TokenizerType#CL100K_BASE}. Required for Kryo
     * deserialization.
     * </p>
     */
    public OpenAiCompatibleProvider() {
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
    public OpenAiCompatibleProvider(String uuid, String displayName, String baseUrl) {
        this(uuid, displayName, baseUrl, null, null);
    }

    /**
     * Constructs a new universal provider with a custom storage folder.
     * @param uuid The unique provider ID.
     * @param displayName The user-facing name.
     * @param baseUrl The API endpoint URL.
     * @param folderName The custom folder name for configuration and key storage.
     */
    public OpenAiCompatibleProvider(String uuid, String displayName, String baseUrl, String folderName, String apiKeyAdquisitionUri) {
        super(uuid);
        setDisplayName(displayName);
        setBaseUrl(baseUrl);
        setKeysAcquisitionUri(apiKeyAdquisitionUri);
        setFolderName(folderName);
        setTokenizerType(TokenizerType.CL100K_BASE);
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

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl != null ? baseUrl.trim() : null;
    }

    public String getBaseUrl() {
        return baseUrl != null ? baseUrl.trim() : null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs a GET request to the {@code /models}
     * endpoint of the configured {@code baseUrl}. Parses the standard OpenAI
     * list-response and wraps each ID in an {@link OpenAiModel} instance.
     * </p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        String apiKey = getCurrentApiKey();
        String url = getBaseUrl();
        log.info("Listing models for {} {}", getDisplayName(), url);
        if (url == null || (apiKey == null && isApiKeyRequired())) {
            log.warn("Cannot list models: baseUrl or API key missing (and required) for provider '{}'", getDisplayName());
            return Collections.emptyList();
        }
        try {
            String modelsUrl = url.endsWith("/") ? url + "models" : url + "/models";
            HttpRequest.Builder requestBuilder = HttpRequest
                    .newBuilder()
                    .timeout(Duration.ofSeconds(9))
                    .uri(URI.create(modelsUrl))
                    .header("Accept", "application/json")
                    .GET();
            
            if (apiKey != null) {
                log.info("listModels for {}", getDisplayName() + " will use apiKey ending with ..." + apiKey.substring(apiKey.length() - 4, apiKey.length()));
                requestBuilder.header("Authorization", "Bearer " + apiKey);
            }
            
            getCustomHeaders().forEach(requestBuilder::header);
            HttpRequest httpRequest = requestBuilder.build();
            HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();
            try (client) {
                log.info("Listing models for {} modelsUrl={} ", getDisplayName(), modelsUrl);
                Thread.dumpStack();
                HttpResponse<String> httpResponse = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());
                log.info("Got response from listModels: " + httpResponse);
                if (httpResponse.statusCode() != 200) {
                    log.error("Failed to fetch models from {}. Status: {}. Response: {}", modelsUrl, httpResponse.statusCode(), httpResponse.body());
                    return Collections.emptyList();
                }
                JsonNode root = JacksonUtils.parse(httpResponse.body(), JsonNode.class);
                JsonNode data = root.get("data");
                if (data != null && data.isArray()) {
                    List<OpenAiModel> models = new ArrayList<>();
                    for (JsonNode modelNode : data) {
                        String id = modelNode.path("id").asText();
                        if (!id.isBlank()) {
                            models.add(new OpenAiModel(this, id, id));
                        }
                    }
                    log.info("Successfully discovered {} models from {}", models.size(), baseUrl);
                    return models;
                }
            }
        } catch (Exception e) {
            log.error("Error fetching models for provider '{}' at {}", getDisplayName(), baseUrl, e);
        }
        return Collections.emptyList();
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
