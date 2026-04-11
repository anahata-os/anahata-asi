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
     * The base URL of the OpenAI-compatible API (e.g., 'http://localhost:11434/v1'). 
     */
    private String baseUrl;

    /**
     * No-arg constructor for Kryo.
     */
    public OpenAiCompatibleProvider() {
        super();
        setDisplayName("OpenAI Compatible (Universal)");
        setTokenizerType(TokenizerType.CL100K_BASE);
    }

    /**
     * Constructs a new universal provider.
     * 
     * @param uuid The unique ID.
     * @param displayName The display name.
     * @param baseUrl The API endpoint.
     */
    public OpenAiCompatibleProvider(String uuid, String displayName, String baseUrl) {
        this(uuid, displayName, baseUrl, null);
    }

    /**
     * Constructs a new universal provider with a custom folder name.
     * 
     * @param uuid The unique ID.
     * @param displayName The display name.
     * @param baseUrl The API endpoint.
     * @param folderName The custom folder name for configuration.
     */
    public OpenAiCompatibleProvider(String uuid, String displayName, String baseUrl, String folderName) {
        super(uuid);
        setDisplayName(displayName);
        this.baseUrl = baseUrl;
        setFolderName(folderName);
        setTokenizerType(TokenizerType.CL100K_BASE);
    }

    /** {@inheritDoc} */
    @Override
    public String getCurrentApiKey() {
        return getNextKey();
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl != null ? baseUrl.trim() : null;
    }

    /** {@inheritDoc} */
    @Override
    public List<? extends AbstractModel> listModels() {
        String apiKey = getCurrentApiKey();
        if (baseUrl == null || apiKey == null) {
            log.warn("Cannot list models: baseUrl or API key missing for provider '{}'", getProviderId());
            return Collections.emptyList();
        }

        try {
            String modelsUrl = baseUrl.endsWith("/") ? baseUrl + "models" : baseUrl + "/models";
            HttpRequest httpRequest = HttpRequest.newBuilder()
                    .uri(URI.create(modelsUrl))
                    .header("Authorization", "Bearer " + apiKey)
                    .header("Accept", "application/json")
                    .GET()
                    .build();

            try (HttpClient client = HttpClient.newBuilder()
                    .connectTimeout(java.time.Duration.ofSeconds(10))
                    .build()) {
                
                HttpResponse<String> httpResponse = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());

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
            log.error("Error fetching models for provider '{}' at {}", getProviderId(), baseUrl, e);
        }

        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    public URI getKeysAcquisitionUri() {
        return URI.create("https://platform.openai.com/api-keys");
    }

    /** {@inheritDoc} */
    @Override
    public String getApiKeyHint() {
        return "# OpenAI-Compatible API Key Configuration\n"
                + "# Add your keys below (one per line).";
    }
}
