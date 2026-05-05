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

    private static final ObjectMapper API_MAPPER = new ObjectMapper();
    private transient HttpClient httpClient;

    public synchronized HttpClient getHttpClient() {
        if (httpClient == null) {
            httpClient = HttpClient.newBuilder()
                    .connectTimeout(Duration.ofSeconds(10))
                    .followRedirects(HttpClient.Redirect.NORMAL)
                    .build();
        }
        return httpClient;
    }

    public OpenAiProvider() {
        super("OpenAI");
        setDisplayName("OpenAI (Responses)");
        setTokenizerType(TokenizerType.CL100K_BASE);
        setKeysAcquisitionUri("https://platform.openai.com/api-keys");
    }

    @Override
    public List<? extends AbstractModel> listModels() {
        log.info("Fetching OpenAI models...");
        try {
            String apiKey = getCurrentKey();
            if (apiKey == null) {
                return List.of();
            }

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create("https://api.openai.com/v1/models"))
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

    @Override
    public String getApiKeyHint() {
        return "# OpenAI API Key Configuration\n"
                + "# -----------------------------\n"
                + "# Add one key per line.\n"
                + "sk-proj-...\n";
    }
}
