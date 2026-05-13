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

/**
 * Provider implementation for the official Anthropic API.
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class AnthropicProvider extends AbstractAiProvider {

    private String baseUrl = "https://api.anthropic.com/v1";
    private String anthropicVersion = "2023-06-01";

    public AnthropicProvider() {
        super("Anthropic");
        setDisplayName("Anthropic (Claude)");
        setTokenizerType(TokenizerType.CL100K_BASE); // Change when Claude tokenizer is available
        setKeysAcquisitionUri("https://console.anthropic.com/settings/keys");
    }

    public HttpClient createHttpClient() {
        return HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(15))
                .build();
    }

    public HttpRequest.Builder createRequestBuilder(String endpoint) {
        String fullUrl = baseUrl.endsWith("/") ? baseUrl + endpoint : baseUrl + "/" + endpoint;
        return HttpRequest.newBuilder()
                .uri(URI.create(fullUrl))
                .header("x-api-key", getCurrentApiKey())
                .header("anthropic-version", anthropicVersion)
                .timeout(Duration.ofSeconds(120));
    }

    @Override
    public List<? extends AbstractModel> listModels() {
        // Anthropic does not have a standard dynamic /models endpoint, 
        // so we provide a hardcoded manifest of the current state-of-the-art models.
        List<AnthropicModel> models = new ArrayList<>();
        models.add(new AnthropicModel(this, "claude-3-5-sonnet-latest", "Claude 3.5 Sonnet"));
        models.add(new AnthropicModel(this, "claude-3-5-haiku-latest", "Claude 3.5 Haiku"));
        models.add(new AnthropicModel(this, "claude-3-opus-latest", "Claude 3 Opus"));
        return models;
    }

    @Override
    public String getApiKeyHint() {
        return "# Anthropic API Key Configuration\n"
                + "sk-ant-...\n";
    }
}
