/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini;

import com.google.genai.Client;
import com.google.genai.errors.ClientException;
import com.google.genai.types.Content;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.Model;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.gemini.adapter.GeminiContentAdapter;
import uno.anahata.ai.gemini.adapter.RequestConfigAdapter;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.tool.RetryableApiException;

/**
 * Gemini-specific implementation of the {@code AbstractModel}. It wraps the
 * native Google GenAI {@code Model} object and implements the abstract methods
 * from the superclass by delegating to the wrapped object.
 *
 * @author anahata-gemini-pro-2.5
 */
@RequiredArgsConstructor
@Slf4j
public class GeminiModel extends AbstractModel {

    private final GeminiAiProvider provider;
    private final Model genaiModel;

    @Override
    public AbstractAiProvider getProvider() {
        return provider;
    }

    @Override
    public String getModelId() {
        return genaiModel.name().orElse("");
    }

    @Override
    public String getDisplayName() {
        return genaiModel.displayName().orElse("");
    }

    @Override
    public String getDescription() {
        return genaiModel.description().orElse("No description available.");
    }

    @Override
    public String getVersion() {
        return genaiModel.version().orElse("");
    }

    @Override
    public int getMaxInputTokens() {
        return genaiModel.inputTokenLimit().orElse(0);
    }

    @Override
    public int getMaxOutputTokens() {
        return genaiModel.outputTokenLimit().orElse(0);
    }

    @Override
    public List<String> getSupportedActions() {
        return genaiModel.supportedActions().orElse(Collections.emptyList());
    }

    @Override
    public String getRawDescription() {
        String json = genaiModel.toJson();
        String toString = genaiModel.toString();

        // Return only the inner content. WrappingHtmlPane will add the <html><body> tags.
        return "<html><b>ID: </b>" + escapeHtml(getModelId()) + "<br>"
                + "<b>Display Name: </b>" + escapeHtml(getDisplayName()) + "<br>"
                + "<b>Version: </b>" + escapeHtml(getVersion()) + "<br>"
                + "<b>Description: </b>" + escapeHtml(getDescription()) + "<br>"
                + "<b>Supported Actions: </b>" + getSupportedActions() + "<br>"
                + "<b>Labels: </b>" + genaiModel.labels().orElse(Collections.EMPTY_MAP) + "<br>"
                + "<b>TunedModelInfo: </b>" + genaiModel.tunedModelInfo().orElse(null) + "<br>"
                + "<hr>"
                /*
                + "<b>JSON:</b>"
                + "<pre style='white-space: pre-wrap; word-wrap: break-word;'>"
                + escapeHtml(json)
                + "</pre>"
                + "<hr>"
                 */
                + "<b>toString():</b><pre style='white-space: pre-wrap; word-wrap: break-word;'></pre>"
                + "<div style='width: 300px;'>"
                + toString
                + "</pre></div></html>";
    }

    private String escapeHtml(String text) {
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#x27;")
                .replace("/", "&#x2F;");
    }

    @Override
    public boolean isSupportsFunctionCalling() {
        return getSupportedActions().contains("tool");
    }

    @Override
    public boolean isSupportsContentGeneration() {
        return getSupportedActions().contains("generateContent");
    }

    @Override
    public boolean isSupportsBatchEmbeddings() {
        return getSupportedActions().contains("batchEmbedContents");
    }

    @Override
    public boolean isSupportsEmbeddings() {
        return getSupportedActions().contains("embedContent");
    }

    @Override
    public boolean isSupportsCachedContent() {
        return getSupportedActions().contains("createCachedContent");
    }

    @Override
    public Response generateContent(Chat chat, RequestConfig config, List<AbstractMessage> history) {
        Client client = provider.getClient();

        // 1. Adapt the anahata-ai request to the Gemini-specific request using the new OO adapter
        boolean includePruned = config.isIncludePruned();
        List<Content> googleHistory = history.stream()
                .map(msg -> new GeminiContentAdapter(msg, includePruned).toGoogle())
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        log.info("Sending request to Gemini model: {} {} content elements", getModelId(), googleHistory.size());
        for (Content content : googleHistory) {
            log.info(content.toJson());
        }

        // 2. Make the API call
        log.info("Sending request to Gemini model: {}", getModelId());
        try {
            GenerateContentConfig gcc = RequestConfigAdapter.toGoogle(config);
            GenerateContentResponse response = client.models.generateContent(
                    getModelId(),
                    googleHistory,
                    gcc
                    
            );
            log.info("Got response from Gemini model: {}", response.toJson());

            // 3. Convert the Gemini response to the Anahata response using the new OO response class.
            return new GeminiResponse(gcc.toJson(), chat, getModelId(), response);
        } catch (ClientException e) {
            log.error("Exception in generateContent", e);
            if (e.toString().contains("429") || e.toString().contains("503") || e.toString().contains("500")) {
                log.error("429, 503 or 500 exception, resetting client", e.getMessage());
                provider.resetClient();
                throw new RetryableApiException(client.apiKey(), e.toString(), e);
            }
            throw e;
        }

    }

    @Override
    public String toString() {
        return genaiModel.displayName().orElse(genaiModel.name().orElse("??"));

    }

}
