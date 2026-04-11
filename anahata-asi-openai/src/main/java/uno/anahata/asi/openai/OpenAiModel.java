/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.provider.AbstractAgiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.openai.adapter.OpenAiContentAdapter;

/**
 * A concrete implementation of {@link AbstractModel} that communicates with any
 * OpenAI-compatible Chat Completion API using the standard JDK HttpClient.
 *
 * @author anahata
 */
@Slf4j
public class OpenAiModel extends AbstractModel {

    /**
     * The parent universal provider.
     */
    private final OpenAiCompatibleProvider provider;

    /**
     * The unique model ID used in the API request (e.g., 'gpt-4o').
     */
    private final String modelId;

    /**
     * The human-friendly display name.
     */
    private final String displayName;

    /**
     * Shared JDK HttpClient for zero-dependency portability.
     */
    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    /**
     * Constructs a new model reference for an OpenAI-compatible endpoint.
     * @param provider The parent universal provider instance.
     * @param modelId The unique identifier used in API requests.
     * @param displayName The human-friendly name for UI display.
     */
    public OpenAiModel(OpenAiCompatibleProvider provider, String modelId, String displayName) {
        this.provider = provider;
        this.modelId = modelId;
        this.displayName = displayName;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the parent provider instance for
     * configuration access.
     * </p>
     */
    @Override
    public AbstractAgiProvider getProvider() {
        return provider;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the unique string identifier used in
     * the OpenAI API payload.
     * </p>
     */
    @Override
    public String getModelId() {
        return modelId;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the human-friendly name configured
     * during discovery.
     * </p>
     */
    @Override
    public String getDisplayName() {
        return displayName;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Provides a standard description identifying the
     * model as OpenAI-compatible.
     * </p>
     */
    @Override
    public String getDescription() {
        return "OpenAI-compatible model: " + modelId;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Always returns 'N/A' as the OpenAI API does not
     * expose a discrete version string separate from the model ID.
     * </p>
     */
    @Override
    public String getVersion() {
        return "N/A";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a safe default of 128K tokens, which is
     * typical for modern OpenAI-compatible models.
     * </p>
     */
    @Override
    public int getMaxInputTokens() {
        return 128000; // Typical default for modern models
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a standard default of 4096 tokens for the
     * completion response.
     * </p>
     */
    @Override
    public int getMaxOutputTokens() {
        return 4096;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a list containing only 'generateContent',
     * reflecting the primary API capability.
     * </p>
     */
    @Override
    public List<String> getSupportedActions() {
        return List.of("generateContent");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Generates an HTML-formatted snippet displaying the
     * model ID for the UI selector.
     * </p>
     */
    @Override
    public String getRawDescription() {
        return "<html><b>Model ID:</b> " + modelId + "</html>";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Always returns true, assuming standard function
     * calling support for universal compatibility.
     * </p>
     */
    @Override
    public boolean isSupportsFunctionCalling() {
        return true;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Always returns true for the chat completion
     * endpoint.
     * </p>
     */
    @Override
    public boolean isSupportsContentGeneration() {
        return true;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns false; batch embeddings are not supported
     * in this universal adapter.
     * </p>
     */
    @Override
    public boolean isSupportsBatchEmbeddings() {
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns false; embedding support is handled
     * separately from chat completion.
     * </p>
     */
    @Override
    public boolean isSupportsEmbeddings() {
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns false; prompt caching is not universally
     * supported across all OpenAI-compatible endpoints.
     * </p>
     */
    @Override
    public boolean isSupportsCachedContent() {
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a list containing only 'TEXT' as the
     * supported modality.
     * </p>
     */
    @Override
    public List<String> getSupportedResponseModalities() {
        return List.of("TEXT");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns an empty list; server-side tools are not
     * integrated in this adapter.
     * </p>
     */
    @Override
    public List<uno.anahata.asi.agi.provider.ServerTool> getAvailableServerTools() {
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns an empty list as no server tools are enabled
     * by default.
     * </p>
     */
    @Override
    public List<uno.anahata.asi.agi.provider.ServerTool> getDefaultServerTools() {
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a standard temperature of 0.7 for creative yet stable output.
     * </p>
     */
    @Override
    public Float getDefaultTemperature() {
        return 0.7f;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns null; Top-K is not a standard OpenAI
     * parameter.
     * </p>
     */
    @Override
    public Integer getDefaultTopK() {
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a standard default of 0.95.
     * </p>
     */
    @Override
    public Float getDefaultTopP() {
        return 0.95f;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Orchestrates the HTTP POST request to the
     * configured endpoint. Synthesizes the payload, manages authentication headers,
     * and parses the JSON response into an {@link OpenAiResponse}.
     * </p>
     */
    @Override
    public Response generateContent(GenerationRequest request) {
        try {
            String trimmedBaseUrl = provider.getBaseUrl() != null ? provider.getBaseUrl().trim() : "";
            ObjectNode payload = preparePayload(request, false);
            String jsonPayload = payload.toString();
            String historyJson = payload.get("messages").toString();

            HttpRequest httpRequest = HttpRequest.newBuilder()
                    .uri(URI.create(trimmedBaseUrl.endsWith("/") ? trimmedBaseUrl + "chat/completions" : trimmedBaseUrl + "/chat/completions"))
                    .header("Content-Type", "application/json")
                    .header("Authorization", "Bearer " + provider.getCurrentApiKey())
                    .POST(HttpRequest.BodyPublishers.ofString(jsonPayload))
                    .build();

            HttpResponse<String> httpResponse = HTTP_CLIENT.send(httpRequest, HttpResponse.BodyHandlers.ofString());

            if (httpResponse.statusCode() != 200) {
                throw new RuntimeException("API error (" + httpResponse.statusCode() + "): " + httpResponse.body());
            }

            return new OpenAiResponse(request.config().getAgi(), modelId, httpResponse.body(), jsonPayload, historyJson);

        } catch (Exception e) {
            log.error("Failed to generate content", e);
            throw new RuntimeException(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Currently unsupported. Universal provider
     * implementations focus on synchronous completion in this version.
     * </p>
     */
    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        // TODO: Implement SSE streaming using HttpClient.sendAsync
        // For now, falling back to sync for the universal provider's MVP.
        observer.onError(new UnsupportedOperationException("Streaming not yet implemented for universal provider."));
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Translates the Anahata tool metadata and JSON
     * schemas into the standard OpenAI 'function' declaration format.
     * </p>
     */
    @Override
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        // Simplified OpenAI tool declaration
        ObjectNode toolNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        toolNode.put("type", "function");
        ObjectNode funcNode = toolNode.putObject("function");
        funcNode.put("name", tool.getName());
        funcNode.put("description", tool.getDescription());
        // Mapping our purified parameters JSON schemas
        ObjectNode paramsNode = funcNode.putObject("parameters");
        paramsNode.put("type", "object");
        ObjectNode propsNode = paramsNode.putObject("properties");
        ArrayNode requiredNode = paramsNode.putArray("required");

        tool.getParameters().forEach(p -> {
            propsNode.set(p.getName(), SchemaProvider.OBJECT_MAPPER.valueToTree(p.getJsonSchema()));
            if (p.isRequired()) {
                requiredNode.add(p.getName());
            }
        });

        return toolNode.toPrettyString();
    }

    /**
     * Synthesizes the full OpenAI Chat Completion payload.
     */
    private ObjectNode preparePayload(GenerationRequest request, boolean stream) {
        ObjectNode payload = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        payload.put("model", modelId);
        payload.put("stream", stream);

        ArrayNode messages = payload.putArray("messages");
        boolean includePruned = request.config().isIncludePruned();

        for (AbstractMessage msg : request.history()) {
            List<ObjectNode> translated = new OpenAiContentAdapter(msg, includePruned).toOpenAi();
            messages.addAll(translated);
        }

        // Add tools if enabled
        List<? extends AbstractTool> localTools = request.config().getLocalTools();
        if (localTools != null && !localTools.isEmpty()) {
            ArrayNode toolsArray = payload.putArray("tools");
            for (AbstractTool<?, ?> tool : localTools) {
                try {
                    toolsArray.add(SchemaProvider.OBJECT_MAPPER.readTree(getToolDeclarationJson(tool, request.config())));
                } catch (Exception e) {
                    log.error("Failed to parse tool declaration for {}", tool.getName(), e);
                }
            }
        }

        if (request.config().getTemperature() != null) {
            payload.put("temperature", request.config().getTemperature());
        }

        return payload;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the display name or model ID for easy
     * identification in logs and UI.
     * </p>
     */
    @Override
    public String toString() {
        return getDisplayName().isEmpty() ? modelId : getDisplayName();
    }
}
