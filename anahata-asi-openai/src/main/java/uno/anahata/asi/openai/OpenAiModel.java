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
 * A concrete implementation of {@link AbstractModel} that communicates with 
 * any OpenAI-compatible Chat Completion API using the standard JDK HttpClient.
 * 
 * @author anahata
 */
@Slf4j
public class OpenAiModel extends AbstractModel {

    /** The parent universal provider. */
    private final OpenAiCompatibleProvider provider;
    
    /** The unique model ID used in the API request (e.g., 'gpt-4o'). */
    private final String modelId;
    
    /** The human-friendly display name. */
    private final String displayName;

    /** Shared JDK HttpClient for zero-dependency portability. */
    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    /**
     * Constructs a new model reference.
     */
    public OpenAiModel(OpenAiCompatibleProvider provider, String modelId, String displayName) {
        this.provider = provider;
        this.modelId = modelId;
        this.displayName = displayName;
    }

    @Override
    public AbstractAgiProvider getProvider() {
        return provider;
    }

    @Override
    public String getModelId() {
        return modelId;
    }

    @Override
    public String getDisplayName() {
        return displayName;
    }

    @Override
    public String getDescription() {
        return "OpenAI-compatible model: " + modelId;
    }

    @Override
    public String getVersion() {
        return "N/A";
    }

    @Override
    public int getMaxInputTokens() {
        return 128000; // Typical default for modern models
    }

    @Override
    public int getMaxOutputTokens() {
        return 4096;
    }

    @Override
    public List<String> getSupportedActions() {
        return List.of("generateContent");
    }

    @Override
    public String getRawDescription() {
        return "<html><b>Model ID:</b> " + modelId + "</html>";
    }

    @Override
    public boolean isSupportsFunctionCalling() {
        return true;
    }

    @Override
    public boolean isSupportsContentGeneration() {
        return true;
    }

    @Override
    public boolean isSupportsBatchEmbeddings() {
        return false;
    }

    @Override
    public boolean isSupportsEmbeddings() {
        return false;
    }

    @Override
    public boolean isSupportsCachedContent() {
        return false;
    }

    @Override
    public List<String> getSupportedResponseModalities() {
        return List.of("TEXT");
    }

    @Override
    public List<uno.anahata.asi.agi.provider.ServerTool> getAvailableServerTools() {
        return Collections.emptyList();
    }

    @Override
    public List<uno.anahata.asi.agi.provider.ServerTool> getDefaultServerTools() {
        return Collections.emptyList();
    }

    @Override
    public Float getDefaultTemperature() {
        return 0.7f;
    }

    @Override
    public Integer getDefaultTopK() {
        return null;
    }

    @Override
    public Float getDefaultTopP() {
        return 0.95f;
    }

    @Override
    public Response generateContent(GenerationRequest request) {
        try {
            ObjectNode payload = preparePayload(request, false);
            String jsonPayload = payload.toString();
            String historyJson = payload.get("messages").toString();
            
            HttpRequest httpRequest = HttpRequest.newBuilder()
                    .uri(URI.create(provider.getBaseUrl() + "/chat/completions"))
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

    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        // TODO: Implement SSE streaming using HttpClient.sendAsync
        // For now, falling back to sync for the universal provider's MVP.
        observer.onError(new UnsupportedOperationException("Streaming not yet implemented for universal provider."));
    }

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
}
