/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.RetryableApiException;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolParameter;
import uno.anahata.asi.anthropic.adapter.AnthropicContentAdapter;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Model implementation for Anthropic's Claude.
 * 
 * @author anahata
 */
@Slf4j
public class AnthropicModel extends AbstractModel {

    /**
     * The parent provider instance.
     */
    private final AnthropicProvider provider;
    /**
     * The unique identifier for the model.
     */
    private final String modelId;
    /**
     * The user-friendly name of the model.
     */
    private final String displayName;
    /**
     * The version identifier for the model.
     */
    private final String version;

    /**
     * Constructs a new Anthropic model instance with full version details.
     * @param provider The parent provider.
     * @param modelId The model ID.
     * @param displayName The display name.
     * @param version The model version.
     */
    public AnthropicModel(AnthropicProvider provider, String modelId, String displayName, String version) {
        this.provider = provider;
        this.modelId = modelId;
        this.displayName = displayName;
        this.version = version;
    }

    public AnthropicModel(AnthropicProvider provider, String modelId, String displayName) {
        this(provider, modelId, displayName, "");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AnthropicProvider getProvider() { return provider; }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getModelId() { return modelId; }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDisplayName() { return displayName; }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() { return "Anthropic Claude Model"; }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersion() { return version; }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMaxInputTokens() { return 200000; }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMaxOutputTokens() { return 8192; }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedActions() { return List.of("messages"); }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getRawDescription() { return "<html><b>Model ID:</b> " + modelId + "<br><b>Version:</b> " + version + "</html>"; }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsFunctionCalling() { return true; }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsContentGeneration() { return true; }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsBatchEmbeddings() { return false; }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsEmbeddings() { return false; }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsCachedContent() { return true; }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedResponseModalities() { return List.of("TEXT", "IMAGE"); }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ServerTool> getAvailableServerTools() { return Collections.emptyList(); }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ServerTool> getDefaultServerTools() { return Collections.emptyList(); }

    /**
     * {@inheritDoc}
     */
    @Override
    public Float getDefaultTemperature() { return 0.7f; }

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer getDefaultTopK() { return null; }

    /**
     * {@inheritDoc}
     */
    @Override
    public Float getDefaultTopP() { return null; }

    protected ObjectNode preparePayload(GenerationRequest request, boolean stream) {
        ObjectNode payload = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        payload.put("model", modelId);
        
        int maxTokens = request.config().getMaxOutputTokens() != null ? request.config().getMaxOutputTokens() : getMaxOutputTokens();
        payload.put("max_tokens", maxTokens);
        
        if (stream) payload.put("stream", true);

        if (request.config().getTemperature() != null) payload.put("temperature", request.config().getTemperature());
        if (request.config().getTopP() != null) payload.put("top_p", request.config().getTopP());

        List<String> si = request.config().getSystemInstructions();
        if (!si.isEmpty()) {
            payload.put("system", String.join("\n\n", si));
        }

        List<? extends AbstractTool> localTools = request.config().getLocalTools();
        if (localTools != null && !localTools.isEmpty()) {
            ArrayNode toolsArray = payload.putArray("tools");
            for (AbstractTool<?, ?> tool : localTools) {
                try {
                    toolsArray.add(SchemaProvider.OBJECT_MAPPER.readTree(getToolDeclarationJson(tool, request.config())));
                } catch (Exception e) {
                    log.error("Failed to parse tool", e);
                }
            }
        }

        ArrayNode messagesArray = payload.putArray("messages");
        boolean includePruned = request.config().isIncludePruned();
        for (AbstractMessage msg : request.history()) {
            List<ObjectNode> translated = new AnthropicContentAdapter(msg, includePruned).toAnthropic();
            messagesArray.addAll(translated);
        }

        return payload;
    }

    @Override
    public Response generateContent(GenerationRequest request) {
        Agi agi = request.config().getAgi();
        ObjectNode payload = preparePayload(request, false);
        
        ObjectNode configNode = payload.deepCopy();
        configNode.remove("messages");
        String configJson = configNode.toPrettyString();
        
        JsonNode messagesNode = payload.get("messages");
        String historyJson = messagesNode != null ? messagesNode.toPrettyString() : "[]";

        log.info("Executing Anthropic request to messages endpoint");
        try {
            HttpRequest httpRequest = provider.createRequestBuilder("messages")
                    .POST(HttpRequest.BodyPublishers.ofString(payload.toString()))
                    .build();
            HttpClient client = provider.getHttpClient(); {
                HttpResponse<String> httpResponse = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());
                if (httpResponse.statusCode() != 200) {
                    String errorBody = httpResponse.body();
                    if (provider.isRetryable(httpResponse.statusCode(), errorBody)) {
                        provider.hokusPocus();
                        throw new RetryableApiException(provider.getCurrentApiKey(), "API error (" + httpResponse.statusCode() + "): " + errorBody, null);
                    }
                    throw new RuntimeException("API error (" + httpResponse.statusCode() + "): " + errorBody);
                }
                return new AnthropicResponse(configJson, historyJson, agi, modelId, httpResponse.body());
            }
        } catch (IOException | InterruptedException e) {
            log.error("Failed to execute Anthropic request", e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        Agi agi = request.config().getAgi();
        ObjectNode payload = preparePayload(request, true);
        
        ObjectNode configNode = payload.deepCopy();
        configNode.remove("messages");
        String configJson = configNode.toPrettyString();
        
        JsonNode messagesNode = payload.get("messages");
        String historyJson = messagesNode != null ? messagesNode.toPrettyString() : "[]";

        log.info("Executing Anthropic streaming request");
        try {
            HttpRequest httpRequest = provider.createRequestBuilder("messages")
                    .header("Accept", "text/event-stream")
                    .POST(HttpRequest.BodyPublishers.ofString(payload.toString()))
                    .build();
                    
            HttpClient client = provider.getHttpClient(); {
                AnthropicMessage target = new AnthropicMessage(agi, modelId);
                List<AnthropicMessage> targets = List.of(target);
                AtomicBoolean started = new AtomicBoolean(false);
                
                HttpResponse<Stream<String>> response = client.send(httpRequest, HttpResponse.BodyHandlers.ofLines());
                if (response.statusCode() != 200) {
                    String errorMsg = "No error body";
                    try (Stream<String> bodyStream = response.body()) {
                        errorMsg = bodyStream.collect(Collectors.joining("\n"));
                    }
                    if (provider.isRetryable(response.statusCode(), errorMsg)) {
                        provider.hokusPocus();
                        observer.onError(new RetryableApiException(provider.getCurrentApiKey(), "Stream Error (" + response.statusCode() + "): " + errorMsg, null));
                    } else {
                        observer.onError(new RuntimeException("Stream Error (" + response.statusCode() + "): " + errorMsg));
                    }
                    return;
                }
                
                try (Stream<String> lines = response.body()) {
                    Iterator<String> it = lines.iterator();
                    String currentEvent = null;
                    
                    while (it.hasNext()) {
                        String line = it.next();
                        if (line == null || line.isBlank()) continue;
                        
                        if (line.startsWith("event: ")) {
                            currentEvent = line.substring(7).trim();
                        } else if (line.startsWith("data: ")) {
                            String data = line.substring(6).trim();
                            if (!started.get()) {
                                observer.onStart((List) targets);
                                started.set(true);
                            }
                            
                            try {
                                JsonNode chunk = JacksonUtils.parse(data, JsonNode.class);
                                if (currentEvent != null) {
                                    target.handleEvent(currentEvent, chunk);
                                }
                                target.appendRawJson(data);
                                
                                AnthropicResponse chunkResponse = new AnthropicResponse(configJson, historyJson, data);
                                observer.onNext(chunkResponse);
                                
                            } catch (Exception e) {
                                log.error("Failed to parse Anthropic stream chunk: {}", data, e);
                            }
                        }
                    }
                }
                target.setStreaming(false);
                observer.onComplete();
            }

        } catch (Exception e) {
            log.error("Failed to execute Anthropic stream", e);
            observer.onError(e);
        }
    }

    @Override
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        ObjectNode toolNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        toolNode.put("name", tool.getName());
        toolNode.put("description", tool.getDescription());

        ObjectNode inputSchema = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        inputSchema.put("type", "object");
        ObjectNode properties = inputSchema.putObject("properties");
        ArrayNode required = inputSchema.putArray("required");

        for (AbstractToolParameter<?> param : tool.getParameters()) {
            try {
                ObjectNode paramSchema = (ObjectNode) SchemaProvider.OBJECT_MAPPER.readTree(param.getJsonSchema());
                if (param.getDescription() != null && !param.getDescription().isBlank()) {
                    paramSchema.put("description", param.getDescription());
                }
                properties.set(param.getName(), paramSchema);
                if (param.isRequired()) {
                    required.add(param.getName());
                }
            } catch (Exception e) {
                log.error("Error parsing schema", e);
            }
        }
        toolNode.set("input_schema", inputSchema);
        return toolNode.toPrettyString();
    }
    
    
}