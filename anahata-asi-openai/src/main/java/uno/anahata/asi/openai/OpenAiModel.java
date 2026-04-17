/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.RetryableApiException;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.openai.adapter.OpenAiContentAdapter;

/**
 * A concrete implementation of {@link AbstractModel} that communicates with any
 * OpenAI-compatible Chat Completion API using the standard JDK HttpClient.
 */
@Slf4j
@Getter
@Setter
public class OpenAiModel extends AbstractModel {

    private final OpenAiCompatibleProvider provider;
    private final String modelId;
    private final String displayName;
    private String version = "";
    private int maxInputTokens = 128000;
    private int maxOutputTokens = 4096;

    private ReasoningStyle reasoningStyle = ReasoningStyle.NONE;
    private String reasoningFieldName;
    private List<String> reasoningTags;

    private boolean supportsFunctionCalling = true;
    private boolean supportsContentGeneration = true;
    private boolean supportsBatchEmbeddings = false;
    private boolean supportsEmbeddings = false;
    private boolean supportsCachedContent = false;

    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    public OpenAiModel(OpenAiCompatibleProvider provider, String modelId, String displayName) {
        this.provider = provider;
        this.modelId = modelId;
        this.displayName = displayName;
    }

    @Override
    public String getDescription() {
        return modelId;
    }

    @Override
    public List<String> getSupportedActions() {
        return List.of("chat/completions");
    }

    @Override
    public String getRawDescription() {
        return "<html><b>Model ID:</b> " + modelId + "</html>";
    }

    @Override
    public boolean isSupportsFunctionCalling() {
        return supportsFunctionCalling;
    }

    @Override
    public boolean isSupportsContentGeneration() {
        return supportsContentGeneration;
    }

    @Override
    public boolean isSupportsBatchEmbeddings() {
        return supportsBatchEmbeddings;
    }

    @Override
    public boolean isSupportsEmbeddings() {
        return supportsEmbeddings;
    }

    @Override
    public boolean isSupportsCachedContent() {
        return supportsCachedContent;
    }

    @Override
    public List<String> getSupportedResponseModalities() {
        String lowerId = modelId.toLowerCase();
        if (lowerId.contains("vision") || lowerId.contains("gpt-4o") || lowerId.contains("claude-3")) {
            return List.of("TEXT", "IMAGE");
        }
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
        Agi agi = request.config().getAgi();
        ObjectNode payload = preparePayload(request, false);
        String jsonPayload = payload.toString();
        String historyJson = payload.get("messages").toString();
        try {
            HttpRequest httpRequest = provider.createRequestBuilder("chat/completions").header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(jsonPayload)).build();
            try (HttpClient client = provider.createHttpClient()) {
                HttpResponse<String> httpResponse = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());
                if (httpResponse.statusCode() != 200) {
                    String errorBody = httpResponse.body();
                    if (provider.isRetryable(httpResponse.statusCode(), errorBody)) {
                        log.info("Retryable error detected ({}). Rotating key and retrying...", httpResponse.statusCode());
                        provider.hokusPocus();
                        throw new RetryableApiException(provider.getCurrentApiKey(), "API error (" + httpResponse.statusCode() + "): " + errorBody, null);
                    }
                    throw new RuntimeException("API error (" + httpResponse.statusCode() + "): " + errorBody);
                }
                return new OpenAiResponse(agi, modelId, httpResponse.body(), jsonPayload, historyJson, this);
            }
        } catch (IOException | InterruptedException e) {
            log.error("Failed to execute OpenAI request", e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        Agi agi = request.config().getAgi();
        ObjectNode payload = preparePayload(request, true);
        String jsonPayload = payload.toString();
        String historyJson = payload.get("messages").toString();
        try {
            HttpRequest httpRequest = provider.createRequestBuilder("chat/completions").header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(jsonPayload)).build();
            try (HttpClient client = provider.createHttpClient()) {
                List<OpenAiModelMessage> targets = new ArrayList<>();
                AtomicBoolean started = new AtomicBoolean(false);
                HttpResponse<Stream<String>> response = client.send(httpRequest, HttpResponse.BodyHandlers.ofLines());
                if (response.statusCode() != 200) {
                    String errorMsg = "No error body";
                    try (Stream<String> bodyStream = response.body()) {
                        errorMsg = bodyStream.collect(Collectors.joining("\n"));
                    }

                    if (provider.isRetryable(response.statusCode(), errorMsg)) {
                        log.info("Retryable streaming error detected ({}). Rotating key and retrying...", response.statusCode());
                        provider.hokusPocus();
                        observer.onError(new RetryableApiException(provider.getCurrentApiKey(), "OpenAI Stream Error (" + response.statusCode() + "): " + errorMsg, null));
                    } else {
                        observer.onError(new RuntimeException("OpenAIModel Stream Error (" + response.statusCode() + "): " + errorMsg));
                    }
                    return;
                }
                try (Stream<String> lines = response.body()) {
                    var it = lines.iterator();
                    while (it.hasNext()) {
                        String line = it.next();
                        if (line == null || line.isBlank()) {
                            continue;
                        }
                        String trimmedLine = line.trim();
                        if (!trimmedLine.startsWith("data:")) {
                            continue;
                        }
                        String data = trimmedLine.substring(5).trim();
                        if ("[DONE]".equals(data)) {
                            targets.forEach(OpenAiModelMessage::flushToolCalls);
                            observer.onComplete();
                            break;
                        }
                        try {
                            JsonNode chunk = JacksonUtils.parse(data, JsonNode.class);
                            if (chunk.has("error")) {
                                log.error("Error in OpenAI stream chunk: {}", data);
                                observer.onError(new RuntimeException("OpenAI Stream Chunk Error: " + chunk.get("error").path("message").asText()));
                                return;
                            }
                            JsonNode choices = chunk.get("choices");
                            if (choices != null && choices.isArray() && choices.size() > 0) {
                                if (!started.get()) {
                                    for (int i = 0; i < choices.size(); i++) {
                                        targets.add(new OpenAiModelMessage(agi, modelId));
                                    }
                                    observer.onStart((List) targets);
                                    started.set(true);
                                }
                                for (int i = 0; i < Math.min(choices.size(), targets.size()); i++) {
                                    JsonNode choice = choices.get(i);
                                    OpenAiModelMessage target = targets.get(i);
                                    routeChunk(choice, target);
                                }
                            }
                            observer.onNext(new OpenAiResponse(agi, modelId, data, jsonPayload, historyJson, this));
                        } catch (Exception e) {
                            log.error("Failed to parse OpenAI stream chunk: {}", data, e);
                        }
                    }
                }
            }

        } catch (Exception e) {
            log.error("Failed to execute OpenAI stream", e);
            observer.onError(e);
        }
    }

    private void routeChunk(JsonNode choice, OpenAiModelMessage target) {
        JsonNode delta = choice.get("delta");
        if (delta == null) {
            delta = choice.get("message");
        }
        if (delta == null) {
            return;
        }

        // 1. Handle Reasoning (Field style - e.g. DeepSeek)
        if (reasoningStyle == ReasoningStyle.FIELD && reasoningFieldName != null
                && delta.has(reasoningFieldName) && !delta.get(reasoningFieldName).isNull()) {
            target.appendThoughts(delta.get(reasoningFieldName).asText());
        }

        // 2. Handle Content (might contain TAGS style reasoning)
        if (delta.has("content") && !delta.get("content").isNull()) {
            String text = delta.get("content").asText();
            if (reasoningStyle == ReasoningStyle.TAGS && reasoningTags != null && reasoningTags.size() >= 2) {
                target.appendTaggedContent(text, reasoningTags.get(0), reasoningTags.get(1));
            } else {
                target.appendContent(text);
            }
        }

        // 3. Tool Calls (Metadata parsing still handled by message for now)
        if (delta.has("tool_calls")) {
            for (JsonNode callNode : delta.get("tool_calls")) {
                target.updateToolCall(callNode);
            }
        }

        // 4. Finish Reason
        if (choice.has("finish_reason") && !choice.get("finish_reason").isNull()) {
            target.setFinishReasonFromOpenAi(choice.get("finish_reason").asText());
        }
    }

    @Override
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        ObjectNode toolNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        toolNode.put("type", "function");
        ObjectNode funcNode = toolNode.putObject("function");
        funcNode.put("name", tool.getName());
        funcNode.put("description", tool.getDescription());

        ObjectNode paramsNode = funcNode.putObject("parameters");
        paramsNode.put("type", "object");
        ObjectNode propsNode = paramsNode.putObject("properties");
        ArrayNode requiredNode = paramsNode.putArray("required");

        tool.getParameters().forEach(p -> {
            try {
                ObjectNode paramNode = (ObjectNode) SchemaProvider.OBJECT_MAPPER.readTree(p.getJsonSchema());
                String desc = p.getDescription();
                if (desc != null && !desc.isBlank()) {
                    paramNode.put("description", desc);
                }
                propsNode.set(p.getName(), paramNode);
            } catch (Exception e) {
                log.error("Failed to parse parameter schema for {}", p.getName(), e);
            }
            if (p.isRequired()) {
                requiredNode.add(p.getName());
            }
        });

        return toolNode.toPrettyString();
    }

    private ObjectNode preparePayload(GenerationRequest request, boolean stream) {
        ObjectNode payload = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        payload.put("model", modelId);
        payload.put("stream", stream);

        ArrayNode messages = payload.putArray("messages");
        
        // 1. Inject System Instructions if present in config
        if (!request.config().getSystemInstructions().isEmpty()) {
            for (String si : request.config().getSystemInstructions()) {
                messages.addObject()
                        .put("role", "system")
                        .put("content", si);
            }
        }

        // 2. Inject Conversation History
        boolean includePruned = request.config().isIncludePruned();
        for (AbstractMessage msg : request.history()) {
            List<ObjectNode> translated = new OpenAiContentAdapter(msg, includePruned, getTokenizerType()).toOpenAi();
            messages.addAll(translated);
        }

        // 3. Handle Reasoning/Thinking flags
        if (request.config().getAgi().getConfig().isIncludeThoughts()) {
            // For DeepSeek/HuggingFace Router
            payload.put("include_reasoning", true);
        }

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
        if (request.config().getTopP() != null) {
            payload.put("top_p", request.config().getTopP());
        }
        if (request.config().getMaxOutputTokens() != null) {
            payload.put("max_tokens", request.config().getMaxOutputTokens());
        }

        return payload;
    }

}
