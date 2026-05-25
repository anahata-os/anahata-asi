/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.ResponseUsageMetadata;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.RetryableApiException;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.agi.tool.ToolResponseAttachment;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;
import uno.anahata.asi.internal.ImageMetadataUtils;
import uno.anahata.asi.internal.ImageMetadataUtils.ImageMetadata;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.openai.compatible.adapter.OpenAiCompatibleResponseAdapter;

/**
 * A concrete implementation of {@link AbstractModel} that communicates with any
 * OpenAI-compatible Chat Completion API using the standard JDK HttpClient.
 */
@Slf4j
@Getter
@Setter
public class OpenAiCompatibleModel extends AbstractModel {

    /**
     * The provider instance managing this model.
     */
    private final OpenAiChatCompletionsProvider provider;
    /**
     * The unique identifier for the model (e.g., 'gpt-4o', 'claude-3-5-sonnet').
     */
    private final String modelId;
    /**
     * The user-friendly name of the model.
     */
    private final String displayName;
    /** The model version. */
    private String version = "";
    /** The maximum input tokens allowed. */
    private int maxInputTokens = 200000;
    /** The maximum output tokens allowed. */
    private int maxOutputTokens = 32000;

    /** The reasoning extraction style used by this model. */
    private OpenAiCompatibleReasoningStyle reasoningStyle = OpenAiCompatibleReasoningStyle.NONE;
    /** The specific field name representing thoughts in the JSON response. */
    private String reasoningFieldName;
    /** The start and end tags wrapping reasoning content. */
    private List<String> reasoningTags;

    /** Whether the model supports native function calling. */
    private boolean supportsFunctionCalling = true;
    /** Whether the model supports content generation. */
    private boolean supportsContentGeneration = true;
    /** Whether the model supports batch embeddings. */
    private boolean supportsBatchEmbeddings = false;
    /** Whether the model supports simple embeddings. */
    private boolean supportsEmbeddings = false;
    /** Whether the model supports content caching. */
    private boolean supportsCachedContent = false;

    /** The shared, thread-safe HTTP Client instance. */
    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    /**
     * Constructs an OpenAiCompatibleModel with explicit metadata parameters.
     * @param provider the parent provider instance.
     * @param modelId the unique model identifier.
     * @param displayName the human-readable display name.
     */
    public OpenAiCompatibleModel(OpenAiChatCompletionsProvider provider, String modelId, String displayName) {
        this.provider = provider;
        this.modelId = modelId;
        this.displayName = displayName;
    }

    /**
     * Constructs a model instance from a JSON node returned by the /models
     * endpoint. Extracts ID, ownership, and creation timestamp for display and
     * versioning.
     *
     * @param provider The parent provider.
     * @param node The JSON node containing model metadata.
     */
    public OpenAiCompatibleModel(OpenAiChatCompletionsProvider provider, JsonNode node) {
        this(provider,
                node.path("id").asText(),
                String.format("%s (%s)", node.path("id").asText(), node.path("owned_by").asText("unknown")));

        long created = node.path("created").asLong(0);
        if (created > 0) {
            this.version = LocalDateTime.ofInstant(
                    Instant.ofEpochSecond(created),
                    ZoneId.systemDefault()
            ).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        }
    }

    /**
     * {@inheritDoc}
     * <p>Utilizes TokenizerUtils to perform offline, BPE token counting based on configured TokenizerType.</p>
     * @param text The text to count tokens for.
     * @return The token count, or 0 if the text is null or empty.
     */
    @Override public int countTokens(java.lang.String text) {
        return TokenizerUtils.countTokens(text, getTokenizerType());
    }


    /**
     * {@inheritDoc}
     * <p>
     * Serializes the base tool response to its flat JSON representation and adds the
     * token cost of any generated attachments (like images or logs) by delegating
     * to the generic binary tokenizer. This mirrors the exact OpenAI-compatible
     * wire-format, where attachments are sent as a separate message item.
     * </p>
     * @param toolResponse The tool response instance to count.
     * @return The precise, billing-identical token count.
     */
    @Override public int countTokens(AbstractToolResponse<?> toolResponse) {
        if (toolResponse == null) {
                    return 0;
                }
                try {
                    int total = countTokens(JacksonUtils.serialize(toolResponse));

                    // Replicate the OpenAI-compatible wire format: attachments are sent
                    // as a separate top-level message containing input_image or input_audio parts.
                    if (!toolResponse.getAttachments().isEmpty()) {
                        total += 20; // Message packaging overhead
                        total += countTokens("The following are multimodal attachments generated by the tool '" + toolResponse.getToolName() + "':");
                        for (ToolResponseAttachment att : toolResponse.getAttachments()) {
                            total += countTokens(att.getData(), att.getMimeType());
                        }
                    }
                    return total;
                } catch (Exception e) {
                    log.error("Failed to serialize OpenAI-compatible tool response for token counting", e);
                    throw new RuntimeException(e);
                }
    }
    /**
     * {@inheritDoc}
     * <p>
     * Serializes the tool call into a standard OpenAI function call JSON object
     * containing 'name' and 'arguments' properties, and counts its tokens using the active BPE encoding.
     * </p>
     * @param toolCall The tool call to count tokens for.
     * @return The total token count.
     */
    @Override public int countTokens(AbstractToolCall<?, ?> toolCall) {
        if (toolCall == null) {
            return 0;
        }
        try {
            Map<String, Object> map = new HashMap<>();
            map.put("name", toolCall.getToolName());
            map.put("arguments", JacksonUtils.serialize(toolCall.getEffectiveArgs()));
            return countTokens(JacksonUtils.serialize(map));
        } catch (Exception e) {
            return countTokens(toolCall.asText());
        }
    }
    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        return modelId;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedActions() {
        return List.of("chat/completions");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getRawDescription() {
        return "<html><b>Model ID:</b> " + modelId + "</html>";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsFunctionCalling() {
        return supportsFunctionCalling;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsContentGeneration() {
        return supportsContentGeneration;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsBatchEmbeddings() {
        return supportsBatchEmbeddings;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsEmbeddings() {
        return supportsEmbeddings;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsCachedContent() {
        return supportsCachedContent;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedResponseModalities() {
        String lowerId = modelId.toLowerCase();
        if (lowerId.contains("vision") || lowerId.contains("gpt-4o") || lowerId.contains("claude-3")) {
            return List.of("TEXT", "IMAGE");
        }
        return List.of("TEXT");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ServerTool> getAvailableServerTools() {
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ServerTool> getDefaultServerTools() {
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Float getDefaultTemperature() {
        return 0.7f;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer getDefaultTopK() {
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Float getDefaultTopP() {
        return 0.95f;
    }

    /**
     * Gets the relative endpoint URL for Chat Completion requests.
     * @return the sub-resource endpoint string.
     */
    protected String getEndpoint() {
        return "chat/completions";
    }

    /**
     * Instantiates an OpenAI-compatible message container.
     * @param agi the parent AGI session.
     * @return a new OpenAiCompatibleModelMessage.
     */
    public OpenAiCompatibleModelMessage createModelMessage(Agi agi) {
        return new OpenAiCompatibleMessage(agi, modelId);
    }

    @Override
    public Response generateContent(GenerationRequest request) {
        Agi agi = request.config().getAgi();
        ObjectNode payload = preparePayload(request, false);
        // --- Refined Partitioning ---
        // 1. Request Config JSON: Everything except history/input items
        ObjectNode configNode = payload.deepCopy();
        JsonNode messagesNode = configNode.remove("messages");
        JsonNode inputNode = configNode.remove("input");
        JsonNode promptNode = configNode.remove("prompt");
        // Always include consolidated SI in the Config partition for visibility
        if (!request.config().getSystemInstructions().isEmpty()) {
            configNode.put("consolidated_system_instructions", String.join("\n\n", request.config().getSystemInstructions()));
        }
        String configJson = configNode.toPrettyString();
        // 2. History JSON: Only the conversational items (User/Assistant/Tool)
        JsonNode rawHistoryNode = messagesNode != null ? messagesNode : (inputNode != null ? inputNode : promptNode);
        String historyJson = "[]";
        if (rawHistoryNode != null && rawHistoryNode.isArray()) {
            ArrayNode historyArray = SchemaProvider.OBJECT_MAPPER.createArrayNode();
            for (JsonNode m : rawHistoryNode) {
                // Strictly filter out system messages to avoid cluttering the History view
                // In V2, the role is inside the item: {"type": "message", "role": "system"}
                boolean isSystem = "system".equals(m.path("role").asText()) || "system".equals(m.path("type").asText());
                if (!isSystem) {
                    historyArray.add(m);
                }
            }
            historyJson = historyArray.toPrettyString();
        } else if (rawHistoryNode != null) {
            historyJson = rawHistoryNode.toPrettyString();
        }
        log.info("Executing OpenAI request to endpoint: {}", getEndpoint());
        try {
            HttpRequest httpRequest = provider.createRequestBuilder(getEndpoint()).header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(payload.toString())).build();
            try (HttpClient client = provider.createHttpClient()) {
                HttpResponse<String> httpResponse = client.send(httpRequest, HttpResponse.BodyHandlers.ofString());
                if (httpResponse.statusCode() != 200) {
                    String errorBody = httpResponse.body();
                    if (provider.isRetryable(httpResponse.statusCode(), errorBody)) {
                        provider.hokusPocus();
                        throw new RetryableApiException(provider.getCurrentKey(), "API error (" + httpResponse.statusCode() + "): " + errorBody, null);
                    }
                    throw new RuntimeException("API error (" + httpResponse.statusCode() + "): " + errorBody);
                }
                return new OpenAiCompatibleResponse(agi, modelId, httpResponse.body(), configJson, historyJson, this);
            }
        } catch (IOException | InterruptedException e) {
            log.error("Failed to execute OpenAI request", e);
            throw new RuntimeException(e);
        }
    }
    // --- Refined Partitioning ---
    // Include consolidated SI in Config view for better clarity
    // Filter out system roles from history JSON as they are now in the Config partition
        // --- Refined Partitioning ---
    // Include consolidated SI in Config view for better clarity
    // Filter out system roles from history JSON as they are now in the Config partition
        // Partition JSON: History vs Config (Gemini-style partitioning for status panel)
    
    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        Agi agi = request.config().getAgi();
        ObjectNode payload = preparePayload(request, true);
        String jsonPayload = payload.toString();
        JsonNode historyNode = payload.get("messages");
        if (historyNode == null) {
            historyNode = payload.get("input");
        }
        if (historyNode == null) {
            historyNode = payload.get("prompt");
        }
        String historyJson = historyNode != null ? historyNode.toString() : "[]";
        ObjectNode configNode = payload.deepCopy();
        configNode.remove("messages");
        configNode.remove("input");
        configNode.remove("prompt");
        String configJson = configNode.toString();
        log.info("Executing OpenAI streaming request to endpoint: {}", getEndpoint());
        try {
            HttpRequest httpRequest = provider.createRequestBuilder(getEndpoint()).header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(jsonPayload)).build();
            try (HttpClient client = provider.createHttpClient()) {
                List<OpenAiCompatibleModelMessage> targets = new ArrayList<>();
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
                        observer.onError(new RetryableApiException(provider.getCurrentKey(), "OpenAI Stream Error (" + response.statusCode() + "): " + errorMsg, null));
                    } else {
                        observer.onError(new RuntimeException("OpenAIModel Stream Error (" + response.statusCode() + "): " + errorMsg));
                    }
                    return;
                }
                try (Stream<String> lines = response.body()) {
                    Iterator<String> it = lines.iterator();
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
                            targets.forEach(OpenAiCompatibleModelMessage::flushToolCalls);
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
                            if (chunk.has("type") && chunk.get("type").asText().startsWith("response.")) {
                                if (!started.get()) {
                                    targets.add(createModelMessage(agi));
                                    observer.onStart((List) targets);
                                    started.set(true);
                                }
                                for (OpenAiCompatibleModelMessage target : targets) {
                                    target.updateFromNode(chunk, reasoningStyle, reasoningFieldName, reasoningTags);
                                }
                            } else if (chunk.has("choices")) {
                                JsonNode choices = chunk.get("choices");
                                if (choices != null && choices.isArray() && choices.size() > 0) {
                                    if (!started.get()) {
                                        for (int i = 0; i < choices.size(); i++) {
                                            targets.add(createModelMessage(agi));
                                        }
                                        observer.onStart((List) targets);
                                        started.set(true);
                                    }
                                    for (int i = 0; i < Math.min(choices.size(), targets.size()); i++) {
                                        JsonNode choice = choices.get(i);
                                        OpenAiCompatibleModelMessage target = targets.get(i);
                                        routeChunk(choice, target);
                                    }
                                }
                            }
                            for (OpenAiCompatibleModelMessage target : targets) {
                                target.appendRawJson(data);
                            }
                            OpenAiCompatibleResponse chunkResponse = new OpenAiCompatibleResponse(agi, modelId, data, configJson, historyJson, this);
                            if (chunkResponse.getUsageMetadata() != null && chunkResponse.getUsageMetadata().getTotalTokenCount() > 0) {
                                for (OpenAiCompatibleModelMessage target : targets) {
                                    target.setBilledPromptTokens(chunkResponse.getUsageMetadata().getPromptTokenCount());
                                    target.setBilledCompletionTokens(chunkResponse.getUsageMetadata().getCandidatesTokenCount());
                                }
                            }
                            observer.onNext(chunkResponse);
                        } catch (Exception e) {
                            log.error("Failed to parse OpenAI stream chunk: {}", data, e);
                        }
                    }
                }
                if (!targets.isEmpty()) {
                    boolean usageProvided = targets.stream()
                            .anyMatch(t-> t.getBilledCompletionTokens() > 0 || t.getBilledPromptTokens() > 0);
                    OpenAiCompatibleResponse finalResponse;
                    if (!usageProvided) {
                        log.info("No usage metadata provided by API, estimating tokens using {} tokenizer", getTokenizerType());
                        int estimatedPromptTokens = TokenizerUtils.countTokens(jsonPayload, getTokenizerType());
                        int totalCompletionTokens = 0;
                        for (OpenAiCompatibleModelMessage target : targets) {
                            StringBuilder contentBuilder = new StringBuilder();
                            for (AbstractPart part : target.getParts()) {
                                if (part instanceof ModelTextPart mtp) {
                                    contentBuilder.append(mtp.getText());
                                }
                            }
                            int estimatedCompletionTokens = TokenizerUtils.countTokens(contentBuilder.toString(), getTokenizerType());
                            totalCompletionTokens += estimatedCompletionTokens;
                            target.setBilledPromptTokens(estimatedPromptTokens);
                            target.setBilledCompletionTokens(estimatedCompletionTokens);
                        }
                        finalResponse = createResponseWithEstimatedUsage(agi, modelId, configJson, historyJson,
                                estimatedPromptTokens, totalCompletionTokens, getTokenizerType());
                    } else {
                        finalResponse = new OpenAiCompatibleResponse(agi, modelId,
                                targets.get(0).getRawJson() != null ? targets.get(0).getRawJson() : "{}",
                                configJson, historyJson, this);
                    }
                    for (OpenAiCompatibleModelMessage target : targets) {
                        target.setResponse(finalResponse);
                        target.setStreaming(false);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Failed to execute OpenAI stream", e);
            observer.onError(e);
        }
    }
    // 1. Handle Responses API Events (response.*)
    // 2. Handle standard Chat Completions Choices
    // Accumulate raw JSON chunk
    // Accumulate usage metadata if present in this chunk (like Gemini does)
    // OpenAI typically sends usage only in the final chunk
    // Set the final response on each target message (like Gemini does)
    // Check if we ever received usage from the API during streaming
    // Modal's GLM-5 returns usage: null in all streaming chunks
    // No usage provided by API - estimate tokens ourselves
    // Estimate prompt tokens from the payload
    // Estimate completion tokens from accumulated content per target
    // Get accumulated text content from parts
    // Create response with estimated usage metadata
    
    /**
     * Routes a streaming JSON choice chunk to the target message, extracting
     * thoughts or text content dynamically.
     * @param choice the JSON choice node from the chunk event.
     * @param target the target message accumulating the content.
     */
    private void routeChunk(JsonNode choice, OpenAiCompatibleModelMessage target) {
        JsonNode delta = choice.get("delta");
        if (delta == null) {
            delta = choice.get("message");
        }
        if (delta == null) {
            return;
        }

        // AUTODETECT: Check for reasoning_content field on first chunk if not explicitly configured
        if (reasoningStyle == OpenAiCompatibleReasoningStyle.NONE
                && delta.has("reasoning_content") && !delta.get("reasoning_content").isNull()) {
            log.info("Auto-detected FIELD reasoning style with field 'reasoning_content' for model {}", modelId);
            this.reasoningStyle = OpenAiCompatibleReasoningStyle.FIELD;
            this.reasoningFieldName = "reasoning_content";
        }

        if (reasoningStyle == OpenAiCompatibleReasoningStyle.NONE
                && delta.has("content") && !delta.get("content").isNull()
                && delta.get("content").asText().contains("<think>")) {
            log.info("Auto-detected TAGS reasoning style with '<think>' for model {}", modelId);
            this.reasoningStyle = OpenAiCompatibleReasoningStyle.TAGS;
            this.reasoningTags = List.of("<think>", "</think>");
        }

        if (reasoningStyle == OpenAiCompatibleReasoningStyle.FIELD && reasoningFieldName != null && delta.has(reasoningFieldName) && !delta.get(reasoningFieldName).isNull()) {
            target.appendThoughts(delta.get(reasoningFieldName).asText());
        }

        // 2. Handle Content (might contain TAGS style reasoning)
        if (delta.has("content") && !delta.get("content").isNull()) {
            String text = delta.get("content").asText();
            // Only append if text is not empty (avoid empty text parts before thoughts)
            if (!text.isEmpty()) {
                if (reasoningStyle == OpenAiCompatibleReasoningStyle.TAGS && reasoningTags != null && reasoningTags.size() >= 2) {
                    target.appendTaggedContent(text, reasoningTags.get(0), reasoningTags.get(1));
                } else {
                    target.appendContent(text);
                }
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

        funcNode.set("parameters", buildParametersNode(tool, false));

        return toolNode.toPrettyString();
    }

    /**
     * Builds the JSON Schema parameters node for a tool.
     * 
     * @param tool The tool to build parameters for.
     * @param strict Whether to enforce OpenAI 'strict' mode (requires additionalProperties: false).
     * @return The constructed ObjectNode.
     */
    protected ObjectNode buildParametersNode(AbstractTool<?, ?> tool, boolean strict) {
        ObjectNode paramsNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
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

        if (strict) {
            // OpenAI Strict mode requires additionalProperties: false at the root of the schema
            paramsNode.put("additionalProperties", false);
        }

        return paramsNode;
    }

    /**
     * Creates an OpenAiResponse with estimated usage metadata when the API
     * doesn't provide it. This is used for providers like Modal's GLM-5 that
     * return usage: null in streaming mode.
     *
     * @param agi The parent session.
     * @param modelId The model ID.
     * @param jsonPayload The request payload JSON.
     * @param historyJson The history JSON.
     * @param estimatedPromptTokens Estimated prompt tokens.
     * @param estimatedCompletionTokens Estimated completion tokens.
     * @param tokenizerType The tokenizer used for estimation.
     * @return A new OpenAiResponse with estimated usage metadata.
     */
    protected OpenAiCompatibleResponse createResponseWithEstimatedUsage(
            Agi agi, String modelId, String jsonPayload, String historyJson,
            int estimatedPromptTokens, int estimatedCompletionTokens,
            TokenizerType tokenizerType) {

        // Create estimated usage metadata with a descriptive rawJson
        String estimatedRawJson = String.format(
                "{\"estimated\":true,\"prompt_tokens\":%d,\"completion_tokens\":%d,\"total_tokens\":%d,\"note\":\"No usage JSON was provided or detected when parsing the response. This usage has been calculated by Anahata using the %s tokenizer.\"}",
                estimatedPromptTokens, estimatedCompletionTokens,
                estimatedPromptTokens + estimatedCompletionTokens,
                tokenizerType);

        ResponseUsageMetadata estimatedUsage = ResponseUsageMetadata.builder()
                .promptTokenCount(estimatedPromptTokens)
                .candidatesTokenCount(estimatedCompletionTokens)
                .totalTokenCount(estimatedPromptTokens + estimatedCompletionTokens)
                .rawJson(estimatedRawJson)
                .build();

        // Create a minimal response JSON with estimated usage
        String estimatedResponseJson = String.format(
                "{\"id\":\"estimated-%s\",\"object\":\"chat.completion\",\"model\":\"%s\",\"usage\":%s,\"choices\":[]}",
                UUID.randomUUID().toString().substring(0, 8),
                modelId,
                estimatedRawJson);

        return new OpenAiCompatibleResponse(agi, modelId, estimatedResponseJson, jsonPayload, historyJson, this, estimatedUsage);
    }

    /**
     * Prepares the final JSON request payload combining system instructions,
     * history, tools, and temperature.
     * @param request the current generation request.
     * @param stream whether the request is streaming.
     * @return the constructed JSON ObjectNode payload.
     */
    protected ObjectNode preparePayload(GenerationRequest request, boolean stream) {
        ObjectNode payload = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        payload.put("model", modelId);
        payload.put("stream", stream);
        if (stream) {
            payload.putObject("stream_options").put("include_usage", true);
        }
        ArrayNode messages = payload.putArray("messages");
        // 1. Consolidated System Instructions
        if (!request.config().getSystemInstructions().isEmpty()) {
            String si = String.join("\n\n", request.config().getSystemInstructions());
            messages.addObject().put("role", "system").put("content", si);
        }
        // 2. Inject Conversation History
        boolean includePruned = request.config().isIncludePruned();
        for (AbstractMessage msg : request.history()) {
            List<ObjectNode> translated = new OpenAiCompatibleResponseAdapter(msg, includePruned, getTokenizerType(),
                    reasoningStyle, reasoningTags).toOpenAi();
            messages.addAll(translated);
        }
        // 3. Local Tools
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
        // 4. Dynamic max_tokens calculation
        if (request.config().getMaxOutputTokens() != null) {
            int requestedMaxOutput = request.config().getMaxOutputTokens();
            int userThreshold = request.config().getAgi().getConfig().getTokenThreshold();
            int effectiveLimit = Math.min(maxInputTokens, userThreshold > 0 ? userThreshold : maxInputTokens);
            String payloadStr = payload.toString();
            int estimatedPayloadTokens = TokenizerUtils.countTokens(payloadStr, getTokenizerType());
            int availableForOutput = effectiveLimit - estimatedPayloadTokens;
            int actualMaxOutput = Math.min(requestedMaxOutput, availableForOutput);
            if (actualMaxOutput < requestedMaxOutput) {
                log.warn("Reducing max_tokens from {} to {} due to context limit", requestedMaxOutput, actualMaxOutput);
            }
            actualMaxOutput = Math.max(actualMaxOutput, 1);
            payload.put("max_tokens", actualMaxOutput);
        }
        enrichPayload(payload, request);
        return payload;
    }
    // 1. Consolidated System Instructions
    // 2. Inject Conversation History
    // 3. Local Tools
    // 4. Dynamic max_tokens calculation
        // Standard OpenAI include_usage for compatible providers
    // 1. Inject System Instructions if present in config
    // 2. Inject Conversation History
    // 3. Local Tools
    // 4. Dynamic max_tokens calculation to prevent context overflow
    // Calculate effective limit: min of model's max input and user's threshold
    // Estimate payload tokens using the tokenizer
    // Calculate available tokens for output
    // Final max_tokens is the minimum of requested and available
    // Ensure at least 1 token for output
        // 1. Inject System Instructions if present in config
    // 2. Inject Conversation History
    // 3. Local Tools
    // 4. Dynamic max_tokens calculation to prevent context overflow
    // Calculate effective limit: min of model's max input and user's threshold
    // Estimate payload tokens using the tokenizer
    // Calculate available tokens for output
    // Final max_tokens is the minimum of requested and available
    // Ensure at least 1 token for output
    
    /**
     * Hook for subclasses to add or modify payload parameters before sending
     * the request.
     *
     * @param payload The JSON payload being constructed.
     * @param request The generation request.
     */
    protected void enrichPayload(ObjectNode payload, GenerationRequest request) {
        // Default implementation does nothing.
    }

    /**
     * {@inheritDoc}
     * <p>
     * Calculates the exact, model-specific multimodal token count for OpenAI-compatible image data.
     * Delegates the header-only image dimension reading to the core {@link ImageMetadataUtils} utility,
     * and performs the OpenAI-specific high-detail scaling and tiling calculations.
     * </p>
     * @param mimeType The detected MIME type of the binary data (e.g. "image/png").
     * @param data The raw binary data of the file or attachment.
     * @return The precise, billing-identical multimodal token count.
     */
    @Override public int countTokens(byte[] data, String mimeType) {
        if (data == null || data.length == 0) {
                    return 0;
                }
                if (mimeType != null && mimeType.startsWith("image/")) {
                    ImageMetadata metadata = ImageMetadataUtils.readMetadata(data);
                    return ImageMetadataUtils.calculateOpenAiTileTokens(metadata);
                }
                return 85; // Fallback for non-image binary data
    }
}
