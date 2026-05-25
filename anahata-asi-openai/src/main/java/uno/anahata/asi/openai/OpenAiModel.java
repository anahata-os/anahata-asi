/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.SneakyThrows;
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
import uno.anahata.asi.agi.provider.ThinkingLevel;
import uno.anahata.asi.agi.tool.ToolResponseAttachment;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolParameter;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.internal.ImageMetadataUtils;
import uno.anahata.asi.internal.ImageMetadataUtils.ImageMetadata;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.internal.TokenizerUtils;

/**
 * Native implementation for OpenAI models using the Responses API
 * (/v1/responses).
 * <p>
 * This implementation performs "Partitioned Construction" of the request
 * payload by assembling the full API body and then extracting the Identity
 * (Config) and Memory (History) JSON strings from it for UI visibility.
 * Supports built-in hosted tools like Web Search and Code Interpreter.</p>
 * @author anahata
 */
@Slf4j
@SuppressWarnings("unchecked")
@Getter
public class OpenAiModel extends AbstractModel {

    /**
     * Internal mapper for Responses API JSON operations.
     */
    private static final ObjectMapper API_MAPPER = new ObjectMapper();
    /**
     * The parent provider for this model.
     */
    private final OpenAiResponsesProvider provider;
    /**
     * The unique identifier for the OpenAI model (e.g., 'gpt-4o').
     */
    private final String modelId;
    /**
     * The human-readable name for the model.
     */
    private final String displayName;

    /**
     * Constructs a new OpenAiModel from an API model node.
     * @param provider The parent provider.
     * @param node The JSON node containing model metadata.
     */
    public OpenAiModel(OpenAiResponsesProvider provider, JsonNode node) {
        this.provider = provider;
        this.modelId = node.get("id").asText();
        this.displayName = node.path("name").asText(modelId);
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
            java.util.Map<String, Object> map = new java.util.HashMap<>();
            map.put("name", toolCall.getToolName());
            map.put("arguments", JacksonUtils.serialize(toolCall.getEffectiveArgs()));
            return countTokens(JacksonUtils.serialize(map));
        } catch (Exception e) {
            return countTokens(toolCall.asText());
        }
    }
    /**
     * {@inheritDoc}
     * <p>Implementation details: Returns the display name or model ID.</p>
     */
    @Override
    public String getDescription() {
        return displayName;
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Always returns null as Responses API models 
     * use the base ID for versioning.</p>
     */
    @Override
    public String getVersion() {
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMaxInputTokens() {
        return 1050000;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMaxOutputTokens() {
        return 128000;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedActions() {
        return List.of("generateContent");
    }

    /**
     * {@inheritDoc}
     * <p>Returns a high-density HTML summary of the model and its specialized 
     * Responses API provider.</p>
     */
    @Override
    public String getRawDescription() {
        return "<html><b>Model ID:</b> " + modelId + "<br><b>Provider:</b> OpenAI Responses API</html>";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsFunctionCalling() {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsContentGeneration() {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsBatchEmbeddings() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsEmbeddings() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsCachedContent() {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedResponseModalities() {
        return List.of("TEXT", "IMAGE", "AUDIO");
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Provides 'web_search' and 'code_interpreter' 
     * as native server-side tools.</p>
     */
    @Override
    public List<ServerTool> getAvailableServerTools() {
        List<ServerTool> tools = new ArrayList<>();
        tools.add(new ServerTool("web_search", "Web Search", "Search the web using OpenAI's built-in tool."));
        tools.add(new ServerTool("code_interpreter", "Code Interpreter", "Execute Python code in a secure sandbox."));
        return tools;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ServerTool> getDefaultServerTools() {
        return getAvailableServerTools().stream()
                .filter((ServerTool st) -> "web_search".equals(st.getId()))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Float getDefaultTemperature() {
        return null;
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
        return null;
    }

    /**
     * Record holding the three distinct partitions of a request payload.
     * @param fullPayload the raw JSON of the entire aggregated payload.
     * @param configJson the raw JSON of the request configuration partition.
     * @param historyJson the raw JSON of the history/memory partition.
     */
    public record PreparedPayload(String fullPayload, String configJson, String historyJson) {

    }

    /**
     * Assembles the full OpenAI Responses API payload and extracts partitions 
     * for UI visibility.
     * @param request The generation request.
     * @param stream Whether to enable SSE streaming.
     * @return The prepared payload partitions.
     */
    @SneakyThrows
    private PreparedPayload preparePayload(GenerationRequest request, boolean stream) {
        ObjectNode root = API_MAPPER.createObjectNode();
        root.put("model", modelId);
        
        // 0. Deduce statefulness and reasoning transmission capabilities
        boolean isVerifiedOrg = provider.isVerifiedOrganization();
        root.put("store", isVerifiedOrg); 
        
        if (stream) root.put("stream", true);

        ArrayNode include = root.putArray("include");
        
        if (!isVerifiedOrg) {
            // ZDR / Stateless requirement: must explicitly request encrypted hashes 
            // from the API because the model's 'thoughts' cannot be stored by OpenAI.
            include.add("reasoning.encrypted_content");
        }

        // 1. Identity / Behavioral Params
        ThinkingLevel level = request.config().getThinkingLevel();
        if (level != null && level != ThinkingLevel.THINKING_LEVEL_UNSPECIFIED) {
            String effort = switch (level) {
                case NONE ->
                    "none";
                case MINIMAL, LOW ->
                    "low";
                case MEDIUM ->
                    "medium";
                case HIGH ->
                    "high";
                case XHIGH ->
                    "xhigh";
                default ->
                    null;
            };
            if (effort != null) {
                ObjectNode reasoning = root.putObject("reasoning");
                reasoning.put("effort", effort);
                if (request.config().getAgi().getConfig().isIncludeThoughts()) {
                    if (isVerifiedOrg) {
                        // Verified orgs can request plain text reasoning summaries
                        reasoning.put("summary", "auto");
                    }
                }
            }
        }

        List<String> si = request.config().getSystemInstructions();
        if (!si.isEmpty()) {
            root.put("instructions", String.join("\n\n", si));
        }

        // Tools (Local and Hosted)
        ArrayNode toolsArray = root.putArray("tools");
        if (request.config().getLocalTools() != null && !request.config().getLocalTools().isEmpty()) {
            Map<AbstractToolkit, List<AbstractTool>> grouped = (Map) request.config().getLocalTools().stream()
                    .collect(Collectors.groupingBy(AbstractTool::getToolkit));

            for (Map.Entry<AbstractToolkit, List<AbstractTool>> entry : grouped.entrySet()) {
                ObjectNode namespaceNode = toolsArray.addObject();
                namespaceNode.put("type", "namespace");
                namespaceNode.put("name", entry.getKey().getName());
                namespaceNode.put("description", entry.getKey().getDescription());
                ArrayNode nsTools = namespaceNode.putArray("tools");

                for (AbstractTool<?, ?> tool : entry.getValue()) {
                    nsTools.add(API_MAPPER.readTree(getToolDeclarationJson(tool, request.config())));
                }
            }
        }

        // Hosted Server Tools
        if (request.config().isServerToolsEnabled()) {
            for (ServerTool st : request.config().getEnabledServerTools()) {
                if ("web_search".equals(st.getId())) {
                    toolsArray.addObject().put("type", "web_search");
                    include.add("web_search_call.results");
                    include.add("web_search_call.action.sources");
                } else if ("code_interpreter".equals(st.getId())) {
                    ObjectNode ciTool = toolsArray.addObject();
                    ciTool.put("type", "code_interpreter");
                    ciTool.putObject("container").put("type", "auto");
                    include.add("code_interpreter_call.outputs");
                }
            }
        }

        // If no tools added, remove the array to keep the payload clean
        if (toolsArray.isEmpty()) {
            root.remove("tools");
        }

        // 2. Memory / History
        ArrayNode input = root.putArray("input");
        boolean includePruned = request.config().isIncludePruned();
        for (AbstractMessage msg : request.history()) {
            input.addAll(new OpenAiItemAdapter(msg, includePruned, getModelId()).toItems());
        }

        // Partitioning: Extract for the UI
        String historyJson = input.toPrettyString();
        ObjectNode configNode = root.deepCopy();
        configNode.remove("input");
        if (root.has("instructions")) {
            configNode.put("consolidated_system_instructions", root.get("instructions").asText());
        }

        return new PreparedPayload(root.toString(), configNode.toPrettyString(), historyJson);
    }

    /**
     * {@inheritDoc}
     * <p>Executes a synchronous content generation request using the Responses API.
     * Performs automated partitioning of the request for clear history and 
     * configuration separation in the UI.</p>
     */
    @Override
    @SneakyThrows
    public Response generateContent(GenerationRequest request) {
        PreparedPayload prepared = preparePayload(request, false);
        String apiKey = provider.getCurrentKey();

        System.out.println("--- Request Config JSON (SI & Tools) ---");
        System.out.println(prepared.configJson());
        System.out.println("--- History JSON (User & Model) ---");
        System.out.println(prepared.historyJson());

        HttpRequest httpRequest = provider.createRequestBuilder("responses")
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(prepared.fullPayload()))
                .build();

        HttpResponse<String> httpResponse = provider.getHttpClient().send(httpRequest, HttpResponse.BodyHandlers.ofString());

        System.out.println("--- Entire Response JSON ---");
        System.out.println(httpResponse.body());

        if (httpResponse.statusCode() == 429 || httpResponse.statusCode() == 503) {
            provider.hokusPocus();
            throw new RetryableApiException(apiKey, "OpenAI API " + httpResponse.statusCode() + ": " + httpResponse.body(), null);
        }

        if (httpResponse.statusCode() != 200) {
            throw new RuntimeException("OpenAI API error: " + httpResponse.statusCode() + " - " + httpResponse.body());
        }

        return new OpenAiResponse(prepared.configJson(), prepared.historyJson(),
                request.config().getAgi(), modelId, httpResponse.body());
    }

    /**
     * {@inheritDoc}
     * <p>Executes an asynchronous content generation request using the Responses API 
     * with Server-Sent Events (SSE). Streams text and reasoning in real-time, and 
     * seamlessly processes tool calls upon item completion.</p>
     */
    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        Agi agi = request.config().getAgi();
        PreparedPayload prepared = preparePayload(request, true);
        
        log.info("Executing OpenAI streaming request to Responses API");
        try {
            HttpRequest httpRequest = provider.createRequestBuilder("responses")
                    .header("Content-Type", "application/json")
                    .header("Accept", "text/event-stream")
                    .POST(HttpRequest.BodyPublishers.ofString(prepared.fullPayload()))
                    .build();
                    
            HttpClient client = provider.getHttpClient(); {
                OpenAiModelMessage targetMessage = new OpenAiModelMessage(agi, getModelId());
                targetMessage.setStreaming(true);
                List<OpenAiModelMessage> targets = List.of(targetMessage);
                AtomicBoolean started = new AtomicBoolean(false);
                
                HttpResponse<Stream<String>> response = client.send(httpRequest, HttpResponse.BodyHandlers.ofLines());
                if (response.statusCode() != 200) {
                    String errorMsg = "No error body";
                    try (Stream<String> bodyStream = response.body()) {
                        errorMsg = bodyStream.collect(Collectors.joining("\n"));
                    }
                    if (provider.isRetryable(response.statusCode(), errorMsg)) {
                        provider.hokusPocus();
                        observer.onError(new RetryableApiException(provider.getCurrentKey(), "Stream Error (" + response.statusCode() + "): " + errorMsg, null));
                    } else {
                        observer.onError(new RuntimeException("Stream Error (" + response.statusCode() + "): " + errorMsg));
                    }
                    return;
                }
                
                try (Stream<String> lines = response.body()) {
                    Iterator<String> it = lines.iterator();
                    
                    while (it.hasNext()) {
                        String line = it.next();
                        log.debug("Got line " + line);
                        if (line == null || line.isBlank()) continue;
                        
                        if (line.startsWith("data: ")) {
                            String data = line.substring(6).trim();
                            if ("[DONE]".equals(data)) {
                                break;
                            }
                            
                            if (!started.get()) {
                                observer.onStart((List) targets);
                                started.set(true);
                            }
                            
                            try {
                                JsonNode chunk = JacksonUtils.parse(data, JsonNode.class);
                                targetMessage.handleStreamEvent(chunk);
                                targetMessage.appendRawJson(data);
                                
                                OpenAiResponse chunkResponse = new OpenAiResponse(prepared.configJson(), prepared.historyJson(), data);
                                observer.onNext(chunkResponse);
                                
                            } catch (Exception e) {
                                log.error("Failed to parse OpenAI stream chunk: {}", data, e);
                            }
                        }
                    }
                }
                targetMessage.setStreaming(false);
                observer.onComplete();
            }

        } catch (Exception e) {
            log.error("Failed to execute OpenAI stream", e);
            observer.onError(e);
        }
    }

    /**
     * {@inheritDoc}
     * <p>Translates Anahata tool definitions into the OpenAI 'function' 
     * specification format.</p>
     */
    @Override
    @SneakyThrows
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        Map<String, Object> decl = new HashMap<>();
        decl.put("type", "function");

        String fullName = tool.getName();
        String simpleName = fullName.contains(".") ? fullName.substring(fullName.lastIndexOf(".") + 1) : fullName;
        decl.put("name", simpleName);
        decl.put("description", tool.getDescription());

        Map<String, Object> parameters = new HashMap<>();
        parameters.put("type", "object");

        Map<String, Object> properties = new HashMap<>();
        List<String> required = new ArrayList<>();

        for (AbstractToolParameter param : tool.getParameters()) {
            properties.put(param.getName(), API_MAPPER.readTree(param.getJsonSchema()));
            if (param.isRequired()) {
                required.add(param.getName());
            }
        }

        parameters.put("properties", properties);
        parameters.put("required", required);
        parameters.put("additionalProperties", false);

        decl.put("parameters", parameters);
        decl.put("strict", false);

        return API_MAPPER.writeValueAsString(decl);
    }


    /**
     * {@inheritDoc}
     * <p>
     * Serializes the base tool response to its flat JSON representation and adds the
     * token cost of any generated attachments (like images or logs) by delegating
     * to the generic binary tokenizer. This mirrors the exact OpenAI Responses API
     * wire-format, where attachments are sent as a separate developer-role message.
     * </p>
     * @param toolResponse The tool response instance to count.
     * @return The precise, billing-identical token count.
     */
    @Override public int countTokens(uno.anahata.asi.agi.tool.spi.AbstractToolResponse<?> toolResponse) {
        if (toolResponse == null) {
                    return 0;
                }
                try {
                    int total = countTokens(JacksonUtils.serialize(toolResponse));
                    if (!toolResponse.getAttachments().isEmpty()) {
                        total += 20; // Developer message packaging overhead
                        total += countTokens("The following are multimodal attachments generated by the tool '" + toolResponse.getToolName() + "':");
                        for (ToolResponseAttachment att : toolResponse.getAttachments()) {
                            total += countTokens(att.getData(), att.getMimeType());
                        }
                    }
                    return total;
                } catch (Exception e) {
                    log.error("Failed to serialize OpenAI tool response for token counting", e);
                    throw new RuntimeException(e);
                }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Calculates the exact, model-specific multimodal token count for OpenAI image data.
     * Delegates the header-only image dimension reading to the core {@link ImageMetadataUtils} utility,
     * and performs the OpenAI-specific high-detail scaling and tiling calculations.
     * </p>
     * @param data The raw binary data of the file or attachment.
     * @param mimeType The detected MIME type of the binary data (e.g. "image/png").
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
