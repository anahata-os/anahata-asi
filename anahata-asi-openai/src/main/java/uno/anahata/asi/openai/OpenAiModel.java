/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolParameter;

/**
 * Native implementation for OpenAI models using the Responses API
 * (/v1/responses).
 * <p>
 * This implementation performs "Partitioned Construction" of the request
 * payload by assembling the full API body and then extracting the Identity
 * (Config) and Memory (History) JSON strings from it for UI visibility.
 * Supports built-in hosted tools like Web Search and Code Interpreter.</p>
 *
 * @author anahata
 */
@Slf4j
@SuppressWarnings("unchecked")
public class OpenAiModel extends AbstractModel {

    private static final ObjectMapper API_MAPPER = new ObjectMapper();
    private final OpenAiProvider provider;
    private final String modelId;
    private final String displayName;

    public OpenAiModel(OpenAiProvider provider, JsonNode node) {
        this.provider = provider;
        this.modelId = node.get("id").asText();
        this.displayName = node.path("name").asText(modelId);
    }

    @Override
    public OpenAiProvider getProvider() {
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
        return modelId;
    }

    @Override
    public String getVersion() {
        return null;
    }

    @Override
    public int getMaxInputTokens() {
        return 1050000;
    }

    @Override
    public int getMaxOutputTokens() {
        return 128000;
    }

    @Override
    public List<String> getSupportedActions() {
        return List.of("generateContent");
    }

    @Override
    public String getRawDescription() {
        return "<html><b>Model ID:</b> " + modelId + "<br><b>Provider:</b> OpenAI Responses API</html>";
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
        return true;
    }

    @Override
    public List<String> getSupportedResponseModalities() {
        return List.of("TEXT", "IMAGE", "AUDIO");
    }

    @Override
    public List<ServerTool> getAvailableServerTools() {
        List<ServerTool> tools = new ArrayList<>();
        tools.add(new ServerTool("web_search", "Web Search", "Search the web using OpenAI's built-in tool."));
        tools.add(new ServerTool("code_interpreter", "Code Interpreter", "Execute Python code in a secure sandbox."));
        return tools;
    }

    @Override
    public List<ServerTool> getDefaultServerTools() {
        return getAvailableServerTools().stream()
                .filter(st -> "web_search".equals(st.getId()))
                .collect(java.util.stream.Collectors.toList());
    }

    @Override
    public Float getDefaultTemperature() {
        return null;
    }

    @Override
    public Integer getDefaultTopK() {
        return null;
    }

    @Override
    public Float getDefaultTopP() {
        return null;
    }

    /**
     * Record holding the three distinct partitions of a request payload.
     */
    public record PreparedPayload(String fullPayload, String configJson, String historyJson) {

    }

    @SneakyThrows
    private PreparedPayload preparePayload(GenerationRequest request, boolean stream) {
        ObjectNode root = API_MAPPER.createObjectNode();
        root.put("model", modelId);
        root.put("store", false); // Stateless ASI mode
        if (stream) root.put("stream", true);

        ArrayNode include = root.putArray("include");
        include.add("reasoning.encrypted_content");

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
                    reasoning.put("generate_summary", "auto");
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
            Map<uno.anahata.asi.agi.tool.spi.AbstractToolkit, List<AbstractTool>> grouped = (Map) request.config().getLocalTools().stream()
                    .collect(java.util.stream.Collectors.groupingBy(AbstractTool::getToolkit));

            for (var entry : grouped.entrySet()) {
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

        HttpRequest httpRequest = HttpRequest.newBuilder()
                .uri(URI.create("https://api.openai.com/v1/responses"))
                .header("Authorization", "Bearer " + apiKey)
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
            HttpRequest httpRequest = HttpRequest.newBuilder()
                    .uri(URI.create("https://api.openai.com/v1/responses"))
                    .header("Authorization", "Bearer " + provider.getCurrentKey())
                    .header("Content-Type", "application/json")
                    .header("Accept", "text/event-stream")
                    .POST(HttpRequest.BodyPublishers.ofString(prepared.fullPayload()))
                    .build();
                    
            java.net.http.HttpClient client = provider.getHttpClient(); {
                OpenAiModelMessage targetMessage = new OpenAiModelMessage(agi, getModelId());
                List<OpenAiModelMessage> targets = List.of(targetMessage);
                java.util.concurrent.atomic.AtomicBoolean started = new java.util.concurrent.atomic.AtomicBoolean(false);
                
                HttpResponse<java.util.stream.Stream<String>> response = client.send(httpRequest, HttpResponse.BodyHandlers.ofLines());
                if (response.statusCode() != 200) {
                    String errorMsg = "No error body";
                    try (java.util.stream.Stream<String> bodyStream = response.body()) {
                        errorMsg = bodyStream.collect(java.util.stream.Collectors.joining("\n"));
                    }
                    if (provider.isRetryable(response.statusCode(), errorMsg)) {
                        provider.hokusPocus();
                        observer.onError(new RetryableApiException(provider.getCurrentKey(), "Stream Error (" + response.statusCode() + "): " + errorMsg, null));
                    } else {
                        observer.onError(new RuntimeException("Stream Error (" + response.statusCode() + "): " + errorMsg));
                    }
                    return;
                }
                
                try (java.util.stream.Stream<String> lines = response.body()) {
                    java.util.Iterator<String> it = lines.iterator();
                    
                    while (it.hasNext()) {
                        String line = it.next();
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
                                JsonNode chunk = uno.anahata.asi.internal.JacksonUtils.parse(data, JsonNode.class);
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
}
