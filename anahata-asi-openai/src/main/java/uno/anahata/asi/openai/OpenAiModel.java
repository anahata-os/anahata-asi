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
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.RetryableApiException;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolParameter;

/**
 * Native implementation for OpenAI models using the Responses API (/v1/responses).
 * 
 * <p>This implementation performs "Partitioned Construction" of the request payload,
 * explicitly building the Identity (Config) and Memory (History) JSON strings 
 * during assembly to ensure perfect synchronization with the UI raw JSON viewers.</p>
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
        return Collections.emptyList();
    }

    @Override
    public List<ServerTool> getDefaultServerTools() {
        return Collections.emptyList();
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
    public record PreparedPayload(String fullPayload, String configJson, String historyJson) {}

    private PreparedPayload preparePayload(GenerationRequest request) throws Exception {
        ObjectNode root = API_MAPPER.createObjectNode();
        root.put("model", modelId);
        
        // 1. Config Partition (Identity: SI and Tools)
        ObjectNode configNode = API_MAPPER.createObjectNode();
        configNode.put("model", modelId);
        
        List<String> si = request.config().getSystemInstructions();
        if (!si.isEmpty()) {
            String consolidatedSi = String.join("\n\n", si);
            root.put("instructions", consolidatedSi);
            configNode.put("consolidated_system_instructions", consolidatedSi);
        }

        // Tools
        if (request.config().getLocalTools() != null && !request.config().getLocalTools().isEmpty()) {
            ArrayNode toolsArray = root.putArray("tools");
            ArrayNode configTools = configNode.putArray("tools");
            
            // Group tools by toolkit to create namespaces
            Map<uno.anahata.asi.agi.tool.spi.AbstractToolkit, List<AbstractTool>> grouped = (Map) request.config().getLocalTools().stream()
                    .collect(java.util.stream.Collectors.groupingBy(AbstractTool::getToolkit));

            for (var entry : grouped.entrySet()) {
                var toolkit = entry.getKey();
                var tools = entry.getValue();

                ObjectNode namespaceNode = API_MAPPER.createObjectNode();
                namespaceNode.put("type", "namespace");
                namespaceNode.put("name", toolkit.getName());
                namespaceNode.put("description", toolkit.getDescription());
                ArrayNode nsTools = namespaceNode.putArray("tools");

                for (AbstractTool<?, ?> tool : tools) {
                    String decl = getToolDeclarationJson(tool, request.config());
                    nsTools.add(API_MAPPER.readTree(decl));
                }
                toolsArray.add(namespaceNode);
                configTools.add(namespaceNode);
            }
        }
        
        // 2. History Partition (Memory: User/Model items)
        ArrayNode input = root.putArray("input");
        ArrayNode historyNode = API_MAPPER.createArrayNode();
        
        boolean includePruned = request.config().isIncludePruned();
        for (AbstractMessage msg : request.history()) {
            List<ObjectNode> items = new OpenAiItemAdapter(msg, includePruned, getModelId()).toItems();
            for (ObjectNode item : items) {
                input.add(item);
                historyNode.add(item);
            }
        }

        return new PreparedPayload(
            root.toString(),
            configNode.toPrettyString(),
            historyNode.toPrettyString()
        );
    }

    @Override
    public Response generateContent(GenerationRequest request) {
        try {
            PreparedPayload prepared = preparePayload(request);
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
            
        } catch (RetryableApiException rae) {
            throw rae;
        } catch (Exception e) {
            log.error("Generation failed", e);
            throw new RuntimeException(e);
        }
    }

    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        throw new UnsupportedOperationException("Streaming not yet implemented in clean-room.");
    }

    @Override
    @SneakyThrows
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        Map<String, Object> decl = new HashMap<>();
        decl.put("type", "function");
        // Use simple name since it is already scoped within a namespace item
        
        // Use simple name since it is already scoped within a namespace item
        String fullName = tool.getName();
        String simpleName = fullName.contains(".") ? fullName.substring(fullName.lastIndexOf(".") + 1) : fullName;
        decl.put("name", simpleName);
        decl.put("description", tool.getDescription());
        
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("type", "object");
        
        Map<String, Object> properties = new HashMap<>();
        List<String> required = new ArrayList<>();
        
        for (AbstractToolParameter param : tool.getParameters()) {
            JsonNode pSchema = API_MAPPER.readTree(param.getJsonSchema());
            properties.put(param.getName(), pSchema);
            if (param.isRequired()) {
                required.add(param.getName());
            }
        }
        
        parameters.put("properties", properties);
        parameters.put("required", required);
        parameters.put("additionalProperties", false);
        
        decl.put("parameters", parameters);
        decl.put("strict", false);
        
        try {
            return API_MAPPER.writeValueAsString(decl);
        } catch (Exception e) {
            return "{}";
        }
    }
}
