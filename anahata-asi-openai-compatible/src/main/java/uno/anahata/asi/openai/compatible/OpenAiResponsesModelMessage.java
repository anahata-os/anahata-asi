/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Implementation of {@link OpenAiModelMessage} for the newer OpenAI Responses
 * API (/v1/responses). Handles the "Items" architecture (message, reasoning,
 * tool_call) and provides persistent reasoning support.
 *
 * @author anahata
 */
@Slf4j
public class OpenAiResponsesModelMessage extends OpenAiModelMessage {

    /**
     * Stores the raw items from the Responses API to support persistent
     * reasoning in future turns.
     */
    @Getter
    private final List<JsonNode> persistentItems = new ArrayList<>();
    private final java.util.Set<String> createdItemIds = new java.util.HashSet<>();

    private transient Map<String, StringBuilder> callArgsBuffers;
    private transient Map<String, String> callIds; // Stores the 'call_id' (call_...)
    private transient Map<String, String> callNames;

    public OpenAiResponsesModelMessage(Agi agi, String modelId) {
        super(agi, modelId);
        this.callArgsBuffers = new HashMap<>();
        this.callIds = new HashMap<>();
        this.callNames = new HashMap<>();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Handles streaming events (response.*), authorative response objects, and
     * static items.</p>
     */
    @Override
    public void updateFromNode(JsonNode node, ReasoningStyle reasoningStyle, String reasoningFieldName, List<String> reasoningTags) {
        String type = node.path("type").asText();
        String object = node.path("object").asText();
        // 1. Handle Response Object (Full or from completed event)
        if ("response".equals(object)) {
            JsonNode output = node.get("output");
            if (output != null && output.isArray()) {
                persistentItems.clear();
                for (JsonNode item : output) {
                    updateFromNode(item, reasoningStyle, reasoningFieldName, reasoningTags);
                }
            }
            JsonNode usage = node.get("usage");
            if (usage != null && getResponse() != null) {
                setBilledTokenCount(usage.path("completion_tokens").asInt());
            }
            return;
        }
        // 2. Handle Responses API Streaming Events (response.*)
        if (type.startsWith("response.")) {
            if ("response.output_text.delta".equals(type)) {
                appendContent(node.path("delta").asText());
            } else if ("response.refusal.delta".equals(type)) {
                appendThoughts("[Refusal] " + node.path("delta").asText());
            } else if ("response.reasoning_summary_text.delta".equals(type)) {
                appendThoughts(node.path("delta").asText());
            } else if ("response.reasoning_text.delta".equals(type)) {
                appendThoughts(node.path("delta").asText());
            } else if ("response.output_item.added".equals(type)) {
                JsonNode item = node.get("item");
                if (item != null && "function_call".equals(item.path("type").asText())) {
                    String itemId = item.path("id").asText();
                    callIds.put(itemId, item.path("call_id").asText());
                    String ns = item.path("namespace").asText(null);
                    String name = item.path("name").asText();
                    String qualifiedName = (ns != null) ? ns + "." + name : name;
                    callNames.put(itemId, qualifiedName);
                }
            } else if ("response.function_call_arguments.delta".equals(type)) {
                String itemId = node.path("item_id").asText();
                String delta = node.path("delta").asText();
                callArgsBuffers.computeIfAbsent(itemId, k-> new StringBuilder()).append(delta);
            } else if ("response.output_item.done".equals(type)) {
                JsonNode item = node.get("item");
                if (item != null) {
                    addItemIfMissing(item);
                    if ("function_call".equals(item.path("type").asText())) {
                        String itemId = item.path("id").asText();
                        callIds.put(itemId, item.path("call_id").asText());
                        String ns = item.path("namespace").asText(null);
                        String name = item.path("name").asText();
                        String qualifiedName = (ns != null) ? ns + "." + name : name;
                        callNames.put(itemId, qualifiedName);
                        if (item.has("arguments")) {
                            callArgsBuffers.put(itemId, new StringBuilder(item.get("arguments").asText()));
                        }
                        createToolCallImmediately(itemId);
                    }
                }
            } else if ("response.done".equals(type)) {
                JsonNode resp = node.get("response");
                if (resp != null) {
                    if ("completed".equals(resp.path("status").asText())) {
                        setFinishReason(FinishReason.STOP);
                    } else if ("incomplete".equals(resp.path("status").asText())) {
                        setFinishReasonFromOpenAi(resp.path("incomplete_details").path("reason").asText());
                    }
                    // IMPORTANT: We do NOT append the full 'done' response object to avoid echoed instructions/tools in message state
                    updateFromNode(resp, reasoningStyle, reasoningFieldName, reasoningTags);
                }
                flushToolCalls();
            }
            return;
        }
        // 3. Handle Static Items
        if ("message".equals(type) || "reasoning".equals(type) || "function_call".equals(type)) {
            addItemIfMissing(node);
        }
        if ("message".equals(type)) {
            JsonNode content = node.get("content");
            if (content != null && content.isArray()) {
                for (JsonNode part : content) {
                    String partType = part.path("type").asText();
                    if ("output_text".equals(partType)) {
                        appendContent(part.path("text").asText());
                    } else if ("refusal".equals(partType)) {
                        appendThoughts("[Refusal] " + part.path("refusal").asText());
                    }
                }
            }
        } else if ("reasoning".equals(type)) {
            JsonNode summary = node.get("summary");
            if (summary != null && summary.isArray()) {
                for (JsonNode part : summary) {
                    if ("summary_text".equals(part.path("type").asText())) {
                        appendThoughts(part.path("text").asText());
                    }
                }
            }
        } else if ("function_call".equals(type)) {
            String itemId = node.path("id").asText();
            String callId = node.path("call_id").asText();
            String ns = node.path("namespace").asText(null);
            String name = node.path("name").asText();
            String qualifiedName = (ns != null) ? ns + "." + name : name;
            String args = node.path("arguments").asText();
            if (callId != null && name != null) {
                callIds.put(itemId, callId);
                callNames.put(itemId, qualifiedName);
                callArgsBuffers.put(itemId, new StringBuilder(args));
            }
        }
        if (node.has("status") && "completed".equals(node.path("status").asText())) {
            setFinishReason(FinishReason.STOP);
            flushToolCalls();
        }
    }

    private void addItemIfMissing(JsonNode item) {
        String id = item.path("id").asText(null);
        if (id != null) {
            boolean exists = persistentItems.stream()
                    .anyMatch(n -> id.equals(n.path("id").asText()));
            if (!exists) {
                persistentItems.add(item);
            }
        } else {
            persistentItems.add(item);
        }
    }

    @Override
    public void updateToolCall(JsonNode callNode) {
        if (callArgsBuffers == null) callArgsBuffers = new HashMap<>();
        if (callIds == null) callIds = new HashMap<>();
        if (callNames == null) callNames = new HashMap<>();
        String itemId = callNode.path("id").asText(null);
        String callId = callNode.path("call_id").asText(null);
        // Strictly V2 Responses API: flat structure.
        String name = callNode.path("name").asText(null);
        String ns = callNode.path("namespace").asText(null);
        String qualifiedName = (ns != null) ? ns + "." + name : name;
        String argsFragment = callNode.path("arguments").asText("");
        if (itemId != null) {
            if (callId != null) callIds.put(itemId, callId);
            if (name != null) callNames.put(itemId, qualifiedName);
            if (!argsFragment.isEmpty()) {
                callArgsBuffers.computeIfAbsent(itemId, k-> new StringBuilder()).append(argsFragment);
            }
        }
    }
    // Removed V1 compatibility checks. In V2, the structure is flat.
        // Support both V1 (nested) and V2 (flat) structures
    //this is totally wrong, we have a different ModelMessage implementation for this.
    
    private void createToolCallImmediately(String itemId) {
        if (createdItemIds.contains(itemId)) {
            return;
        }

        String callId = callIds.get(itemId);
        String name = callNames.get(itemId);
        StringBuilder buffer = callArgsBuffers.get(itemId);

        if (callId != null && name != null && buffer != null) {
            String fullJson = buffer.toString();
            if (!fullJson.isEmpty()) {
                Map<String, Object> args = JacksonUtils.parse(fullJson, Map.class);
                getAgi().getToolManager().createToolCall(this, callId, name, args);
                createdItemIds.add(itemId);
            }
        }
    }

    @Override
    public void flushToolCalls() {
        if (callArgsBuffers == null) {
            return;
        }
        for (String itemId : callArgsBuffers.keySet()) {
            createToolCallImmediately(itemId);
        }
        callArgsBuffers = new HashMap<>();
        callIds = new HashMap<>();
        callNames = new HashMap<>();
    }
}
