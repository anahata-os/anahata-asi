/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

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

        // 1. Handle Response Object (Root or inside Event)
        if ("response".equals(object)) {
            JsonNode output = node.get("output");
            if (output != null && output.isArray()) {
                // IMPORTANT: When receiving a full response object, we clear existing persistent items
                // to avoid duplication if we were previously streaming.
                persistentItems.clear();
                for (JsonNode item : output) {
                    updateFromNode(item, reasoningStyle, reasoningFieldName, reasoningTags);
                }
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
                callArgsBuffers.computeIfAbsent(itemId, k -> new StringBuilder()).append(delta);
            } else if ("response.output_item.done".equals(type)) {
                JsonNode item = node.get("item");
                if (item != null) {
                    addItemIfMissing(item);
                    if ("function_call".equals(item.path("type").asText())) {
                        String itemId = item.path("id").asText();
                        callIds.put(itemId, item.path("call_id").asText());
                        callNames.put(itemId, item.path("name").asText());
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
                }
                flushToolCalls();
            } else if ("response.completed".equals(type)) {
                JsonNode resp = node.get("response");
                if (resp != null) {
                    updateFromNode(resp, reasoningStyle, reasoningFieldName, reasoningTags);
                }
            }
            return;
        }

        // 3. Handle Responses API Static Items (non-streaming or full response)
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
            // In V2, the function call details are at the top level of the item
            String itemId = node.path("id").asText();
            String callId = node.path("call_id").asText();
            String name = node.path("name").asText();
            String args = node.path("arguments").asText();

            if (callId != null && name != null) {
                callIds.put(itemId, callId);
                callNames.put(itemId, name);
                callArgsBuffers.put(itemId, new StringBuilder(args));
            }
        }

        // 4. Finish Reason / Status
        if (node.has("status") && "completed".equals(node.path("status").asText())) {
            setFinishReason(FinishReason.STOP);
            flushToolCalls();
        } else if (node.has("finish_reason") && !node.get("finish_reason").isNull()) {
            setFinishReasonFromOpenAi(node.get("finish_reason").asText());
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
        if (callArgsBuffers == null) {
            callArgsBuffers = new HashMap<>();
        }
        if (callIds == null) {
            callIds = new HashMap<>();
        }
        if (callNames == null) {
            callNames = new HashMap<>();
        }

        String itemId = callNode.path("id").asText(null);
        String callId = callNode.path("call_id").asText(null);

        // Support both V1 (nested) and V2 (flat) structures
        //this is totally wrong, we have a different ModelMessage implementation for this.
        JsonNode funcNode = callNode.get("function");
        
        String name = (funcNode != null && funcNode.has("name")) ? funcNode.get("name").asText() : callNode.path("name").asText(null);
        String fqToolName;
        String nameSpace = (funcNode != null && funcNode.has("namespace")) ? funcNode.get("namespace").asText() : callNode.path("namespace").asText(null);
        if (nameSpace != null) {
            fqToolName = nameSpace + "." + name;
        } else {
            log.warn("No namespace found for: " + callNode.toPrettyString());
            fqToolName = name;
        }
        String argsFragment = (funcNode != null && funcNode.has("arguments")) ? funcNode.get("arguments").asText("") : callNode.path("arguments").asText("");

        if (itemId != null) {
            if (callId != null) {
                callIds.put(itemId, callId);
            }
            if (name != null) {
                callNames.put(itemId, fqToolName);
            }
            if (!argsFragment.isEmpty()) {
                callArgsBuffers.computeIfAbsent(itemId, k -> new StringBuilder()).append(argsFragment);
            }
        }
    }

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
