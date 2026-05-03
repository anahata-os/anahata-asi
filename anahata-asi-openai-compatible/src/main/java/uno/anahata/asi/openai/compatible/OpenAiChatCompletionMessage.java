/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Implementation of {@link OpenAiModelMessage} for the standard Chat Completions API.
 * Handles the classic "choices" and "delta" structures used by most OpenAI-compatible providers.
 * 
 * @author anahata
 */
@Slf4j
public class OpenAiChatCompletionMessage extends OpenAiModelMessage {

    /**
     * Buffers for accumulating streaming tool call arguments, keyed by their
     * index in the 'choices' array.
     */
    private transient Map<Integer, StringBuilder> callArgsBuffers;
    private transient Map<Integer, String> callIds;
    private transient Map<Integer, String> callNames;

    public OpenAiChatCompletionMessage(Agi agi, String modelId) {
        super(agi, modelId);
        this.callArgsBuffers = new HashMap<>();
        this.callIds = new HashMap<>();
        this.callNames = new HashMap<>();
    }

    /**
     * Constructs a message from a final (non-streaming) choice node.
     */
    public OpenAiChatCompletionMessage(Agi agi, String modelId, JsonNode choiceNode, OpenAiResponse response,
            ReasoningStyle reasoningStyle, String reasoningFieldName, List<String> reasoningTags) {
        this(agi, modelId);
        setResponse(response);
        if (choiceNode != null) {
            setRawJson(choiceNode.toString());
            updateFromNode(choiceNode, reasoningStyle, reasoningFieldName, reasoningTags);
        }
    }

    /**
     * {@inheritDoc}
     * <p>Handles both streaming deltas and final message objects from the Chat Completions API.</p>
     */
    @Override
    public void updateFromNode(JsonNode choice, ReasoningStyle reasoningStyle, String reasoningFieldName, List<String> reasoningTags) {
        JsonNode messageNode = choice.get("message");
        if (messageNode == null) {
            messageNode = choice.get("delta");
        }
        if (messageNode == null) {
            return;
        }

        // 0. AUTODETECT: Check for reasoning_content field on first chunk if not explicitly configured
        if (reasoningStyle == ReasoningStyle.NONE
                && messageNode.has("reasoning_content") && !messageNode.get("reasoning_content").isNull()) {
            log.info("Auto-detected FIELD reasoning style with field 'reasoning_content' for model {}", getModelId());
            reasoningStyle = ReasoningStyle.FIELD;
            reasoningFieldName = "reasoning_content";
        }

        // 1. Reasoning Content (FIELD style)
        if (reasoningStyle == ReasoningStyle.FIELD && reasoningFieldName != null
                && messageNode.has(reasoningFieldName) && !messageNode.get(reasoningFieldName).isNull()) {
            appendThoughts(messageNode.get(reasoningFieldName).asText());
        }

        // 2. Text Content
        if (messageNode.has("content") && !messageNode.get("content").isNull()) {
            String text = messageNode.get("content").asText();
            if (!text.isEmpty()) {
                if (reasoningStyle == ReasoningStyle.TAGS && reasoningTags != null && reasoningTags.size() >= 2) {
                    appendTaggedContent(text, reasoningTags.get(0), reasoningTags.get(1));
                } else {
                    appendContent(text);
                }
            }
        }

        // 3. Tool Calls
        if (messageNode.has("tool_calls")) {
            for (JsonNode callNode : messageNode.get("tool_calls")) {
                updateToolCall(callNode);
            }
        }

        // 4. Finish Reason
        if (choice.has("finish_reason") && !choice.get("finish_reason").isNull()) {
            setFinishReasonFromOpenAi(choice.get("finish_reason").asText());
        }
    }

    @Override
    public void updateToolCall(JsonNode callNode) {
        if (callArgsBuffers == null) callArgsBuffers = new HashMap<>();
        if (callIds == null) callIds = new HashMap<>();
        if (callNames == null) callNames = new HashMap<>();
        
        String callId = callNode.path("id").asText(null);
        int index = callNode.path("index").asInt(-1);
        JsonNode funcNode = callNode.get("function");

        if (callId != null) {
            callIds.put(index, callId);
            if (funcNode != null && funcNode.has("name")) {
                callNames.put(index, funcNode.get("name").asText());
            }
        }

        if (index != -1 && funcNode != null && funcNode.has("arguments")) {
            String argsFragment = funcNode.get("arguments").asText("");
            if (!argsFragment.isEmpty()) {
                callArgsBuffers.computeIfAbsent(index, k -> new StringBuilder()).append(argsFragment);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void flushToolCalls() {
        if (callArgsBuffers == null) {
            return;
        }
        for (Integer index : callArgsBuffers.keySet()) {
            String id = callIds.get(index);
            String name = callNames.get(index);
            String fullJson = callArgsBuffers.get(index).toString();

            if (id != null && name != null && !fullJson.isEmpty()) {
                try {
                    Map<String, Object> args = JacksonUtils.parse(fullJson, Map.class);
                    getAgi().getToolManager().createToolCall(this, id, name, args);
                } catch (Exception e) {
                    log.error("Failed to parse buffered tool call arguments for index {}: {}", index, fullJson, e);
                }
            }
        }
        callArgsBuffers = null;
        callIds = null;
        callNames = null;
    }
}
