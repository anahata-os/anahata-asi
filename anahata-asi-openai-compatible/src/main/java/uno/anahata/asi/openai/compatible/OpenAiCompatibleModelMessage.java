/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Concrete model message implementation for OpenAI-compatible Chat Completion
 * APIs. Handles content accumulation, finish reason mapping, and tool call
 * streaming buffers.
 *
 * @author anahata
 */
@Slf4j
public class OpenAiCompatibleModelMessage extends AbstractModelMessage<OpenAiCompatibleResponse> {

    /**
     * State flag indicating if the parser is currently inside a reasoning tag.
     */
    private boolean insideReasoningTags = false;

    /**
     * Buffers for accumulating streaming tool call arguments, keyed by their
     * index in the 'tool_calls' array.
     */
    private transient Map<Integer, StringBuilder> callArgsBuffers;
    /**
     * Maps tool call indices to their stable API-provided unique IDs.
     */
    private transient Map<Integer, String> callIds;
    /**
     * Maps tool call indices to the requested function name.
     */
    private transient Map<Integer, String> callNames;

    /**
     * Constructs a new, empty compatible message for streaming.
     *
     * @param agi The parent session.
     * @param modelId The model ID.
     */
    public OpenAiCompatibleModelMessage(Agi agi, String modelId) {
        super(agi, modelId);
        this.callArgsBuffers = new HashMap<>();
        this.callIds = new HashMap<>();
        this.callNames = new HashMap<>();
    }

    /**
     * Constructs a message from a final (non-streaming) choice node.
     *
     * @param agi The parent session.
     * @param modelId The model ID.
     * @param choiceNode The 'choice' node from the chat completion response.
     * @param response The parent response object.
     * @param reasoningStyle The strategy for thought extraction.
     * @param reasoningFieldName The field for thoughts (if using FIELD style).
     * @param reasoningTags The tags for thoughts (if using TAGS style).
     */
    public OpenAiCompatibleModelMessage(Agi agi, String modelId, JsonNode choiceNode, OpenAiCompatibleResponse response,
            OpenAiCompatibleReasoningStyle reasoningStyle, String reasoningFieldName, List<String> reasoningTags) {
        this(agi, modelId);
        setResponse(response);
        if (choiceNode != null) {
            setRawJson(choiceNode.toString());
            updateFromNode(choiceNode, reasoningStyle, reasoningFieldName, reasoningTags);
        }
    }

    /**
     * Updates the message content and state from a JSON node (choice, item, or
     * event). Handles both streaming deltas and final message objects from the
     * Chat Completions API.
     *
     * @param choice The JSON node to parse.
     * @param reasoningStyle The strategy for extracting thoughts.
     * @param reasoningFieldName The field name for reasoning content (if using
     * FIELD style).
     * @param reasoningTags The tags for reasoning content (if using TAGS
     * style).
     */
    public void updateFromNode(JsonNode choice, OpenAiCompatibleReasoningStyle reasoningStyle, String reasoningFieldName, List<String> reasoningTags) {
        JsonNode messageNode = choice.get("message");
        if (messageNode == null) {
            messageNode = choice.get("delta");
        }
        if (messageNode == null) {
            return;
        }

        // 0. AUTODETECT: Check for reasoning_content field on first chunk if not explicitly configured
        if (reasoningStyle == OpenAiCompatibleReasoningStyle.NONE
                && messageNode.has("reasoning_content") && !messageNode.get("reasoning_content").isNull()) {
            log.info("Auto-detected FIELD reasoning style with field 'reasoning_content' for model {}", getModelId());
            reasoningStyle = OpenAiCompatibleReasoningStyle.FIELD;
            reasoningFieldName = "reasoning_content";
        }

        if (reasoningStyle == OpenAiCompatibleReasoningStyle.NONE
                && messageNode.has("content") && !messageNode.get("content").isNull()
                && messageNode.get("content").asText().contains("<think>")) {
            log.info("Auto-detected TAGS reasoning style with '<think>' for model {}", getModelId());
            reasoningStyle = OpenAiCompatibleReasoningStyle.TAGS;
            reasoningTags = List.of("<think>", "</think>");
        }

        // 1. Reasoning Content (FIELD style)
        if (reasoningStyle == OpenAiCompatibleReasoningStyle.FIELD && reasoningFieldName != null
                && messageNode.has(reasoningFieldName) && !messageNode.get(reasoningFieldName).isNull()) {
            appendThoughts(messageNode.get(reasoningFieldName).asText());
        }

        // 2. Text Content
        if (messageNode.has("content") && !messageNode.get("content").isNull()) {
            String text = messageNode.get("content").asText();
            if (!text.isEmpty()) {
                if (reasoningStyle == OpenAiCompatibleReasoningStyle.TAGS && reasoningTags != null && reasoningTags.size() >= 2) {
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

    /**
     * Updates a single tool call from a JSON node.
     *
     * @param callNode The JSON node containing the tool call (or delta).
     */
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

        String callId = callNode.path("id").asText(null);
        if (callId != null && callId.isBlank()) {
            callId = null;
        }

        int index = callNode.path("index").asInt(-1);

        if (index == -1 && callId != null) {
            index = callIds.size();
        }

        JsonNode funcNode = callNode.get("function");

        if (callId != null) {
            callIds.put(index, callId);
        }

        if (funcNode != null && funcNode.has("name")) {
            String name = funcNode.get("name").asText();
            if (name != null && !name.isBlank()) {
                callNames.put(index, name);
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
     * Flushes any buffered tool calls (used during streaming). This ensures
     * that partial tool call arguments are fully assembled and registered in
     * the tool manager.
     */
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

    /**
     * Sets the finish reason from a raw OpenAI string.
     *
     * @param fr The raw finish reason string (e.g., "stop", "length").
     */
    public void setFinishReasonFromOpenAi(String fr) {
        setFinishReason(mapFinishReason(fr));
        if ("stop".equals(fr) || "tool_calls".equals(fr)) {
            flushToolCalls();
        }
    }

    /**
     * Maps a standard OpenAI finish reason string to the Anahata enum.
     *
     * @param reason The raw string from the API.
     * @return The corresponding FinishReason.
     */
    private FinishReason mapFinishReason(String reason) {
        if (reason == null) {
            return FinishReason.OTHER;
        }
        return switch (reason) {
            case "stop" ->
                FinishReason.STOP;
            case "length" ->
                FinishReason.MAX_TOKENS;
            case "tool_calls" ->
                FinishReason.STOP;
            case "content_filter" ->
                FinishReason.SAFETY;
            default ->
                FinishReason.OTHER;
        };
    }

    /**
     * {@inheritDoc}
     * <p>
     * An actively streaming model message is never considered pruned, even if its parts list
     * is temporarily empty while the first chunk is being parsed.
     * </p>
     */
    @Override
    public boolean isEffectivelyPruned() {
        if (isStreaming()) {
            return false;
        }
        return super.isEffectivelyPruned();
    }

    /**
     * Appends text to the main content part or creates a new one.
     *
     * @param text The text to append.
     */
    public void appendContent(String text) {
        if (text == null || text.isEmpty()) {
            return;
        }
        List<AbstractPart> parts = getParts();
        if (!parts.isEmpty() && parts.get(parts.size() - 1) instanceof ModelTextPart mtp && !mtp.isThought()) {
            mtp.appendText(text);
        } else {
            addTextPart(text);
        }
    }

    /**
     * Appends text to the reasoning/thought part or creates a new one.
     *
     * @param text The thought text to append.
     */
    public void appendThoughts(String text) {
        if (text == null || text.isEmpty()) {
            return;
        }
        List<AbstractPart> parts = getParts();
        if (!parts.isEmpty() && parts.get(parts.size() - 1) instanceof ModelTextPart mtp && mtp.isThought()) {
            mtp.appendText(text);
        } else {
            addTextPart(text, null, true);
        }
    }

    /**
     * Appends text while detecting and extracting reasoning content wrapped in
     * tags.
     *
     * @param text The text containing potential tags.
     * @param startTag The opening tag (e.g., {@code <think>}).
     * @param endTag The closing tag (e.g., {@code </think>}).
     */
    public void appendTaggedContent(String text, String startTag, String endTag) {
        if (!insideReasoningTags && text.contains(startTag)) {
            int idx = text.indexOf(startTag);
            String before = text.substring(0, idx);
            if (!before.isEmpty()) {
                appendContent(before);
            }
            insideReasoningTags = true;
            appendTaggedContent(text.substring(idx + startTag.length()), startTag, endTag);
        } else if (insideReasoningTags && text.contains(endTag)) {
            int idx = text.indexOf(endTag);
            String thoughts = text.substring(0, idx);
            if (!thoughts.isEmpty()) {
                appendThoughts(thoughts);
            }
            insideReasoningTags = false;
            appendTaggedContent(text.substring(idx + endTag.length()), startTag, endTag);
        } else {
            if (insideReasoningTags) {
                appendThoughts(text);
            } else {
                appendContent(text);
            }
        }
    }
}
