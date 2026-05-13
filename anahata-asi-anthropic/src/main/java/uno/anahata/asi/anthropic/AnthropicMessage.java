/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Message implementation for Anthropic's Claude.
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class AnthropicMessage extends AbstractModelMessage<AnthropicResponse> {

    private transient Map<Integer, StringBuilder> toolArgBuffers = new HashMap<>();
    private transient Map<Integer, String> toolIds = new HashMap<>();
    private transient Map<Integer, String> toolNames = new HashMap<>();

    public AnthropicMessage(Agi agi, String modelId) {
        super(agi, modelId);
    }

    public void parseFinalContent(JsonNode contentArray) {
        for (JsonNode block : contentArray) {
            String type = block.path("type").asText();
            if ("text".equals(type)) {
                appendContent(block.path("text").asText());
            } else if ("tool_use".equals(type)) {
                String id = block.path("id").asText();
                String name = block.path("name").asText();
                JsonNode input = block.get("input");
                Map<String, Object> args = input != null ? JacksonUtils.parse(input.toString(), Map.class) : new HashMap<>();
                getAgi().getToolManager().createToolCall(this, id, name, args);
            }
        }
    }

    public void handleEvent(String eventType, JsonNode data) {
        switch (eventType) {
            case "message_start":
                JsonNode msgNode = data.get("message");
                if (msgNode != null && msgNode.has("usage")) {
                    setBilledTokenCount(msgNode.get("usage").path("input_tokens").asInt(0));
                }
                break;
            case "content_block_start":
                int index = data.path("index").asInt();
                JsonNode block = data.get("content_block");
                if (block != null && "tool_use".equals(block.path("type").asText())) {
                    toolIds.put(index, block.path("id").asText());
                    toolNames.put(index, block.path("name").asText());
                    toolArgBuffers.put(index, new StringBuilder());
                }
                break;
            case "content_block_delta":
                index = data.path("index").asInt();
                JsonNode delta = data.get("delta");
                if (delta != null) {
                    if ("text_delta".equals(delta.path("type").asText())) {
                        appendContent(delta.path("text").asText());
                    } else if ("input_json_delta".equals(delta.path("type").asText())) {
                        toolArgBuffers.computeIfAbsent(index, k -> new StringBuilder()).append(delta.path("partial_json").asText());
                    }
                }
                break;
            case "content_block_stop":
                index = data.path("index").asInt();
                if (toolIds.containsKey(index)) {
                    flushToolCall(index);
                }
                break;
            case "message_delta":
                JsonNode deltaNode = data.get("delta");
                if (deltaNode != null && deltaNode.has("stop_reason") && !deltaNode.get("stop_reason").isNull()) {
                    setFinishReasonFromAnthropic(deltaNode.path("stop_reason").asText());
                }
                if (data.has("usage")) {
                    setBilledTokenCount(getBilledTokenCount() + data.get("usage").path("output_tokens").asInt(0));
                }
                break;
        }
    }

    private void flushToolCall(int index) {
        String id = toolIds.remove(index);
        String name = toolNames.remove(index);
        StringBuilder argsBuilder = toolArgBuffers.remove(index);
        if (id != null && name != null && argsBuilder != null) {
            try {
                Map<String, Object> args = JacksonUtils.parse(argsBuilder.toString(), Map.class);
                getAgi().getToolManager().createToolCall(this, id, name, args);
            } catch (Exception e) {
                log.error("Failed to parse buffered tool call arguments", e);
            }
        }
    }

    public void appendContent(String text) {
        List<AbstractPart> parts = getParts();
        if (!parts.isEmpty() && parts.get(parts.size() - 1) instanceof ModelTextPart mtp) {
            mtp.appendText(text);
        } else {
            addTextPart(text);
        }
    }

    public void setFinishReasonFromAnthropic(String reason) {
        if ("end_turn".equals(reason)) setFinishReason(FinishReason.STOP);
        else if ("max_tokens".equals(reason)) setFinishReason(FinishReason.MAX_TOKENS);
        else if ("tool_use".equals(reason)) {
            setFinishReason(FinishReason.STOP);
            if (toolIds != null && !toolIds.isEmpty()) {
                for (Integer k : new java.util.ArrayList<>(toolIds.keySet())) {
                    flushToolCall(k);
                }
            }
        }
        else setFinishReason(FinishReason.OTHER);
    }
}