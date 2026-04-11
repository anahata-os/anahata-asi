/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * An OpenAI-specific implementation of {@link AbstractModelMessage}.
 * It parses the choices and tool calls from the OpenAI API response JSON 
 * into the Anahata domain model.
 * 
 * @author anahata
 */
@Slf4j
public class OpenAiModelMessage extends AbstractModelMessage<OpenAiResponse> {

    /**
     * Constructs a new OpenAI model message by parsing a specific choice
     * from the API response.
     * @param agi The parent session.
     * @param modelId The ID of the model that generated the message.
     * @param choiceNode The JSON node containing the choice data.
     * @param response The parent response object.
     */
    public OpenAiModelMessage(Agi agi, String modelId, JsonNode choiceNode, OpenAiResponse response) {
        super(agi, modelId);
        setResponse(response);
        parseChoice(choiceNode);
    }

    /**
     * Extracts text content and tool calls from the OpenAI 'message' node.
     * <p>
     * Implementation details: Maps the 'content' field to a text part and
     * recursively converts each 'tool_calls' entry into a native Anahata
     * tool call via the {@link uno.anahata.asi.agi.tool.ToolManager}.
     * </p>
     * @param choice The choices array element node.
     */
    private void parseChoice(JsonNode choice) {
        JsonNode messageNode = choice.get("message");
        if (messageNode == null) return;
        
        // 1. Parse Text Content
        if (messageNode.has("content") && !messageNode.get("content").isNull()) {
            addTextPart(messageNode.get("content").asText());
        }
        
        // 2. Parse Tool Calls
        if (messageNode.has("tool_calls")) {
            for (JsonNode call : messageNode.get("tool_calls")) {
                String id = call.get("id").asText();
                JsonNode func = call.get("function");
                String name = func.get("name").asText();
                String argsJson = func.get("arguments").asText();
                
                Map<String, Object> args = JacksonUtils.parse(argsJson, Map.class);
                getAgi().getToolManager().createToolCall(this, id, name, args);
            }
        }
        
        // 3. Finish Reason
        if (choice.has("finish_reason")) {
            setFinishReason(mapFinishReason(choice.get("finish_reason").asText()));
        }
    }

    /**
     * Maps OpenAI's finish reason strings to our internal enum.
     * @param reason The raw reason string from the API.
     * @return The corresponding {@link FinishReason}.
     */
    private FinishReason mapFinishReason(String reason) {
        return switch (reason) {
            case "stop" -> FinishReason.STOP;
            case "length" -> FinishReason.MAX_TOKENS;
            case "tool_calls" -> FinishReason.STOP; // Logical continuation
            case "content_filter" -> FinishReason.SAFETY;
            default -> FinishReason.OTHER;
        };
    }
}
