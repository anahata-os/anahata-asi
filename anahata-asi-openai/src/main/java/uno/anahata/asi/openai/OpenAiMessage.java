/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.Map;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Specialized ModelMessage for the OpenAI Responses API.
 * 
 * <p>Aggregates multiple items (messages, reasoning, and function calls) 
 * from a single Responses API turn into a unified Anahata message.</p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
public class OpenAiMessage extends AbstractModelMessage<OpenAiResponse> {

    /** Placeholder text used for reasoning items in stateless mode when no summary is available. */
    public static final String ENCRYPTED_REASONING_PLACEHOLDER = "(Encrypted Reasoning Chain)";

    public OpenAiMessage(Agi agi, String modelId) {
        super(agi, modelId);
    }

    /**
     * Processes a single item from the OpenAI 'output' array and maps it 
     * to the appropriate Anahata parts.
     * 
     * @param item The JSON node representing an OpenAI item.
     */
    @SneakyThrows
    public void processItem(JsonNode item) {
        String type = item.path("type").asText();
        
        // 1. Map Status to FinishReason (updates as we process items)
        String status = item.path("status").asText();
        if ("completed".equals(status)) {
            setFinishReason(FinishReason.STOP);
        } else if ("incomplete".equals(status)) {
            setFinishReason(FinishReason.MAX_TOKENS);
        }

        if ("message".equals(type)) {
            JsonNode content = item.get("content");
            if (content != null && content.isArray()) {
                for (JsonNode part : content) {
                    String partType = part.path("type").asText();
                    String text = part.path("text").asText();
                    if ("output_text".equals(partType)) {
                        addTextPart(text, null, false);
                    } else if ("reasoning_content".equals(partType)) {
                        addTextPart(text, null, true);
                    }
                }
            }
        } else if ("reasoning".equals(type)) {
            // Stateless Reasoning: Capture the signature even if text is hidden
            String encrypted = item.path("encrypted_content").asText(null);
            if (encrypted != null) {
                // Store signature in a thought part. Use the constant placeholder.
                addTextPart(ENCRYPTED_REASONING_PLACEHOLDER, encrypted.getBytes(), true);
            }
        } else if ("function_call".equals(type)) {
            String callId = item.path("call_id").asText();
            String ns = item.path("namespace").asText();
            String name = item.path("name").asText();
            String argsJson = item.path("arguments").asText("{}");
            
            // Reconstruct the Canonical FQN for Anahata tool lookup
            String fullToolName = (ns != null && !ns.isEmpty()) ? ns + "." + name : name;
            
            Map<String, Object> args = JacksonUtils.parse(argsJson, Map.class);
            getAgi().getToolManager().createToolCall(this, callId, fullToolName, args);
        }
    }

    @Override
    public String getFrom() {
        return getModelId();
    }

    @Override
    public String getDevice() {
        return "OpenAI-Cloud";
    }
}
