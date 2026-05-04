/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.TextPart;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;

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
@Setter
public class OpenAiModelMessage extends AbstractModelMessage<OpenAiResponse> {

    private static final ObjectMapper API_MAPPER = new ObjectMapper();

    /** Placeholder text used for reasoning items in stateless mode when no summary is available. */
    public static final String ENCRYPTED_REASONING_PLACEHOLDER = "(Encrypted Reasoning Chain)";

    /** The generation phase reported by the model (e.g., 'commentary', 'final_answer'). */
    private String phase;

    public OpenAiModelMessage(Agi agi, String modelId) {
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
        String id = item.path("id").asText(null);
        // 1. Map Status to FinishReason (updates as we process items)
        String status = item.path("status").asText();
        if ("completed".equals(status)) {
            setFinishReason(FinishReason.STOP);
        } else if ("incomplete".equals(status)) {
            setFinishReason(FinishReason.MAX_TOKENS);
        }
        if ("message".equals(type)) {
            setProviderId(id);
            this.phase = item.path("phase").asText(null);
            JsonNode content = item.get("content");
            if (content != null && content.isArray()) {
                for (JsonNode part : content) {
                    String partType = part.path("type").asText();
                    String text = part.path("text").asText();
                    TextPart tp = null;
                    if ("output_text".equals(partType)) {
                        tp = addTextPart(text, null, false);
                    } else if ("reasoning_content".equals(partType)) {
                        tp = addTextPart(text, null, true);
                    }
                    if (tp != null) {
                        tp.setProviderId(id);
                    }
                }
            }
        } else if ("reasoning".equals(type)) {
            // Stateless Reasoning: Capture the signature even if text is hidden
            String encrypted = item.path("encrypted_content").asText(null);
            // Capture optional summary text
            StringBuilder summaryText = new StringBuilder();
            JsonNode summary = item.get("summary");
            if (summary != null && summary.isArray()) {
                for (JsonNode s : summary) {
                    if ("summary_text".equals(s.path("type").asText())) {
                        summaryText.append(s.path("text").asText()).append("\n");
                    }
                }
            }
            if (encrypted != null) {
                String label = summaryText.length() > 0 ? summaryText.toString().trim() : ENCRYPTED_REASONING_PLACEHOLDER;
                // Store signature in a thought part.
                TextPart tp = addTextPart(label, encrypted.getBytes(), true);
                tp.setProviderId(id);
            }
        } else if ("function_call".equals(type)) {
            String callId = item.path("call_id").asText();
            String ns = item.path("namespace").asText(null);
            String name = item.path("name").asText();
            String argsJson = item.path("arguments").asText("{}");
            // Reconstruct the Canonical FQN for Anahata tool lookup
            String fullToolName = (ns != null && !ns.isEmpty()) ? ns + "." + name : name;
            Map<String, Object> args = API_MAPPER.readValue(argsJson, Map.class);
            AbstractToolCall tc = getAgi().getToolManager().createToolCall(this, callId, fullToolName, args);
            if (tc != null) {
                tc.setProviderId(id);
            }
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
