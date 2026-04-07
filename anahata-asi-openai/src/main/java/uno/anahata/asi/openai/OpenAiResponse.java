/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.ResponseUsageMetadata;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.internal.JacksonUtils;

import java.util.Optional;

/**
 * Encapsulates the complete response from an OpenAI-compatible API.
 * 
 * @author anahata
 */
@Getter
public class OpenAiResponse extends Response<OpenAiModelMessage> {

    /** The final candidates (choices) converted to Anahata messages. */
    private final List<OpenAiModelMessage> candidates = new ArrayList<>();
    
    /** Usage statistics from the API. */
    private final ResponseUsageMetadata usageMetadata;
    
    /** The raw request configuration JSON. */
    private final String rawRequestConfigJson;
    
    /** The raw conversation history JSON. */
    private final String rawHistoryJson;
    
    /** The full raw response JSON. */
    private final String rawJson;

    public OpenAiResponse(Agi agi, String modelId, String jsonResponse, String requestPayload, String historyJson) {
        this.rawJson = jsonResponse;
        this.rawRequestConfigJson = requestPayload;
        this.rawHistoryJson = historyJson;
        
        JsonNode root = JacksonUtils.parse(jsonResponse, JsonNode.class);
        
        // 1. Usage
        JsonNode usage = root.get("usage");
        if (usage != null) {
            this.usageMetadata = ResponseUsageMetadata.builder()
                    .promptTokenCount(usage.path("prompt_tokens").asInt())
                    .candidatesTokenCount(usage.path("completion_tokens").asInt())
                    .totalTokenCount(usage.path("total_tokens").asInt())
                    .rawJson(usage.toString())
                    .build();
        } else {
            this.usageMetadata = ResponseUsageMetadata.builder().build();
        }
        
        // 2. Choices
        JsonNode choices = root.get("choices");
        if (choices != null && choices.isArray()) {
            for (JsonNode choice : choices) {
                candidates.add(new OpenAiModelMessage(agi, modelId, choice, this));
            }
        }
    }

    @Override
    public Optional<String> getPromptFeedback() {
        return Optional.empty();
    }

    @Override
    public int getTotalTokenCount() {
        return usageMetadata.getTotalTokenCount();
    }
}
