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

    /**
     * The final candidates (choices) converted to Anahata messages.
     */
    private final List<OpenAiModelMessage> candidates = new ArrayList<>();
    
    /**
     * Usage statistics from the API, including prompt and completion tokens.
     */
    private final ResponseUsageMetadata usageMetadata;
    
    /**
     * The raw request configuration JSON sent to the API.
     */
    private final String rawRequestConfigJson;
    
    /**
     * The raw conversation history JSON sent as part of the payload.
     */
    private final String rawHistoryJson;
    
    /**
     * The complete raw response JSON received from the API.
     */
    private final String rawJson;

    /**
     * Constructs a new response object with pre-calculated usage metadata.
     * Used for client-side estimation when the API doesn't provide usage data.
     */
    public OpenAiResponse(Agi agi, String modelId, String jsonResponse, String requestPayload, String historyJson, OpenAiModel model, ResponseUsageMetadata estimatedUsage) {
        this.rawJson = jsonResponse;
        this.rawRequestConfigJson = requestPayload;
        this.rawHistoryJson = historyJson;
        this.usageMetadata = estimatedUsage;
        parseChoices(agi, modelId, model);
    }

    /**
     * Constructs a new response object by parsing the JSON returned
     * from an OpenAI-compatible endpoint.
     * @param agi The parent session.
     * @param modelId The ID of the model that generated the response.
     * @param jsonResponse The raw JSON response string.
     * @param requestPayload The raw payload sent to the API.
     * @param historyJson The raw history segment of the payload.
     * @param model The model instance to retrieve reasoning configuration from.
     */
    public OpenAiResponse(Agi agi, String modelId, String jsonResponse, String requestPayload, String historyJson, OpenAiModel model) {
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
                    .thoughtsTokenCount(usage.path("reasoning_tokens").asInt())
                    .rawJson(usage.toString())
                    .build();
        } else {
            this.usageMetadata = ResponseUsageMetadata.builder().build();
        }
        
        // 2. Choices
        parseChoices(agi, modelId, model);
    }

    /**
     * Parses the response choices from the raw JSON and populates the candidates list.
     */
    private void parseChoices(Agi agi, String modelId, OpenAiModel model) {
        JsonNode root = JacksonUtils.parse(rawJson, JsonNode.class);
        JsonNode choices = root.get("choices");
        if (choices != null && choices.isArray()) {
            for (JsonNode choice : choices) {
                candidates.add(new OpenAiModelMessage(agi, modelId, choice, this, 
                        model.getReasoningStyle(), model.getReasoningFieldName(), model.getReasoningTags()));
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: OpenAI does not provide standard prompt feedback
     * in the completion response; returns empty.
     * </p>
     */
    @Override
    public Optional<String> getPromptFeedback() {
        return Optional.empty();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the total token count as reported in
     * the 'usage' block of the API response.
     * </p>
     */
    @Override
    public int getTotalTokenCount() {
        return usageMetadata.getTotalTokenCount();
    }
}
