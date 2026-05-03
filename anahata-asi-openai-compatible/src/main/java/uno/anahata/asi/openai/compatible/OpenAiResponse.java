/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.ResponseUsageMetadata;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.internal.JacksonUtils;

import java.util.Optional;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Encapsulates the complete response from an OpenAI-compatible API.
 * It parses the native OpenAI JSON (standard or Responses API) into Anahata's
 * model messages and usage metadata.
 * 
 * @author anahata
 */
@Getter
public class OpenAiResponse extends Response<OpenAiModelMessage> {

    /**
     * The unique identifier for the response (e.g., chatcmpl-..., resp_...).
     */
    private final String id;

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
     * 
     * @param agi The parent session.
     * @param modelId The ID of the model.
     * @param jsonResponse The raw JSON response.
     * @param requestPayload The raw payload sent.
     * @param historyJson The history segment of the payload.
     * @param model The model instance.
     * @param estimatedUsage The pre-calculated usage metadata.
     */
    public OpenAiResponse(Agi agi, String modelId, String jsonResponse, String requestPayload, String historyJson, OpenAiCompatibleModel model, ResponseUsageMetadata estimatedUsage) {
        JsonNode root = JacksonUtils.parse(jsonResponse, JsonNode.class);
        JsonNode responseNode = root.has("response") ? root.get("response") : root;
        this.id = responseNode.path("id").asText("estimated-id");
        this.rawJson = jsonResponse;
        this.rawRequestConfigJson = requestPayload;
        this.rawHistoryJson = historyJson;
        this.usageMetadata = estimatedUsage;
        parseCandidates(agi, modelId, model, responseNode);
    }

    /**
     * Constructs a new response object by parsing the JSON returned
     * from an OpenAI-compatible endpoint.
     * 
     * @param agi The parent session.
     * @param modelId The ID of the model.
     * @param jsonResponse The raw JSON response string.
     * @param requestPayload The raw payload sent to the API.
     * @param historyJson The raw history segment of the payload.
     * @param model The model instance to retrieve reasoning configuration from.
     */
    public OpenAiResponse(Agi agi, String modelId, String jsonResponse, String requestPayload, String historyJson, OpenAiCompatibleModel model) {
        this.rawRequestConfigJson = requestPayload;
        this.rawHistoryJson = historyJson;
        JsonNode root = JacksonUtils.parse(jsonResponse, JsonNode.class);
        JsonNode responseNode = root.has("response") ? root.get("response") : root;
        this.id = responseNode.path("id").asText(null);
        // 1. Usage
        JsonNode usage = responseNode.get("usage");
        if (usage != null) {
            int thoughts = usage.path("reasoning_tokens").asInt();
            if (thoughts == 0 && usage.has("completion_tokens_details")) {
                thoughts = usage.path("completion_tokens_details").path("reasoning_tokens").asInt();
            }
            this.usageMetadata = ResponseUsageMetadata.builder()
                    .promptTokenCount(usage.path("prompt_tokens").asInt())
                    .candidatesTokenCount(usage.path("completion_tokens").asInt())
                    .totalTokenCount(usage.path("total_tokens").asInt())
                    .thoughtsTokenCount(thoughts)
                    .rawJson(usage.toString())
                    .build();
        } else {
            this.usageMetadata = ResponseUsageMetadata.builder().build();
        }
        // 2. Display JSON Clean-up (remove echoed input/tools from display rawJson)
        ObjectNode displayNode = responseNode.deepCopy();
        displayNode.remove("tools");
        displayNode.remove("instructions");
        displayNode.remove("input");
        displayNode.remove("messages");
        displayNode.remove("prompt");
        this.rawJson = displayNode.toPrettyString();
        // 3. Candidates
        parseCandidates(agi, modelId, model, responseNode);
    }

    private void parseCandidates(Agi agi, String modelId, OpenAiCompatibleModel model, JsonNode responseNode) {
        if (responseNode.has("choices")) {
            JsonNode choices = responseNode.get("choices");
            if (choices != null && choices.isArray()) {
                for (JsonNode choice : choices) {
                    OpenAiChatCompletionMessage msg = new OpenAiChatCompletionMessage(agi, modelId, choice, this, 
                            model.getReasoningStyle(), model.getReasoningFieldName(), model.getReasoningTags());
                    candidates.add(msg);
                }
            }
        } else if (responseNode.has("output")) {
            JsonNode output = responseNode.get("output");
            if (output != null && output.isArray()) {
                OpenAiModelMessage message = model.createModelMessage(agi);
                message.setResponse(this);
                // Set candidate-specific rawJson to the output items array JSON
                message.setRawJson(output.toPrettyString());
                for (JsonNode item : output) {
                    message.updateFromNode(item, model.getReasoningStyle(), model.getReasoningFieldName(), model.getReasoningTags());
                }
                candidates.add(message);
            }
        }
    }
    // Set the candidate-specific rawJson to the output array JSON
    
    /**
     * {@inheritDoc}
     * <p>OpenAI does not provide standard prompt feedback in the completion response; returns empty.</p>
     */
    @Override
    public Optional<String> getPromptFeedback() {
        return Optional.empty();
    }

    /**
     * {@inheritDoc}
     * <p>Returns the total token count as reported in the 'usage' block of the API response.</p>
     */
    @Override
    public int getTotalTokenCount() {
        return usageMetadata.getTotalTokenCount();
    }
}
