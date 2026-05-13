/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.ResponseUsageMetadata;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Response implementation for Anthropic's Claude.
 * 
 * @author anahata
 */
@Getter
public class AnthropicResponse extends Response<AnthropicMessage> {

    private final String id;
    private final List<AnthropicMessage> candidates = new ArrayList<>();
    private final ResponseUsageMetadata usageMetadata;
    private final String rawJson;
    private final String rawRequestConfigJson;
    private final String rawHistoryJson;

    public AnthropicResponse(String rawRequestConfigJson, String rawHistoryJson, Agi agi, String modelId, String responseBody) {
        this.rawRequestConfigJson = rawRequestConfigJson;
        this.rawHistoryJson = rawHistoryJson;
        this.rawJson = responseBody;

        JsonNode root = JacksonUtils.parse(responseBody, JsonNode.class);
        this.id = root.path("id").asText("anthropic-response");
        
        JsonNode usage = root.get("usage");
        if (usage != null) {
            int inputTokens = usage.path("input_tokens").asInt(0);
            int outputTokens = usage.path("output_tokens").asInt(0);
            this.usageMetadata = ResponseUsageMetadata.builder()
                    .promptTokenCount(inputTokens)
                    .candidatesTokenCount(outputTokens)
                    .totalTokenCount(inputTokens + outputTokens)
                    .rawJson(usage.toString())
                    .build();
        } else {
            this.usageMetadata = ResponseUsageMetadata.builder().build();
        }

        AnthropicMessage msg = new AnthropicMessage(agi, modelId);
        msg.setResponse(this);
        msg.setRawJson(responseBody);
        
        JsonNode content = root.get("content");
        if (content != null && content.isArray()) {
            msg.parseFinalContent(content);
        }
        if (root.has("stop_reason")) {
            msg.setFinishReasonFromAnthropic(root.get("stop_reason").asText());
        }
        candidates.add(msg);
    }
    
    public AnthropicResponse(String rawRequestConfigJson, String rawHistoryJson, String chunkJson) {
        this.id = "stream";
        this.rawRequestConfigJson = rawRequestConfigJson;
        this.rawHistoryJson = rawHistoryJson;
        this.rawJson = chunkJson;
        this.usageMetadata = ResponseUsageMetadata.builder().build();
    }

    @Override
    public Optional<String> getPromptFeedback() {
        return Optional.empty();
    }

    @Override
    public int getTotalTokenCount() {
        return usageMetadata != null ? usageMetadata.getTotalTokenCount() : 0;
    }
}