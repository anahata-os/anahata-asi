/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import uno.anahata.asi.agi.message.ResponseUsageMetadata;
import uno.anahata.asi.agi.provider.Response;

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

    public AnthropicResponse(String id, String rawJson, String rawRequestConfigJson, String rawHistoryJson, ResponseUsageMetadata usageMetadata) {
        this.id = id;
        this.rawJson = rawJson;
        this.rawRequestConfigJson = rawRequestConfigJson;
        this.rawHistoryJson = rawHistoryJson;
        this.usageMetadata = usageMetadata;
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
