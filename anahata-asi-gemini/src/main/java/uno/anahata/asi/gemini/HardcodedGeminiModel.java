/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.types.Model;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * A specialized {@link GeminiModel} that uses hardcoded metadata instead of
 * fetching it from the Gemini API. This is useful for restricted environments
 * like "Google Vertex Express" which do not support model discovery (listing).
 *
 * @author anahata
 */
@Getter
@Setter
public class HardcodedGeminiModel extends GeminiModel {

    /**
     * Constructs a new hardcoded model instance.
     * @param provider The owning Gemini provider.
     * @param modelId  The unique model identifier.
     */
    public HardcodedGeminiModel(GeminiAiProvider provider, String modelId) {
        super(provider, Model.builder().name(modelId).build());
        this.supportedActions = new ArrayList<>(List.of("generateContent", "countTokens"));
    }

    /**
     * Constructs a new hardcoded model instance with full metadata.
     * 
     * @param provider        The owning Gemini provider.
     * @param modelId         The unique model identifier.
     * @param displayName     The human-readable display name.
     * @param description     The model description.
     * @param version         The version string.
     * @param maxInputTokens  The context window limit.
     * @param maxOutputTokens The single-turn output limit.
     */
    public HardcodedGeminiModel(GeminiAiProvider provider, String modelId, String displayName, 
            String description, String version, int maxInputTokens, int maxOutputTokens) {
        super(provider, Model.builder()
                .name(modelId)
                .displayName(displayName)
                .description(description)
                .version(version)
                .inputTokenLimit(maxInputTokens)
                .outputTokenLimit(maxOutputTokens)
                .supportedActions(List.of("generateContent", "countTokens"))
                .build());
        this.displayName = displayName;
        this.description = description;
        this.version = version;
        this.maxInputTokens = maxInputTokens;
        this.maxOutputTokens = maxOutputTokens;
        this.supportedActions = new ArrayList<>(List.of("generateContent", "countTokens"));
    }
}
