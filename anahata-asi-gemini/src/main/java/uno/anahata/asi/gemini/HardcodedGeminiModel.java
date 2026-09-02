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
        this.supportedActions = new ArrayList<>(List.of("generateContent"));
    }
}
