/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import com.fasterxml.jackson.databind.JsonNode;
import uno.anahata.asi.agi.provider.AbstractModel;

/**
 * Official OpenAI provider implementation.
 * Uses the specialized {@link OpenAiModel} to handle legacy endpoints and 
 * advanced usage details.
 * 
 * @author anahata
 */
public class OpenAiProvider extends OpenAiCompatibleProvider {

    public OpenAiProvider() {
        super("OpenAI", "OpenAI (Official)", "https://api.openai.com/v1", "OpenAI", "https://platform.openai.com/api-keys");
    }

    @Override
    protected OpenAiCompatibleModel createModel(JsonNode node) {
        return new OpenAiModel(this, node);
    }
}
