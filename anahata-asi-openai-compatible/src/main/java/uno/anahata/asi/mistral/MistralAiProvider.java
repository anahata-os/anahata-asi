/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.mistral;

import com.fasterxml.jackson.databind.JsonNode;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;

/**
 * Dedicated AI Provider for Mistral AI using their native OpenAI-compatible API.
 * <p>
 * This provider parses rich capability flags, FIM endpoints, reasoning tags,
 * and context lengths directly from Mistral's /v1/models JSON endpoint.
 * </p>
 * 
 * @author anahata
 */
public class MistralAiProvider extends OpenAiChatCompletionsProvider {

    /**
     * Constructs a new Mistral AI provider with stable UUID "Mistral".
     */
    public MistralAiProvider() {
        super("Mistral", "Mistral AI", "https://api.mistral.ai/v1", "Mistral", "https://console.mistral.ai/api-keys");
        setDescription("Official Mistral AI provider supporting frontier multimodal, reasoning, vision, and coding models.");
        setFolderName(AbstractAsiContainer.getWorkDirSubDir("Mistral").toString());
    }

    /**
     * {@inheritDoc}
     * <p>Creates a specialized {@link MistralModel} that parses capabilities, context lengths, and FIM endpoints.</p>
     */
    @Override
    protected OpenAiCompatibleModel createModel(JsonNode node) {
        return new MistralModel(this, node);
    }

    /**
     * {@inheritDoc}
     * <p>Provides the Mistral-specific key hint for the api_keys.txt configuration file.</p>
     */
    @Override
    public String getApiKeyHint() {
        return "# Mistral AI API Key Configuration\n"
                + "# Add your keys below (one per line)\n"
                + "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx // croissant.delicious.108@gmail.com\n"
                + "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy // eiffel.fan@gmail.com\n"
                + "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz // mbappe.merci.beaucoup@gmail.com\n";
    }
}
