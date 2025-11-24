/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini;

import com.google.genai.Client;
import com.google.genai.types.ListModelsConfig;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;

/**
 * The concrete implementation of the {@code AbstractAiProvider} for the Google Gemini API.
 * This class manages the native {@code Client} instance and handles the discovery
 * and listing of available Gemini models.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class GeminiAiProvider extends AbstractAiProvider {

    private Client client;

    public GeminiAiProvider(@NonNull Chat chat) {
        super(chat, "gemini");
        // Fail fast if no API key is configured.
        if (getApiKey() == null) {
            throw new IllegalStateException("GEMINI_API_KEY is not set or the api_keys.txt file is missing or empty.");
        }
    }

    /**
     * Gets the native Gemini API client, creating it lazily if necessary.
     * @return The native {@code Client} instance.
     */
    public synchronized Client getClient() {
        if (client == null) {
            client = Client.builder()
                    .apiKey(getApiKey())
                    .build();
        }
        return client;
    }

    @Override
    public List<? extends AbstractModel> listModels() {
        var pager = getClient().models.list(ListModelsConfig.builder().build());
        return StreamSupport.stream(pager.spliterator(), false)
                .map(model -> new GeminiModel(this, model))
                .collect(Collectors.toList());
    }
}
