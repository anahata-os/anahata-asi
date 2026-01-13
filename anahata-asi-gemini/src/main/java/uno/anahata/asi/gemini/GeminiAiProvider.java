/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.Client;
import com.google.genai.types.ListModelsConfig;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Getter;
import uno.anahata.asi.model.provider.AbstractAiProvider;
import uno.anahata.asi.model.provider.AbstractModel;

/**
 * The concrete implementation of the {@code AbstractAiProvider} for the Google
 * Gemini API. This class manages the native {@code Client} instance and handles
 * the discovery and listing of available Gemini models.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class GeminiAiProvider extends AbstractAiProvider {

    private Client client;

    public GeminiAiProvider() {
        super("Gemini");
    }

    /**
     * Gets the native Gemini API client, creating it lazily if necessary.
     *
     * @return The native {@code Client} instance.
     */
    public synchronized Client getClient() {
        if (client == null) {
            String nextKey = getNextKey();
            if (nextKey != null) {
                client = Client.builder()
                        .apiKey(nextKey)
                        .build();
            } else {
                throw new IllegalStateException("Could not load an API key for Gemini. Check " + getKeysFilePath());
            }

        }
        return client;
    }

    /**
     * Returns the api key being used by the current genai Client
     * @return the api key in use
     */
    @Override
    public String getCurrentApiKey() {
        return getClient().apiKey();
    }

    /**
     * Resets the client to null to force a new key on the next call.
     */
    public synchronized void resetClient() {
        client = null;
    }

    @Override
    public List<? extends AbstractModel> listModels() {
        var pager = getClient().models.list(ListModelsConfig.builder().build());
        return StreamSupport.stream(pager.spliterator(), false)
                .map(model -> (AbstractModel) new GeminiModel(this, model))
                .collect(Collectors.toList());
    }
}
