/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.Client;
import com.google.genai.types.ListModelsConfig;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Getter;
import uno.anahata.asi.agi.provider.AbstractAgiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;

/**
 * The concrete implementation of the {@code AbstractAgiProvider} for the Google
 * Gemini API. This class manages the native {@code Client} instance and handles
 * the discovery and listing of available Gemini models.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class GeminiAgiProvider extends AbstractAgiProvider {

    private transient Client client;

    public GeminiAgiProvider() {
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
     * {@inheritDoc}
     * <p>
     * Implementation details: Nullifies the internal Gemini client. This forces 
     * the provider to rotate to the next API key in the pool and reconstruct 
     * the client on the very next generation request.
     * </p>
     */
    @Override
    public synchronized void hokusPocus() {
        client = null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Synchronously fetches the list of available 
     * Generative Models from the Google GenAI service. Each native model is 
     * wrapped in a {@link GeminiModel} adapter.
     * </p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        var pager = getClient().models.list(ListModelsConfig.builder().build());
        return StreamSupport.stream(pager.spliterator(), false)
                .map(model -> (AbstractModel) new GeminiModel(this, model))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the official Google AI Studio API key management URI.
     * </p>
     */
    @Override
    public java.net.URI getKeysAcquisitionUri() {
        try {
            return new java.net.URI("https://aistudio.google.com/app/apikey");
        } catch (java.net.URISyntaxException e) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides a template configuration for the Gemini api_keys.txt file.
     * </p>
     */
    @Override
    public String getApiKeyHint() {
        return "# Gemini API Key Configuration\n"
                + "# -----------------------------\n"
                + "# Add one key per line.\n"
                + "# Lines starting with '#' are discarded.\n"
                + "# You can put comments after each key using '//'.\n"
                + "\n"
                + "AIzaSyB1C2D3E4F5G6H7I8J9K0L1M2N3O4P5Q6R // main_key\n"
                + "AIzaSyA9B8C7D6E5F4G3H2I1J0K9L8M7N6O5P4Q // backup_key";
    }
}
