/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.Chat;
import com.google.genai.Client;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.HttpOptions;
import com.google.genai.types.ListModelsConfig;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.TokenizerType;

/**
 * The concrete implementation of the {@code AbstractAgiProvider} for the Google
 * Gemini API. This class manages the native {@code Client} instance and handles
 * the discovery and listing of available Gemini models.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public class GeminiAiProvider extends AbstractAiProvider {

    /**
     * The native Google GenAI client instance. transient to prevent serialization.
     */
    private transient Client client;

    /**
     * Whether this provider is configured to use Google Cloud Vertex AI 
     * instead of the standard AI Studio endpoint.
     */
    @Setter
    private boolean vertex = false;

    /**
     * Creates a new Gemini Provider with default settings.
     */
    public GeminiAiProvider() {
        this(java.util.UUID.randomUUID().toString(), "Google Gemini", false);
    }

    /**
     * Craetes a new Gemini Provider using the Official Google Genai Java SDK.
     *
     * @param uuid registration id
     * @param displayName
     * @param vertex if true will point to vertex express.
     */
    public GeminiAiProvider(String uuid, String displayName, boolean vertex) {
        super(uuid);
        this.vertex = vertex;
        setDisplayName(displayName);
        setDescription(vertex ? "Official Google Cloud Vertex AI client." : "Official Google Gemini API client supporting Flash, Pro, and Ultra.");
        setTokenizerType(TokenizerType.GEMINI);
        setKeysAcquisitionUri(vertex ? "https://console.cloud.google.com/agent-platform/overview" : "https://aistudio.google.com/app/apikey");
    }

    /**
     * Gets the native Gemini API client, creating it lazily if necessary.
     *
     * @return The native {@code Client} instance.
     */
    public synchronized Client getClient() {
        if (client == null) {

            if (isApiKeyRequired()) {
                String nextKey = getNextKey();
                log.info("Got api key from " + getUuid() + " " + StringUtils.abbreviate(nextKey, 8));
                if (nextKey != null) {
                    Client.Builder builder = Client.builder()
                            .vertexAI(vertex)
                            .apiKey(nextKey);
                    if (getBaseUrl() != null && !getBaseUrl().isBlank()) {
                        builder.httpOptions(HttpOptions.builder().baseUrl(getBaseUrl()).build());
                    }
                    client = builder.build();
                } else {
                    throw new IllegalStateException("Could not load an API key for Gemini. Check " + getKeysFilePath());
                }
            } else {
                client = Client.builder()
                            .vertexAI(vertex)
                            .build();
                }            

        }
        return client;
    }

    /**
     * {@inheritDoc}
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
                + "AIzaSyA9B8C7D6E5F4G3H2I1J0K9L8M7N6O5P4Q // backup_key"
                + "AIzaSyB1C2D3E4F5G6H7I8J9K0L1M2N3O4P5Q6R // secodary_backup_key\n";
    }
    
    /*

    public static void main(String[] args) {
        Client c = new GeminiAiProvider(true).getClient();
        Chat chat = c.chats.create("gemini-3-flash-preview");
        GenerateContentResponse resp = chat.sendMessage("hi");
        System.out.println(resp);
    }*/
}
