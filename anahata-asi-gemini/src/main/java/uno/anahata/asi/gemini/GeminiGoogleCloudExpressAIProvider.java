/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.genai.Chat;
import com.google.genai.Client;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.ListModelsConfig;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.gemini.GeminiModel;
import java.util.ArrayList;
import uno.anahata.asi.gemini.HardcodedGeminiModel;

/**
 * The concrete implementation of the {@code AbstractAgiProvider} for the Google
 * Gemini API. This class manages the native {@code Client} instance and handles
 * the discovery and listing of available Gemini models.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public class GeminiGoogleCloudExpressAIProvider extends GeminiAiProvider {


    /**
     * Constructs a new Gemini Express provider with the stable UUID 
     * 'GeminiGCExpress' and specialized Google Cloud acquisition URI.
     */
    public GeminiGoogleCloudExpressAIProvider() {
        super("GeminiGCExpress", "Google Cloud Express Mode", true);
        setTokenizerType(TokenizerType.GEMINI);
        setKeysAcquisitionUri("https://console.cloud.google.com/expressmode");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns a hardcoded manifest of {@link HardcodedGeminiModel} 
     * instances because Vertex Express mode does not support model discovery.
     * </p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        List<HardcodedGeminiModel> manifest = new ArrayList<>();
        
        manifest.add(createModel("gemini-3.1-pro-preview", "Gemini 3.1 Pro (Preview)", "v3.1", 2097152, 65000));        
        manifest.add(createModel("gemini-3.5-flash", "Gemini 3.5 Flash", "v3.5", 1048576, 65000));
        
        manifest.add(createModel("gemini-3.1-flash-image-preview", "Gemini 3.1 Flash Image (Preview)", "v3.1", 1048576, 8192));
        manifest.add(createModel("gemini-3-pro-preview", "Gemini 3 Pro (Preview)", "v3", 2097152, 8192));
        manifest.add(createModel("gemini-3-flash-preview", "Gemini 3 Flash (Preview)", "v3", 1048576, 8192));
        manifest.add(createModel("gemini-2.5-pro", "Gemini 2.5 Pro", "v2.5", 2097152, 8192));
        manifest.add(createModel("gemini-2.5-flash", "Gemini 2.5 Flash", "v2.5", 1048576, 8192));
        manifest.add(createModel("gemini-2.5-flash-lite", "Gemini 2.5 Flash Lite", "v2.5", 1048576, 8192));
        manifest.add(createModel("gemini-2.0-flash-001", "Gemini 2.0 Flash", "v2.0", 1048576, 8192));
        
        // Add standard 1.5 models as they are usually the stable workhorses
        manifest.add(createModel("gemini-1.5-pro", "Gemini 1.5 Pro", "v1.5", 2097152, 8192));
        manifest.add(createModel("gemini-1.5-flash", "Gemini 1.5 Flash", "v1.5", 1048576, 8192));
        manifest.add(createModel("gemini-1.5-flash-8b", "Gemini 1.5 Flash 8B", "v1.5", 1048576, 8192));

        return manifest;
    }

    /**
     * Internal factory method to create a HardcodedGeminiModel with 
     * pre-defined capabilities.
     * @param id          The model identifier.
     * @param name        The display name.
     * @param version     The version string.
     * @param inputLimit  The context window limit.
     * @param outputLimit The single-turn output limit.
     * @return A configured HardcodedGeminiModel.
     */
    private HardcodedGeminiModel createModel(String id, String name, String version, int inputLimit, int outputLimit) {
        HardcodedGeminiModel m = new HardcodedGeminiModel(this, id);
        m.setDisplayName(name);
        m.setVersion(version);
        m.setMaxInputTokens(inputLimit);
        m.setMaxOutputTokens(outputLimit);
        m.setSupportedActions(List.of("generateContent", "countTokens"));
        return m;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides a template configuration for the Gemini api_keys.txt file.
     * </p>
     */
    @Override
    public String getApiKeyHint() {
        return "# Google Cloud Express Mode API Key Configuration\n"
                + "# -----------------------------\n"
                + "# Add one key per line.\n"
                + "# Lines starting with '#' are discarded.\n"
                + "# You can put comments after each key using '//'.\n"
                + "\n"
                + "AQ.Ab8RN6Iadadadadad123123j9HoXaQPi_01aqh9A// main_key\n"
                + "AQ.Ab8RN6Ieoomc123312313cFmlfSZiTRj9HoXaQPi_01aqh9A // backup_key";
    }

    /*
    public static void main(String[] args) {
        Chat chat = c.chats.create("gemini-3-flash-preview");
        GenerateContentResponse resp = chat.sendMessage("hi");
        System.out.println(resp);
    }*/
}
