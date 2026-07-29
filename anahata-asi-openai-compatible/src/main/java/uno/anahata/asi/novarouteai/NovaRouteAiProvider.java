package uno.anahata.asi.novarouteai;

import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;

/**
 * A provider implementation for NovarouteAI leveraging their OpenAI-compatible endpoints.
 * <p>
 * NovarouteAI offers access to open-source Chinese models and exposes an OpenAI-compatible 
 * API interface. This class extends {@link uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider} to 
 * reuse the robust OpenAI payload generation and SSE parsing, simply overriding the 
 * base URL and API key hints.
 * </p>
 *
 * @author anahata
 */
public class NovaRouteAiProvider extends OpenAiChatCompletionsProvider {

    /**
     * Constructs a new NovarouteAI OpenAI-compatible provider.
     * <p>
     * Configures the base OpenAI-compatible provider to point to NovarouteAI's endpoint 
     * ({@code https://api.novarouteai.com/v1}) and sets the appropriate 
     * documentation URI for API key acquisition.
     * </p>
     */
    public NovaRouteAiProvider() {
        super("NovaRouteAI", "NovaRouteAI", "https://novarouteai.com/v1", "NovaRouteAI", "https://novarouteai.com/register?aff=UBCS7HL727XC");
        setDescription("OpenAI-compatible access to selected official Chinese models");
        setFolderName(AbstractAsiContainer.getWorkDirSubDir("NovaRouteAI").toString());
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Provides the NovarouteAI-specific key format and suggested backup key labeling.</p>
     * @return A multi-line string containing configuration instructions and examples.
     */
    @Override
    public String getApiKeyHint() {
        return "# NovarouteAI API Key Configuration\n"
                + "sk-xxxxxxxxxxxxxxxxxxxx // main\n"
                + "sk-yyyyyyyyyyyyyyyyyyyy // backup\n"
                ;
    }
}
