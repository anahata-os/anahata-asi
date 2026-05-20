package uno.anahata.asi.minimax;

import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.anthropic.AnthropicProvider;

/**
 * A provider implementation for the MiniMax API leveraging their Anthropic-compatible endpoints.
 * <p>
 * MiniMax offers high-performance models (like M2.7, M2.5) and exposes an Anthropic-compatible 
 * API interface. This class extends {@link uno.anahata.asi.anthropic.AnthropicProvider} to 
 * reuse the robust Anthropic payload generation and SSE parsing, simply overriding the 
 * base URL and API key hints.
 * </p>
 *
 * @author anahata
 */
public class MinimaxAnthropicProvider extends AnthropicProvider {

    /**
     * {@inheritDoc}
     * <p>Implementation details: Configures the base Anthropic provider to point to 
     * MiniMax's endpoint ({@code https://api.minimax.io/anthropic/v1}) and sets 
     * the appropriate documentation URI for API key acquisition.</p>
     */
    public MinimaxAnthropicProvider() {
        super("Minimax", "MiniMax (Anthropic)", "https://api.minimax.io/anthropic/v1", "2023-06-01", "https://platform.minimax.io/user-center/basic-information/interface-key");
        setFolderName(AbstractAsiContainer.getWorkDirSubDir("Minimax").toString());
    }

    /**
     * {@inheritDoc}
     * <p>Implementation details: Provides the MiniMax-specific key format including 
     * the {@code sk-api-} prefix and suggested backup key labeling.</p>
     * @return A multi-line string containing configuration instructions and examples.
     */
    @Override
    public String getApiKeyHint() {
        return "# MiniMax API Key Configuration\n"
                + "sk-api-ddYQ_z36...//main\n"
                + "sk-api-I3TuXlWN...//backup\n"
                + "sk-api-I3Tsdsd1...//backup of the backup\n"
                ;
    }
}