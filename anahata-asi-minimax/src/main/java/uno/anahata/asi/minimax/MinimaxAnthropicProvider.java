package uno.anahata.asi.minimax;

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
     * Constructs a new MinimaxAnthropicProvider.
     * <p>
     * Initializes the provider with the MiniMax-specific base URL and configuration endpoints.
     * </p>
     */
    public MinimaxAnthropicProvider() {
        super("Minimax", "MiniMax", "https://api.minimax.io/anthropic/v1", "2023-06-01", "https://platform.minimax.io/user-center/basic-information/interface-key");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides the specific API key format hint for MiniMax.
     * </p>
     */
    @Override
    public String getApiKeyHint() {
        return "# MiniMax API Key Configuration\n"
                + "ey...\n";
    }
}