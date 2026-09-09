/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openrouter;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;

/**
 * Dedicated AI Provider for OpenRouter (openrouter.ai) using their OpenAI-compatible API.
 * <p>
 * Connects to {@code https://openrouter.ai/api/v1} to access hundreds of models from
 * OpenAI, Anthropic, Google, Meta, DeepSeek, xAI (Grok), Mistral, and open-source communities.
 * Includes automated discovery of context lengths, provider output ceilings, pricing,
 * and multimodal capabilities directly from the {@code /v1/models} endpoint.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class OpenRouterAiProvider extends OpenAiChatCompletionsProvider {

    /**
     * Constructs a new OpenRouter provider with stable UUID "OpenRouter".
     */
    public OpenRouterAiProvider() {
        super("OpenRouter", "OpenRouter", "https://openrouter.ai/api/v1", "https://openrouter.ai/keys");
        setPriority(15);
        setDescription("OpenRouter unified model aggregator with intelligent routing, live benchmarks, and competitive pricing.");
        getCustomHeaders().put("HTTP-Referer", "https://anahata.uno");
        getCustomHeaders().put("X-Title", "Anahata ASI");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Instantiates an {@link OpenRouterModel} capable of extracting rich capability,
     * pricing, and context limits from OpenRouter model metadata.
     * </p>
     * 
     * @param node The raw JSON model descriptor.
     * @return A configured {@link OpenRouterModel} instance.
     */
    @Override
    protected OpenAiCompatibleModel createModel(JsonNode node) {
        return new OpenRouterModel(this, node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides the OpenRouter-specific API key format template.
     * </p>
     * 
     * @return Multi-line template string.
     */
    @Override
    public String getApiKeyHint() {
        return "# OpenRouter API Key Configuration\n"
                + "# Add your keys below (one per line)\n"
                + "sk-or-v1-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx // primary key\n";
    }
}
