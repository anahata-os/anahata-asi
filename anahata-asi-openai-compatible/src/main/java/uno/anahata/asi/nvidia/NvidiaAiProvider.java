/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nvidia;

import com.fasterxml.jackson.databind.JsonNode;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;

/**
 * Dedicated AI Provider for NVIDIA NIM microservices using their OpenAI-compatible API.
 * <p>
 * Connects to {@code https://integrate.api.nvidia.com/v1} to access NVIDIA NIM models 
 * including Nemotron, DeepSeek-R1, Qwen, and Llama reasoning models.
 * </p>
 *
 * @author anahata
 */
public class NvidiaAiProvider extends OpenAiChatCompletionsProvider {

    /**
     * Constructs a new NVIDIA AI provider with stable UUID "Nvidia".
     */
    public NvidiaAiProvider() {
        super("Nvidia", "NVIDIA", "https://integrate.api.nvidia.com/v1", "Nvidia", "https://build.nvidia.com/models");
        setDescription("NVIDIA NIM microservices offering high-performance AI models.");
        setFolderName(AbstractAsiContainer.getWorkDirSubDir("Nvidia").toString());
    }

    /**
     * {@inheritDoc}
     * <p>Creates a specialized {@link NvidiaModel} that configures reasoning and thought extraction.</p>
     */
    @Override
    protected OpenAiCompatibleModel createModel(JsonNode node) {
        return new NvidiaModel(this, node);
    }

    /**
     * {@inheritDoc}
     * <p>Adds NVIDIA-specific error checks for GPU VRAM exhaustion and engine loop failures.</p>
     */
    @Override
    public boolean isRetryable(int statusCode, String responseBody) {
        if (super.isRetryable(statusCode, responseBody)) {
            return true;
        }
        if (responseBody != null) {
            String lower = responseBody.toLowerCase();
            return lower.contains("out of memory") 
                || lower.contains("resourceexhausted") 
                || lower.contains("engine loop") 
                || lower.contains("cuda");
        }
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>Provides the NVIDIA-specific key hint for the api_keys.txt configuration file.</p>
     */
    @Override
    public String getApiKeyHint() {
        return "# NVIDIA API Key Configuration\n"
                + "# Add your keys below (one per line)\n"
                + "nvapi-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx // main key\n";
    }
}
