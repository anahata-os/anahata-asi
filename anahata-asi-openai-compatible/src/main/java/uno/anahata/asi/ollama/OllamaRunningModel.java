/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ollama;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Immutable record representing an active model loaded into host memory or GPU VRAM by Ollama.
 * 
 * @param name the model identifier name
 * @param model the exact model tag or path
 * @param size the total size in bytes occupied in system RAM/VRAM
 * @param sizeVram the size in bytes occupied specifically in GPU VRAM
 * @param contextLength the active context window allocated for this instance
 * @param expiresAt the timestamp string when Ollama will automatically unload the model from memory
 * @param parameterSize parameter scale string (e.g., "9.0B", "35B")
 * @param quantizationLevel quantization format (e.g., "Q4_K_M", "IQ4_XS")
 * @author anahata
 */
public record OllamaRunningModel(
        String name,
        String model,
        long size,
        long sizeVram,
        Integer contextLength,
        String expiresAt,
        String parameterSize,
        String quantizationLevel
) {
    /**
     * Parses an Ollama running model item from a JSON node returned by the {@code /api/ps} endpoint.
     * 
     * @param node the JSON node from the {@code models} array
     * @return a populated {@link OllamaRunningModel} instance
     */
    public static OllamaRunningModel fromNode(JsonNode node) {
        String name = node.path("name").asText(node.path("model").asText("unknown"));
        String model = node.path("model").asText(name);
        long size = node.path("size").asLong(0L);
        long sizeVram = node.path("size_vram").asLong(0L);
        Integer contextLength = node.hasNonNull("context_length") ? node.get("context_length").asInt() : null;
        String expiresAt = node.path("expires_at").asText("");

        JsonNode details = node.path("details");
        String paramSize = details.path("parameter_size").asText("N/A");
        String quant = details.path("quantization_level").asText("N/A");

        return new OllamaRunningModel(name, model, size, sizeVram, contextLength, expiresAt, paramSize, quant);
    }
}
