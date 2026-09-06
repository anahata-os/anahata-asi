/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ollama;

import com.fasterxml.jackson.databind.JsonNode;
import java.time.Instant;

/**
 * Immutable record representing an available remote model listed in Ollama's online registry ({@code https://ollama.com/api/tags}).
 * 
 * @param name the unique model repository name and tag (e.g. "qwen2.5-coder:7b", "llama3.3:latest")
 * @param modifiedAt the timestamp when this model tag was last updated on the registry
 * @param size the size in bytes of the model archive
 * @author anahata
 */
public record OllamaRemoteModel(
        String name,
        Instant modifiedAt,
        long size
) {
    /**
     * Parses an {@link OllamaRemoteModel} from a JSON item node in the {@code models} array.
     * 
     * @param node the JSON object node
     * @return a populated {@link OllamaRemoteModel} instance
     */
    public static OllamaRemoteModel fromNode(JsonNode node) {
        String name = node.path("name").asText(node.path("model").asText("unknown"));
        long size = node.path("size").asLong(0L);
        Instant modified = null;
        String modStr = node.path("modified_at").asText(null);
        if (modStr != null && !modStr.isBlank()) {
            try {
                modified = Instant.parse(modStr);
            } catch (Exception ignored) {
            }
        }
        return new OllamaRemoteModel(name, modified, size);
    }
}
