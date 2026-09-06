/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ollama;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Immutable record representing download and verification progress reported by Ollama's {@code /api/pull} endpoint.
 * 
 * @param status the current progress status description (e.g., "pulling manifest", "downloading", "verifying sha256 digest")
 * @param digest the sha256 digest of the layer currently being downloaded
 * @param total the total size in bytes of the current layer
 * @param completed the number of bytes downloaded so far
 * @param error the error message if the pull operation failed
 * @author anahata
 */
public record OllamaPullProgress(
        String status,
        String digest,
        long total,
        long completed,
        String error
) {
    /**
     * Calculates the completion percentage for the current layer.
     * 
     * @return the percentage from 0 to 100, or -1 if the total byte count is unknown
     */
    public int getPercent() {
        if (total > 0) {
            return (int) ((completed * 100L) / total);
        }
        return -1;
    }

    /**
     * Parses an Ollama pull progress event from an SSE JSON chunk.
     * 
     * @param node the JSON node received from the stream
     * @return a populated {@link OllamaPullProgress} instance
     */
    public static OllamaPullProgress fromNode(JsonNode node) {
        String status = node.path("status").asText("");
        String digest = node.path("digest").asText("");
        long total = node.path("total").asLong(0L);
        long completed = node.path("completed").asLong(0L);
        String error = node.hasNonNull("error") ? node.get("error").asText() : null;
        return new OllamaPullProgress(status, digest, total, completed, error);
    }
}
