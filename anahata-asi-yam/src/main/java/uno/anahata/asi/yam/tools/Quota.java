/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.yam.tools;

import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A toolkit for managing API quota and key rotation across multiple providers.
 * Essential for High-ROI operations with limited free-tier quotas.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for managing API quota and key rotation.")
public class Quota extends AnahataToolkit {
    
    private final Map<String, Long> keyUsage = new ConcurrentHashMap<>();

    /**
     * Logs the token usage for a specific API key.
     * 
     * @param keyId A unique identifier for the API key (e.g., "Key-1").
     * @param tokens The number of tokens consumed in the last turn.
     */
    @AiTool("Logs the token usage for a specific API key.")
    public void logUsage(
            @AiToolParam("The unique identifier for the API key.") String keyId, 
            @AiToolParam("The number of tokens consumed.") long tokens) {
        keyUsage.merge(keyId, tokens, Long::sum);
        log.info("Quota Update: Key '{}' consumed {} tokens. Total usage: {}", keyId, tokens, keyUsage.get(keyId));
    }

    /**
     * Returns the current usage statistics for all tracked keys.
     * 
     * @return A map of key IDs to total tokens consumed.
     */
    @AiTool("Returns the current usage statistics for all tracked keys.")
    public Map<String, Long> getUsageStats() {
        return new ConcurrentHashMap<>(keyUsage);
    }

    /**
     * Resets the usage statistics for a specific key (e.g., after a daily reset).
     * 
     * @param keyId The identifier of the key to reset.
     */
    @AiTool("Resets the usage statistics for a specific key.")
    public void resetUsage(@AiToolParam("The identifier of the key to reset.") String keyId) {
        keyUsage.remove(keyId);
        log.info("Quota Reset: Usage for key '{}' has been cleared.", keyId);
    }
}
