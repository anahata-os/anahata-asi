/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.provider;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents a server-side tool provided by an AI model (e.g., Google Search, Code Execution).
 * This allows the UI to dynamically present options for enabling/disabling specific
 * server-side capabilities.
 * 
 * @author anahata
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ServerTool {
    /** 
     * The unique identifier for the tool. 
     * Can be a String, a Class literal, or any other identifying object.
     */
    private Object id;
    /** The human-readable display name (e.g., "Google Search"). */
    private String displayName;
    /** A brief description of what the tool does. */
    private String description;
}
