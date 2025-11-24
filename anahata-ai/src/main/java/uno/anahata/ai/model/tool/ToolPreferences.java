/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool;

import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;

/**
 * A serializable POJO that acts as a container for all user-configured tool preferences.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public class ToolPreferences {

    /**
     * A map where the key is the tool name (e.g., "LocalFiles.readFile") and the value
     * is the user's stored preference for that tool.
     */
    private Map<String, ToolPermission> toolPermissions = new HashMap<>();

}
