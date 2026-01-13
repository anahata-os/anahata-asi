/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.tool.ToolManager;

/**
 * Represents a collection of related tools, parsed from a single Java class.
 * This is the core domain model for a "toolkit".
 *
 * @author anahata-gemini-pro-2.5
 * @param <T> The specific type of AbstractTool contained in this toolkit.
 */
@Getter
public abstract class AbstractToolkit<T extends AbstractTool> {
    @NonNull
    protected final ToolManager toolManager;
    
    protected String name;
    protected String description;
    protected int defaultRetention;
    private boolean enabled = true;

    protected AbstractToolkit(@NonNull ToolManager toolManager) {
        this.toolManager = toolManager;
    }
    
    /**
     * Gets all tools declared within this toolkit, regardless of their permission status.
     * @return The complete list of tools.
     */
    public abstract List<T> getAllTools();
    
    /**
     * Gets a list of tools that are allowed to be presented to the model.
     * This filters out tools that have a permanent {@link ToolPermission#DENY_NEVER} permission
     * and also returns an empty list if the entire toolkit is disabled.
     * 
     * @return A filtered list of allowed tools.
     */
    public List<T> getAllowedTools() {
        if (!enabled) {
            return Collections.emptyList();
        }
        return getAllTools().stream()
                .filter(tool -> tool.getPermission() != ToolPermission.DENY_NEVER)
                .collect(Collectors.toList());
    }

    /**
     * Calculates the total token count of this toolkit on-the-fly by aggregating
     * the token counts of all its contained tools.
     *
     * @return The total token count for the entire toolkit.
     */
    public int getTokenCount() {
        int totalTokens = 0;
        for (AbstractTool<?, ?> tool : getAllTools()) {
            totalTokens += tool.getTokenCount();
        }
        return totalTokens;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }
}
