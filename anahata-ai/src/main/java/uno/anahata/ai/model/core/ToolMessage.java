/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import java.util.List;
import java.util.stream.Collectors;
import uno.anahata.ai.model.tool.AbstractToolResponse;

/**
 * Represents a message containing the results of tool executions.
 * This message is sent from the client back to the model after function calls.
 *
 * @author anahata-gemini-pro-2.5
 */
public class ToolMessage extends AbstractMessage {
    @Override
    public Role getRole() {
        return Role.TOOL;
    }

    /**
     * Filters and returns only the tool response parts from this message.
     * @return A list of {@link AbstractToolResponse} parts, or an empty list if none exist.
     */
    @SuppressWarnings("unchecked")
    public List<AbstractToolResponse> getToolResponses() {
        return getParts().stream()
                .filter(AbstractToolResponse.class::isInstance)
                .map(p -> (AbstractToolResponse) p)
                .collect(Collectors.toList());
    }
}
