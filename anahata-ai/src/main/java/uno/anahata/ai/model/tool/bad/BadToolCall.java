/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool.bad;

import java.util.Map;
import lombok.NonNull;
import uno.anahata.ai.model.tool.AbstractToolCall;

/**
 * Represents a call to a {@link BadTool}, capturing an invalid tool request
 * from the model.
 *
 * @author anahata-gemini-pro-2.5
 */
public class BadToolCall extends AbstractToolCall<BadTool, BadToolResponse> {

    public BadToolCall(@NonNull String id, @NonNull BadTool tool, @NonNull Map<String, Object> args) {
        super(id, tool, args);
    }

    @Override
    protected BadToolResponse createResponse() {
        return new BadToolResponse(this);
    }

    @Override
    protected int getDefaultTurnsToKeep() {
        return 1;
    }
}
