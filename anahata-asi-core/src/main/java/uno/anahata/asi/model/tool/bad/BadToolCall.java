/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool.bad;

import java.util.Map;
import lombok.NonNull;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.tool.AbstractToolCall;

/**
 * Represents a call to a {@link BadTool}, capturing an invalid tool request
 * from the model.
 *
 * @author anahata-gemini-pro-2.5
 */
public class BadToolCall extends AbstractToolCall<BadTool, BadToolResponse> {

    public BadToolCall(AbstractModelMessage amm, @NonNull String id, @NonNull BadTool tool, @NonNull Map<String, Object> args) {
        super(amm, id, tool, args);
    }

    @Override
    protected BadToolResponse createResponse(AbstractToolMessage toolMessage) {
        return new BadToolResponse(this);
    }

    @Override
    protected int getDefaultTurnsToKeep() {
        return 1;
    }
}
