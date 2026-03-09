/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool.spi.bad;

import java.util.Map;
import lombok.NonNull;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;

/**
 * Represents a call to a {@link BadTool}, capturing an invalid tool request
 * from the model.
 *
 * @author anahata-gemini-pro-2.5
 */
public class BadToolCall extends AbstractToolCall<BadTool, BadToolResponse> {

    public BadToolCall(AbstractModelMessage amm, @NonNull String id, @NonNull BadTool tool, @NonNull Map<String, Object> args) {
        super(amm, id, tool, args, args); // For bad tools, raw and enriched args are the same.
    }

    @Override
    protected BadToolResponse createResponse() {
        return new BadToolResponse(this);
    }

}
