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

    /**
     * Constructs a new BadToolCall capturing the failed tool request details.
     *
     * @param amm The originating model message of the tool request.
     * @param id The unique identifier of the tool call.
     * @param tool The associated BadTool instance.
     * @param args The input arguments requested by the model.
     */
    public BadToolCall(AbstractModelMessage amm, String id, @NonNull BadTool tool, @NonNull Map<String, Object> args) {
        super(amm, id, tool, args, args); // For bad tools, raw and enriched args are the same.
    }

    @Override
    protected BadToolResponse createResponse() {
        return new BadToolResponse(this);
    }

}
