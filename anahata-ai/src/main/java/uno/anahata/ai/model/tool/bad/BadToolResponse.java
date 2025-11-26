/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool.bad;

import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolExecutionStatus;

/**
 * The response for a {@link BadToolCall}. Its status is immediately set to
 * {@link ToolExecutionStatus#NOT_EXECUTED} and its execute method is a no-op.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class BadToolResponse extends AbstractToolResponse<BadToolCall> {

    @NonNull
    private final BadToolCall call;

    public BadToolResponse(@NonNull BadToolCall call) {
        super(call);
        this.call = call;
        setStatus(ToolExecutionStatus.NOT_EXECUTED);
        setError("Tool call rejected: The tool '" + call.getName() + "' was not found.");
    }

    @Override
    public BadToolCall getCall() {
        return call;
    }

    @Override
    public void execute() {
        // No-op, as the tool was never found.
    }
    
    @Override
    protected int getDefaultTurnsToKeep() {
        return 1;
    }
}
