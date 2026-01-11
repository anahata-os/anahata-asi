/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import lombok.Getter;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.tool.AbstractToolResponse;

/**
 * An event fired after a tool has finished executing.
 * 
 * @author anahata
 */
@Getter
public class ToolExecutionEndEvent extends ChatStatusEvent {
    
    private final AbstractToolResponse toolResponse;

    public ToolExecutionEndEvent(Chat source, AbstractToolResponse toolResponse) {
        super(source, ChatStatus.TOOL_EXECUTION_IN_PROGRESS, "Finished: " + toolResponse.getToolName());
        this.toolResponse = toolResponse;
    }
}
