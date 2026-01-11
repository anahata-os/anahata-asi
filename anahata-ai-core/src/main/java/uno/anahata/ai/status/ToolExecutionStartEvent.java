/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.status;

import lombok.Getter;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.tool.AbstractToolCall;

/**
 * An event fired just before a tool is executed.
 * 
 * @author anahata
 */
@Getter
public class ToolExecutionStartEvent extends ChatStatusEvent {
    
    private final AbstractToolCall toolCall;

    public ToolExecutionStartEvent(Chat source, AbstractToolCall toolCall) {
        super(source, ChatStatus.TOOL_EXECUTION_IN_PROGRESS, "Starting: " + toolCall.getToolName());
        this.toolCall = toolCall;
    }
}
