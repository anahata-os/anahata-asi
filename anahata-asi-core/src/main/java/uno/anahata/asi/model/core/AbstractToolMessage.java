/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolResponse;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.status.ChatStatus;

/**
 * Represents a message containing the results of tool executions.
 * This message is sent from the client back to the model after function calls.
 * It holds a direct reference to the {@link AbstractModelMessage} that initiated the tool calls.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public abstract class AbstractToolMessage<T extends AbstractModelMessage> extends AbstractMessage {
    /**
     * The AbstractModelMessage that contains the tool calls this message is responding to.
     */
    private final T modelMessage;

    public AbstractToolMessage(T modelMessage) {
        super(modelMessage.getChat());
        this.modelMessage = modelMessage;
        modelMessage.setToolMessage(this);
    }
    
    
    @Override
    public Role getRole() {
        return Role.TOOL;
    }

    @Override
    public String getFrom() {
        return System.getProperty("user.name");
    }

    /**
     * Filters and returns only the tool response parts from this message.
     * @return A list of {@link AbstractToolResponse} parts, or an empty list if none exist.
     */
    public List<AbstractToolResponse<?>> getToolResponses() {
        return getParts().stream()
                .filter(AbstractToolResponse.class::isInstance)
                .map(p -> (AbstractToolResponse<?>) p)
                .collect(Collectors.toList());
    }
    
    /**
     * Determines if this entire batch of tool calls can be executed automatically
     * without user intervention.
     * @return {@code true} if all conditions for automatic execution are met.
     */
    public boolean isAutoRunnable() {
        // Condition 1: Check global configuration settings.
        if (getModelMessage().isStreaming()) {
            return false;
        }
        
        if (!getChat().getConfig().isLocalToolsEnabled()) {
            return false;
        }
        
        List<AbstractToolResponse<?>> responses = getToolResponses();
        if (responses.isEmpty()) {
            return false; // Nothing to run.
        }
        
        // Conditions 2 & 3: Check every single tool response.
        for (AbstractToolResponse<?> response : responses) {
            AbstractTool<?, ?> tool = response.getCall().getTool();
            if (tool.getPermission() != ToolPermission.APPROVE_ALWAYS || response.getStatus() != ToolExecutionStatus.PENDING) {
                return false; // If any tool requires a prompt or is not pending, the batch is not auto-runnable.
            }
        }
        
        // If all checks pass, the batch can be auto-run.
        return true;
    }
    
    /**
     * Checks if there are any tool responses in a PENDING state that have the APPROVE_ALWAYS permission.
     * @return {@code true} if at least one approved tool is pending.
     */
    public boolean hasApprovedPendingTools() {
        return getToolResponses().stream()
                .anyMatch(r -> r.getStatus() == ToolExecutionStatus.PENDING && 
                               r.getCall().getTool().getPermission() == ToolPermission.APPROVE_ALWAYS);
    }
    
    /**
     * Executes all tool responses in this message that are currently in a PENDING state.
     */
    public void executeAllPending() {
        getToolResponses().stream()
            .filter(response -> response.getStatus() == ToolExecutionStatus.PENDING)
            .forEach(response -> response.execute());
    }

    /**
     * Processes all tool responses in this message that are currently in a PENDING state.
     * Tools with APPROVE_ALWAYS permission are executed, while others are rolled to NOT_EXECUTED.
     * This method also updates the chat status if any tools are executed.
     */
    public void processPendingTools() {
        if (hasApprovedPendingTools()) {
            getChat().getStatusManager().fireStatusChanged(ChatStatus.AUTO_EXECUTING_TOOLS);
        }
        
        for (AbstractToolResponse<?> response : getToolResponses()) {
            if (response.getStatus() == ToolExecutionStatus.PENDING) {
                if (response.getCall().getTool().getPermission() == ToolPermission.APPROVE_ALWAYS) {
                    response.execute();
                } else {
                    response.setStatus(ToolExecutionStatus.NOT_EXECUTED);
                }
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean shouldCreateMetadata() {
        return false;
    }
}
