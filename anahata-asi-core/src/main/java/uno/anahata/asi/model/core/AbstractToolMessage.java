/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.TextUtils;
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
 * @param <T> The type of the model message that initiated the tool calls.
 */
@Getter
@Setter
public abstract class AbstractToolMessage<T extends AbstractModelMessage> extends AbstractMessage {
    /**
     * The AbstractModelMessage that contains the tool calls this message is responding to.
     */
    private final T modelMessage;

    /**
     * Constructs a new AbstractToolMessage.
     * 
     * @param modelMessage The model message that initiated the tool calls.
     */
    public AbstractToolMessage(T modelMessage) {
        super(modelMessage.getChat());
        this.modelMessage = modelMessage;
        modelMessage.setToolMessage(this);
    }
    
    /** {@inheritDoc} */
    @Override
    public Role getRole() {
        return Role.TOOL;
    }

    /**
     * {@inheritDoc}
     * Propagates the pruned state back to the associated model message.
     */
    @Override
    public void setPruned(Boolean pruned) {
        super.setPruned(pruned);
        if (modelMessage != null) {
            modelMessage.setPruned(pruned);
        }
    }

    /**
     * {@inheritDoc}
     * A tool message is effectively pruned if it is explicitly pruned,
     * if all its responses are pruned, OR if the initiating model message
     * is effectively pruned. This ensures consistent context for the AI.
     */
    @Override
    public boolean isEffectivelyPruned() {
        if (super.isEffectivelyPruned()) {
            return true;
        }
        // Shadow the parent: If the question is gone, the answer is irrelevant.
        return modelMessage != null && modelMessage.isEffectivelyPruned();
    }

    /**
     * {@inheritDoc}
     * Hide the tool message ID to avoid model ambiguity.
     */
    @Override
    protected String getIdentityLabel() {
        return "";
    }

    /** {@inheritDoc} */
    @Override
    public String getFrom() {
        return TextUtils.getJvmId();
    }

    /**
     * {@inheritDoc}
     * Returns the hostname of the device where the tools were executed.
     */
    @Override
    public String getDevice() {
        return TextUtils.getDeviceId();
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
     * Checks if there are any tool responses in a PENDING state.
     * @return {@code true} if at least one tool is pending.
     */
    public boolean hasPendingTools() {
        return getToolResponses().stream()
                .anyMatch(r -> r.getStatus() == ToolExecutionStatus.PENDING);
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
     * Rejects all tool responses in this message that are currently in a PENDING state.
     * 
     * @param reason The reason for rejection.
     */
    public void rejectAllPending(String reason) {
        getToolResponses().stream()
            .filter(response -> response.getStatus() == ToolExecutionStatus.PENDING)
            .forEach(response -> response.reject(reason));
    }

    /**
     * Processes all tool responses in this message that are currently in a PENDING state.
     * This method is typically called when the user clicks 'Run'. It executes all 
     * pending tools regardless of their individual permission settings, as the 
     * user has provided explicit consent for this turn.
     */
    public void processPendingTools() {
        if (hasPendingTools()) {
            getChat().getStatusManager().fireStatusChanged(ChatStatus.AUTO_EXECUTING_TOOLS);
            executeAllPending();
        }
    }

    /**
     * {@inheritDoc}
     * @param text The text content.
     * @return The created text part.
     * @throws UnsupportedOperationException always, as tool messages cannot contain text parts.
     */
    @Override
    public final TextPart addTextPart(String text) {
        throw new UnsupportedOperationException("Cannot add text parts to a tool message.");
    }

    /**
     * {@inheritDoc}
     * @param mimeType The MIME type.
     * @param data The binary data.
     * @return The created blob part.
     * @throws UnsupportedOperationException always, as tool messages cannot contain blob parts.
     */
    @Override
    public final BlobPart addBlobPart(String mimeType, byte[] data) {
        throw new UnsupportedOperationException("Cannot add blob parts to a tool message.");
    }

    /**
     * {@inheritDoc}
     * @param path The file path.
     * @return The created blob part.
     * @throws UnsupportedOperationException always, as tool messages cannot contain blob parts.
     */
    @Override
    public final BlobPart addBlobPart(Path path) throws Exception {
        throw new UnsupportedOperationException("Cannot add blob parts to a tool message.");
    }

    /** {@inheritDoc} */
    @Override
    public boolean shouldCreateMetadata() {
        return false;
    }
}
