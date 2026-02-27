/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.AbstractToolResponse;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.model.web.GroundingMetadata;
import uno.anahata.asi.status.AgiStatus;

/**
 * Represents a message originating from the AI model. It extends {@link AbstractMessage},
 * sets its role to {@code MODEL}, and provides convenience methods for accessing tool calls.
 * In the V2 simplified architecture, this class acts as the atomic unit of a "turn", 
 * containing both model content and any resulting tool calls/responses.
 *
 * @author anahata-gemini-pro-2.5
 * @param <R> The type of the model response.
 */
@Getter
@Setter
public abstract class AbstractModelMessage<R extends Response> extends AbstractMessage {
    
    /** The ID of the model that generated this message. */
    private String modelId;
    
    /** The reason why the model stopped generating content for this candidate. */
    @Setter(AccessLevel.NONE)
    private FinishReason finishReason; 
    
    /** The message explaining why the model stopped generating content for this candidate. */
    private String finishMessage;
    
    /** The grounding metadata for the response. */
    @Setter(AccessLevel.NONE)
    private GroundingMetadata groundingMetadata;
    
    /** The safety ratings for the response, summarized as a string. */
    private String safetyRatings;
    
    /** The number of billed tokens for this candidate, as reported by the API. */
    private int billedTokenCount;
    
    /** The raw JSON response from the model. */
    @Setter(AccessLevel.NONE)
    private String rawJson;
    
    /** The citation metadata for the response, summarized as a string. */
    private String citationMetadata;
    
    /** The response that returned this message. */
    private R response;
    
    /** Whether the model is currently streaming content for this message. */
    private boolean streaming = false;
    
    /**
     * Constructs a new AbstractModelMessage.
     * 
     * @param agi The parent agi session.
     * @param modelId The ID of the model.
     */
    public AbstractModelMessage(@NonNull Agi agi, @NonNull String modelId) {
        super(agi);
        this.modelId = modelId;
    }
    
    /** {@inheritDoc} */
    @Override
    public Role getRole() {
        return Role.MODEL;
    }

    /** {@inheritDoc} */
    @Override
    public String getFrom() {
        return modelId;
    }

    /** {@inheritDoc} */
    @Override
    public String getDevice() {
        return "Cloud";
    }

    /**
     * Sets the raw JSON representation of the model's response and fires a property change event.
     * This method replaces any existing content.
     * 
     * @param rawJson The raw JSON string.
     */
    public void setRawJson(String rawJson) {
        String oldJson = this.rawJson;
        this.rawJson = rawJson;
        propertyChangeSupport.firePropertyChange("rawJson", oldJson, rawJson);
    }

    /**
     * Appends a raw JSON chunk to the existing content. If multiple chunks are
     * appended, they are automatically wrapped in a JSON array to maintain
     * validity for pretty-printing.
     * 
     * @param chunk The JSON chunk to append.
     */
    public void appendRawJson(String chunk) {
        if (chunk == null || chunk.isEmpty()) {
            return;
        }
        String oldJson = this.rawJson;
        if (this.rawJson == null || this.rawJson.isEmpty()) {
            this.rawJson = chunk;
        } else {
            // If it's the second chunk, start an array.
            if (!this.rawJson.startsWith("[")) {
                this.rawJson = "[\n" + this.rawJson + ",\n" + chunk + "\n]";
            } else {
                // It's already an array, append before the last ']'.
                this.rawJson = this.rawJson.substring(0, this.rawJson.lastIndexOf("]")).trim();
                if (this.rawJson.endsWith(",")) {
                    this.rawJson += "\n" + chunk + "\n]";
                } else {
                    this.rawJson += ",\n" + chunk + "\n]";
                }
            }
        }
        propertyChangeSupport.firePropertyChange("rawJson", oldJson, this.rawJson);
    }

    /**
     * Sets the billed token count and fires a property change event.
     * @param billedTokenCount The new billed token count.
     */
    public void setBilledTokenCount(int billedTokenCount) {
        int oldBilledTokenCount = this.billedTokenCount;
        this.billedTokenCount = billedTokenCount;
        propertyChangeSupport.firePropertyChange("billedTokenCount", oldBilledTokenCount, billedTokenCount);
    }

    /**
     * Sets the finish reason and fires a property change event.
     * @param finishReason The new finish reason.
     */
    public void setFinishReason(FinishReason finishReason) {
        FinishReason oldReason = this.finishReason;
        this.finishReason = finishReason;
        propertyChangeSupport.firePropertyChange("finishReason", oldReason, finishReason);
    }

    /**
     * Sets the grounding metadata and fires a property change event.
     * @param groundingMetadata The new grounding metadata.
     */
    public void setGroundingMetadata(GroundingMetadata groundingMetadata) {
        GroundingMetadata oldMetadata = this.groundingMetadata;
        this.groundingMetadata = groundingMetadata;
        propertyChangeSupport.firePropertyChange("groundingMetadata", oldMetadata, groundingMetadata);
    }

    /**
     * {@inheritDoc}
     * A model message is not garbage collectable if it is currently streaming.
     */
    @Override
    public boolean isGarbageCollectable() {
        return super.isGarbageCollectable() && !streaming;
    }

    /**
     * {@inheritDoc}
     * A model message is never effectively pruned while it is streaming.
     */
    @Override
    public boolean isEffectivelyPruned() {
        return !streaming && super.isEffectivelyPruned();
    }
    
    /**
     * Checks if this model message is the current tool prompt message for the agi.
     * 
     * @return {@code true} if this message is the tool prompt message.
     */
    public boolean isToolPromptMessage() {
        return getAgi() != null && getAgi().getToolPromptMessage() == this;
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public void removePart(AbstractPart part) {
        super.removePart(part);
        if (isToolPromptMessage()) {
            getAgi().checkToolPromptCompletion();
        }
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public void remove() {
        super.remove();
        if (isToolPromptMessage()) {
            getAgi().clearToolPrompt();
        }
    }
    
    /**
     * Filters and returns only the tool call parts from this message.
     * @return A list of {@link AbstractToolCall} parts, or an empty list if none exist.
     */
    public List<AbstractToolCall<?, ?>> getToolCalls() {
        return getParts().stream()
                .filter(AbstractToolCall.class::isInstance)
                .map(p -> (AbstractToolCall<?, ?>) p)
                .collect(Collectors.toList());
    }
    
    /**
     * Returns all tool responses associated with the tool calls in this message.
     * 
     * @return A list of tool responses.
     */
    public List<AbstractToolResponse<?>> getToolResponses() {
        return getToolCalls().stream()
                .map(AbstractToolCall::getResponse)
                .collect(Collectors.toList());
    }

    /**
     * Determines if this entire batch of tool calls can be executed automatically
     * without user intervention.
     * @return {@code true} if all conditions for automatic execution are met.
     */
    public boolean isAutoRunnable() {
        if (isStreaming()) {
            return false;
        }
        
        if (!getAgi().getConfig().isLocalToolsEnabled()) {
            return false;
        }
        
        List<AbstractToolCall<?, ?>> calls = getToolCalls();
        if (calls.isEmpty()) {
            return false;
        }
        
        for (AbstractToolCall<?, ?> call : calls) {
            if (call.getTool().getPermission() != ToolPermission.APPROVE_ALWAYS || call.getResponse().getStatus() != ToolExecutionStatus.PENDING) {
                return false;
            }
        }
        
        return true;
    }

    /**
     * Checks if there are any tool calls with responses in a PENDING state.
     * @return {@code true} if at least one tool is pending.
     */
    public boolean hasPendingTools() {
        return getToolCalls().stream()
                .anyMatch(call -> call.getResponse().getStatus() == ToolExecutionStatus.PENDING);
    }
    
    /**
     * Executes all tool calls in this message that are currently in a PENDING state.
     */
    public void executeAllPending() {
        getToolCalls().stream()
            .map(AbstractToolCall::getResponse)
            .filter(response -> response.getStatus() == ToolExecutionStatus.PENDING)
            .forEach(AbstractToolResponse::execute);
    }

    /**
     * Rejects all tool calls in this message that are currently in a PENDING state.
     * 
     * @param reason The reason for rejection.
     */
    public void rejectAllPending(String reason) {
        getToolCalls().stream()
            .map(AbstractToolCall::getResponse)
            .filter(response -> response.getStatus() == ToolExecutionStatus.PENDING)
            .forEach(response -> response.reject(reason));
    }

    /**
     * Sets all tool calls in this message that are currently in a PENDING state to DECLINED.
     */
    public void declineAllPending() {
        getToolCalls().stream()
            .map(AbstractToolCall::getResponse)
            .filter(response -> response.getStatus() == ToolExecutionStatus.PENDING)
            .forEach(response -> response.setStatus(ToolExecutionStatus.DECLINED));
    }

    /**
     * Processes all tool responses associated with this message that are currently in a PENDING state.
     * Tools with APPROVE_ALWAYS permission are executed, while others are rolled to DECLINED.
     */
    public void processPendingTools() {
        if (hasPendingTools()) {
            getAgi().getStatusManager().fireStatusChanged(AgiStatus.AUTO_EXECUTING_TOOLS);
            executeAllPending();
        }
    }
    
    /**
     * {@inheritDoc}
     * Creates and adds a {@link ModelTextPart} without thought metadata.
     */
    @Override
    public final TextPart addTextPart(String text) {
        return addTextPart(text, null, false);
    }

    /**
     * {@inheritDoc}
     * Creates and adds a {@link ModelBlobPart} without thought metadata.
     */
    @Override
    public final BlobPart addBlobPart(String mimeType, byte[] data) {
        return addBlobPart(mimeType, data, null);
    }

    /**
     * {@inheritDoc}
     * Creates and adds a {@link ModelBlobPart} from a file path, without thought metadata.
     */
    @Override
    public final ModelBlobPart addBlobPart(Path path) throws Exception {
        byte[] data = Files.readAllBytes(path);
        String mimeType = TikaUtils.detectMimeType(path.toFile());
        return addBlobPart(mimeType, data, null);
    }

    /**
     * Creates and adds a new model text part with thought process metadata.
     * 
     * @param text The text content.
     * @param thoughtSignature The thought signature.
     * @param thought Whether this is a thought part.
     * @return The created model text part.
     */
    public final ModelTextPart addTextPart(String text, byte[] thoughtSignature, boolean thought) {
        return new ModelTextPart(this, text, thoughtSignature, thought);
    }

    /**
     * Creates and adds a new model blob part with thought process metadata.
     * 
     * @param mimeType The MIME type.
     * @param data The binary data.
     * @param thoughtSignature The thought signature.
     * @return The created model blob part.
     */
    public final ModelBlobPart addBlobPart(String mimeType, byte[] data, byte[] thoughtSignature) {
        return new ModelBlobPart(this, mimeType, data, thoughtSignature);
    }

    /** {@inheritDoc} */
    @Override
    protected void appendMetadata(StringBuilder sb) {
        // No additional metadata needed at the message level.
    }
}
