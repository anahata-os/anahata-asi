/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import java.util.List;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.tool.AbstractToolCall;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolExecutionStatus;
import uno.anahata.ai.model.web.GroundingMetadata;

/**
 * Represents a message originating from the AI model. It extends {@link AbstractMessage},
 * sets its role to {@code MODEL}, and provides convenience methods for accessing tool calls.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public abstract class AbstractModelMessage<R extends Response, T extends AbstractToolMessage> extends AbstractMessage {
    
    /** The ID of the model that generated this message. */
    private final String modelId;
    
    /** A paired message containing the responses to any tool calls in this message. */
    @Getter(AccessLevel.NONE)
    private T toolMessage;
    
    /** The reason why the model stopped generating content for this candidate. */
    @Setter(AccessLevel.NONE)
    private String finishReason; 
    
    /** The message explaining why the model stopped generating content for this candidate. */
    private String finishMessage;
    
    /** The grounding metadata for the response. */
    @Setter(AccessLevel.NONE)
    private GroundingMetadata groundingMetadata;
    
    /** The safety ratings for the response, summarized as a string. */
    private String safetyRatings;
    
    /** The number of tokens for this candidate. */
    @Setter(AccessLevel.NONE)
    private int tokenCount;
    
    /** The raw JSON response from the model. */
    @Setter(AccessLevel.NONE)
    private String rawJson;
    
    /** The citation metadata for the response, summarized as a string. */
    private String citationMetadata;
    
    /** The response that returned this message. */
    private R response;
    
    /** Whether the model is currently streaming content for this message. */
    private boolean streaming = false;
    
    public AbstractModelMessage(@NonNull Chat chat, @NonNull String modelId) {
        super(chat);
        this.modelId = modelId;
    }
    
    @Override
    public Role getRole() {
        return Role.MODEL;
    }

    @Override
    public String getFrom() {
        return modelId;
    }

    public T getToolMessage() {
        if (toolMessage == null) {
            createToolMessage();
        }
        return toolMessage;
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
        getPropertyChangeSupport().firePropertyChange("rawJson", oldJson, rawJson);
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
        getPropertyChangeSupport().firePropertyChange("rawJson", oldJson, this.rawJson);
    }

    /**
     * Sets the token count and fires a property change event.
     * @param tokenCount The new token count.
     */
    public void setTokenCount(int tokenCount) {
        int oldTokenCount = this.tokenCount;
        this.tokenCount = tokenCount;
        getPropertyChangeSupport().firePropertyChange("tokenCount", oldTokenCount, tokenCount);
    }

    /**
     * Sets the finish reason and fires a property change event.
     * @param finishReason The new finish reason.
     */
    public void setFinishReason(String finishReason) {
        String oldReason = this.finishReason;
        this.finishReason = finishReason;
        getPropertyChangeSupport().firePropertyChange("finishReason", oldReason, finishReason);
    }

    /**
     * Sets the grounding metadata and fires a property change event.
     * @param groundingMetadata The new grounding metadata.
     */
    public void setGroundingMetadata(GroundingMetadata groundingMetadata) {
        GroundingMetadata oldMetadata = this.groundingMetadata;
        this.groundingMetadata = groundingMetadata;
        getPropertyChangeSupport().firePropertyChange("groundingMetadata", oldMetadata, groundingMetadata);
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
     * Filters and returns only the tool call parts from this message.
     * @return A list of {@link AbstractToolCall} parts, or an empty list if none exist.
     */
    public List<AbstractToolCall> getToolCalls() {
        return getParts().stream()
                .filter(AbstractToolCall.class::isInstance)
                .map(AbstractToolCall.class::cast)
                .collect(Collectors.toList());
    }
    
    /**
     * Iterates through all tool responses associated with this message and rolls
     * any that are in a PENDING state to NOT_EXECUTED.
     */
    public void rollPendingToolsToNotExecuted() {
        T tm = getToolMessage();
        if (tm != null) {
            List<AbstractToolResponse<?>> responses = tm.getToolResponses();
            for (AbstractToolResponse<?> response : responses) {
                if (response.getStatus() == ToolExecutionStatus.PENDING) {
                    response.setStatus(ToolExecutionStatus.NOT_EXECUTED);
                }
            }
        }
    }
    
    protected abstract T createToolMessage();
    
    
}
