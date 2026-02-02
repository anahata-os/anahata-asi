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
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.model.web.GroundingMetadata;

/**
 * Represents a message originating from the AI model. It extends {@link AbstractMessage},
 * sets its role to {@code MODEL}, and provides convenience methods for accessing tool calls.
 * This class is generic over the {@link Response} type and the {@link AbstractToolMessage} type
 * to support provider-specific implementations.
 *
 * @author anahata-gemini-pro-2.5
 * @param <R> The type of the model response.
 * @param <T> The type of the tool message.
 */
@Getter
@Setter
public abstract class AbstractModelMessage<R extends Response, T extends AbstractToolMessage> extends AbstractMessage {
    
    /** The ID of the model that generated this message. */
    private String modelId;
    
    /** A paired message containing the responses to any tool calls in this message. */
    @Getter(AccessLevel.NONE)
    private T toolMessage;
    
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
     * @param chat The parent chat session.
     * @param modelId The ID of the model.
     */
    public AbstractModelMessage(@NonNull Chat chat, @NonNull String modelId) {
        super(chat);
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
     * Gets the tool message associated with this model message, creating it if necessary.
     * 
     * @return The tool message.
     */
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
     * Processes all tool responses associated with this message that are in a PENDING state.
     * Tools with APPROVE_ALWAYS permission are executed, while others are rolled to NOT_EXECUTED.
     */
    public void processPendingTools() {
        T tm = getToolMessage();
        if (tm != null) {
            tm.processPendingTools();
        }
    }
    
    /**
     * {@inheritDoc}
     * Creates and adds a {@link ModelTextPart} without thought metadata.
     */
    @Override
    public final ModelTextPart addTextPart(String text) {
        return addTextPart(text, null, false);
    }

    /**
     * {@inheritDoc}
     * Creates and adds a {@link ModelBlobPart} without thought metadata.
     */
    @Override
    public final ModelBlobPart addBlobPart(String mimeType, byte[] data) {
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

    /**
     * Factory method to create the appropriate tool message for this model message.
     * 
     * @return The created tool message.
     */
    protected abstract T createToolMessage();

    /** {@inheritDoc} */
    @Override
    protected void appendMetadata(StringBuilder sb) {
        if (billedTokenCount > 0) {
            sb.append(" | Billed Tokens: ").append(billedTokenCount);
        }

        T tm = getToolMessage();
        if (tm != null && tm.getSequentialId() != 0) {
            sb.append(" | Tool Message ID: ").append(tm.getSequentialId());
        }
    }
}
