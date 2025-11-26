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

/**
 * Represents a message originating from the AI model. It extends {@link AbstractMessage},
 * sets its role to {@code MODEL}, and provides convenience methods for accessing tool calls.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public abstract class AbstractModelMessage<T extends AbstractToolMessage> extends AbstractMessage {
    
    /** The ID of the model that generated this message. */
    private final String modelId;
    
    /** A paired message containing the responses to any tool calls in this message. */
    @Getter(AccessLevel.NONE)
    private T toolMessage;
    
    public AbstractModelMessage(@NonNull Chat chat, @NonNull String modelId) {
        super(chat);
        this.modelId = modelId;
    }
    
    @Override
    public Role getRole() {
        return Role.MODEL;
    }

    public T getToolMessage() {
        if (toolMessage == null) {
            createToolMessage();
        }
        return toolMessage;
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
    
    protected abstract T createToolMessage();
    
    
}