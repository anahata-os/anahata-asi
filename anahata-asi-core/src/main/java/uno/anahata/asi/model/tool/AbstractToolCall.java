/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.core.ThoughtSignature;

/**
 * Represents a request to execute a specific tool. It holds a direct reference
 * to the tool's definition and its corresponding response.
 * 
 * @author anahata-gemini-pro-2.5
 * @param <T> The specific type of the Tool.
 * @param <R> The specific type of the Response.
 */
@Getter
@Setter
public abstract class AbstractToolCall<T extends AbstractTool<?, ?>, R extends AbstractToolResponse<?>> extends AbstractPart implements ThoughtSignature {

    /**
     * A unique, immutable identifier for this specific invocation request.
     */
    @NonNull
    private final String id;

    /**
     * The tool definition that this call corresponds to.
     * Ignored during serialization to avoid issues with non-serializable members (e.g. Method).
     */
    @NonNull
    @JsonIgnore
    private final T tool;

    /**
     * The original, raw arguments received from the AI provider (e.g., Gemini).
     * These are stored as a plain JSON-style Map for debugging and "Final Gate" safety.
     */
    @NonNull
    private final Map<String, Object> rawArgs;

    /**
     * The enriched arguments for the tool, which may contain rich POJOs (like JavaType).
     * This map is used for tool execution and application logic.
     */
    @NonNull
    private final Map<String, Object> args;
    
    /** 
     * Map of arguments that were modified by the user in the UI. 
     * This represents the "draft" state of the arguments.
     */
    private final Map<String, Object> modifiedArgs = new HashMap<>();

    /**
     * The single, final response object associated with this call.
     * This is ignored during schema generation to prevent a circular reference.
     */
    @NonNull
    @JsonIgnore
    private final R response;
    
    /** The signature of the thought process as a byte array. */
    private byte[] thoughtSignature;

    /**
     * Constructs a new AbstractToolCall.
     * 
     * @param message The model message that initiated the call.
     * @param id The unique call ID.
     * @param tool The tool definition.
     * @param rawArgs The original JSON arguments from the provider.
     * @param args The enriched arguments (POJOs).
     */
    public AbstractToolCall(AbstractModelMessage message, @NonNull String id, @NonNull T tool, @NonNull Map<String, Object> rawArgs, @NonNull Map<String, Object> args) {
        super(message);
        this.id = id;
        this.tool = tool;
        this.rawArgs = rawArgs;
        this.args = args; 
        this.response = createResponse(message.getToolMessage());
        getChat().getContextManager().ensureToolMessageFollowsModelMessage(getMessage());
        
        // Leaf class publication: we add ourselves to the message only after 
        // the response field is fully initialized.
        message.addPart(this);
    }

    /**
     * Gets the name of the tool to be invoked.
     * @return The tool's name.
     */
    public String getToolName() {
        return tool.getName();
    }
    
    /**
     * Gets the model message that initiated this tool call.
     * 
     * @return The parent model message.
     */
    @Override
    public AbstractModelMessage getMessage() {
        return (AbstractModelMessage) super.getMessage();
    }

    /**
     * Creates the corresponding response object for this tool call.
     * This acts as a factory method and is called once by the constructor.
     * @param toolMessage The tool message that will contain the response.
     * @return A new, un-executed tool response.
     */
    protected abstract R createResponse(AbstractToolMessage toolMessage);
    
    /**
     * Updates a modified argument in the draft state and fires a property change event.
     * 
     * @param name The name of the argument.
     * @param value The new value.
     */
    public void setModifiedArgument(String name, Object value) {
        Object original = args.get(name);
        if (Objects.equals(original, value)) {
            modifiedArgs.remove(name);
        } else {
            modifiedArgs.put(name, value);
        }
        propertyChangeSupport.firePropertyChange("modifiedArgs", null, modifiedArgs);
    }

    /**
     * Gets the effective arguments for the UI (original + draft modifications).
     * 
     * @return A map of effective arguments.
     */
    public Map<String, Object> getEffectiveArgs() {
        Map<String, Object> effective = new HashMap<>(args);
        effective.putAll(modifiedArgs);
        return effective;
    }

    //<editor-fold defaultstate="collapsed" desc="V2 Context Management Delegation">
    /** {@inheritDoc} */
    @Override
    protected int getDefaultTurnsToKeep() {
        // A tool call's lifecycle is always identical to its response.
        return getResponse().getDefaultTurnsToKeep();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean isEffectivelyPruned() {
        return getResponse().isEffectivelyPruned();
    }
    
    /** {@inheritDoc} */
    @Override
    public int getTurnsLeft() {
        return getResponse().getTurnsLeft();
    }
    
    /** {@inheritDoc} */
    @Override
    public Boolean getPruned() {
        return getResponse().getPruned();
    }

    /** {@inheritDoc} */
    @Override
    public void setPruned(Boolean pruned) {
        getResponse().setPruned(pruned);
    }
    
    /** {@inheritDoc} */
    @Override
    public Integer getTurnsToKeep() {
        return getResponse().getTurnsToKeep();
    }

    /** {@inheritDoc} */
    @Override
    public void setTurnsToKeep(Integer turnsToKeep) {
        getResponse().setTurnsToKeep(turnsToKeep);
    }
    //</editor-fold>
    
    /** {@inheritDoc} */
    @Override
    public String asText() {
        return "[Tool Call: " + getToolName() + " with args: " + getArgs().toString() + "]";
    }

    /** {@inheritDoc} */
    @Override
    protected void appendMetadata(StringBuilder sb) {
        sb.append(" | Response: ").append(getResponse().createMetadataHeader());
    }
}
