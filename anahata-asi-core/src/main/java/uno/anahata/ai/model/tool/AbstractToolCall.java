/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.AbstractToolMessage;
import uno.anahata.ai.model.core.ThoughtSignature;

/**
 * Represents a request to execute a specific tool. It holds a direct reference
 * to the tool's definition and its corresponding response.
 * 
 * @author anahata-gemini-pro-2.5
 * @param <T> The specific type of the Tool.
 * @param <R> The specific type of the Response.
 */
@Getter
@Setter // Added @Setter for thoughtSignature
public abstract class AbstractToolCall<T extends AbstractTool<?, ?>, R extends AbstractToolResponse<?>> extends AbstractPart implements ThoughtSignature {

    /**
     * A unique, immutable identifier for this specific invocation request.
     */
    @NonNull
    private final String id;

    /**
     * The tool definition that this call corresponds to.
     */
    @NonNull
    private final T tool;

    /**
     * The arguments for the method, provided as a map of parameter names to
     * values.
     */
    @NonNull
    private final Map<String, Object> args;
    
    /**
     * The single, final response object associated with this call.
     * This is ignored during schema generation to prevent a circular reference.
     */
    @NonNull
    @JsonIgnore
    private final R response;
    
    /** The signature of the thought process as a byte array. */
    private byte[] thoughtSignature;

    public AbstractToolCall(AbstractModelMessage message, @NonNull String id, @NonNull T tool, @NonNull Map<String, Object> args) {
        super(message);
        this.id = id;
        this.tool = tool;
        this.args = args;
        this.response = createResponse(message.getToolMessage());
        getChat().getContextManager().ensureToolMessageFolllowsModelMessage(getMessage());
    }

    /**
     * Gets the name of the tool to be invoked.
     * @return The tool's name.
     */
    public String getToolName() {
        return tool.getName();
    }
    
    public AbstractModelMessage getMessage() {
        return (AbstractModelMessage) super.getMessage();
    }

    /**
     * Creates the corresponding response object for this tool call.
     * This acts as a factory method and is called once by the constructor.
     * @return A new, un-executed tool response.
     */
    protected abstract R createResponse(AbstractToolMessage toolMessage);
    
    //<editor-fold defaultstate="collapsed" desc="V2 Context Management Delegation">
    @Override
    protected int getDefaultTurnsToKeep() {
        // A tool call's lifecycle is always identical to its response.
        return getResponse().getDefaultTurnsToKeep();
    }
    
    @Override
    public boolean isEffectivelyPruned() {
        return getResponse().isEffectivelyPruned();
    }
    
    @Override
    public int getTurnsLeft() {
        return getResponse().getTurnsLeft();
    }
    
    @Override
    public void setPruned(Boolean pruned) {
        super.setPruned(pruned);
        getResponse().setPruned(pruned);
    }
    
    @Override
    public void setTurnsToKeep(Integer turnsToKeep) {
        super.setTurnsToKeep(turnsToKeep);
        getResponse().setTurnsToKeep(turnsToKeep);
    }
    //</editor-fold>
    
    @Override
    public String asText() {
        return "[Tool Call: " + getToolName() + " with args: " + getArgs().toString() + "]";
    }
}
