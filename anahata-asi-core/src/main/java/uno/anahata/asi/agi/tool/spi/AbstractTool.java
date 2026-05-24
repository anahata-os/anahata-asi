/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool.spi;

import uno.anahata.asi.agi.tool.ToolPermission;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.agi.message.AbstractModelMessage;

import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.provider.TokenizerType;

/**
 * The abstract base class for a tool, now generic on its Parameter and Call types.
 * 
 * @author anahata-gemini-pro-2.5
 * @param <P> The specific subclass of AbstractToolParameter this tool uses.
 * @param <C> The specific subclass of AbstractToolCall this tool creates.
 */
@Getter
@Slf4j
public abstract class AbstractTool<P extends AbstractToolParameter, C extends AbstractToolCall> extends BasicPropertyChangeSource {
    
    /** The fully qualified name of the tool, e.g., "LocalFiles.readFile". This is immutable. */
    @NonNull
    protected final String name;

    /** A detailed description of what the tool does. */
    protected String description;

    /** A reference to the parent toolkit that owns this tool. Can be null for standalone tools. */
    protected AbstractToolkit toolkit;

    /** The user's configured preference for this tool, determining its execution behavior. */
    protected ToolPermission permission;
    
    /**
     * Sets the user's execution permission preference for this tool and fires a property change event.
     *
     * @param permission The new permission level to apply.
     */
    public void setPermission(ToolPermission permission) {
        ToolPermission old = this.permission;
        if (Objects.equals(old, permission)) {
            return;
        }
        this.permission = permission;
        propertyChangeSupport.firePropertyChange("permission", old, permission);
    }

    /** The maximum depth this tool call should be retained in the context. */
    @Setter
    private int maxDepth = -1;//inherit

    /** A rich, ordered list of the tool's parameters. */
    private final List<P> parameters = new ArrayList<>();
    
    /** A pre-generated, language-agnostic JSON schema for the tool's return type. Can be null for void methods. */
    @Getter
    protected String responseJsonSchema;

    /**
     * Constructs a new AbstractTool with the given name.
     * 
     * @param name The tool's name.
     */
    protected AbstractTool(@NonNull String name) {
        this.name = name;
    }
    
    /**
     * The effective maximum depth. 
     * 
     * @return the effective max depth.
     */
    public int getEffectiveMaxDepth() {
        int ret = maxDepth;
        if (ret == -1) {
            ret = toolkit.getDefaultMaxDepth();
        }
        if (ret == -1) {
            ret = toolkit.getToolManager().getAgi().getConfig().getDefaultToolMaxDepth();
        }
        return ret;
    }
    
    /**
     * Factory method to create a tool-specific call object from raw model data.
     * @param message the model message the call will belong to.
     * @param id The call ID.
     * @param args The raw arguments from the model.
     * @return A new tool call instance.
     */
    public abstract C createCall(AbstractModelMessage message, String id, Map<String, Object> args);
    
    /**
     * Template method hook for subclasses to provide their specific Response type.
     * @return The reflection Type of the corresponding AbstractToolResponse subclass.
     */
    public abstract Type getResponseType();
    
    /**
     * Calculates the total token count of this tool on-the-fly using the specified tokenizer.
     * The count is a provider-agnostic approximation of the token overhead,
     * calculated by summing the tokens in its description, response schema,
     * and all of its parameters.
     * @param type The tokenizer strategy to use.
     * @return The total token count.
     */
    public int getTokenCount(TokenizerType type) {
        int totalTokens = 0;
        totalTokens += TokenizerUtils.countTokens(description, type);
        totalTokens += TokenizerUtils.countTokens(responseJsonSchema, type);

        for (AbstractToolParameter<?> param : parameters) {
            totalTokens += param.getTokenCount(type);
        }

        return totalTokens;
    }
}
