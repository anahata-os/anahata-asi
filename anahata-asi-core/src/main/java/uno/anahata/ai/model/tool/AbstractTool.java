/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.internal.TokenizerUtils;
import uno.anahata.ai.model.core.AbstractModelMessage;

/**
 * The abstract base class for a tool, now generic on its Parameter and Call types.
 * 
 * @author anahata-gemini-pro-2.5
 * @param <P> The specific subclass of AbstractToolParameter this tool uses.
 * @param <C> The specific subclass of AbstractToolCall this tool creates.
 */
@Getter
@Slf4j
public abstract class AbstractTool<P extends AbstractToolParameter, C extends AbstractToolCall> {
    
    /** The default retention policy for tool calls, in number of user turns. */
    public static final int DEFAULT_RETENTION_TURNS = 5;
    
    /** The fully qualified name of the tool, e.g., "LocalFiles.readFile". This is immutable. */
    @NonNull
    protected final String name;

    /** A detailed description of what the tool does. */
    protected String description;

    /** A reference to the parent toolkit that owns this tool. Can be null for standalone tools. */
    protected AbstractToolkit toolkit;

    /** The user's configured preference for this tool, determining its execution behavior. */
    @Setter
    protected ToolPermission permission;

    /** The number of turns this tool call should be retained in the context. */
    @Setter
    private int retentionTurns;

    /** A rich, ordered list of the tool's parameters. */
    private final List<P> parameters = new ArrayList<>();
    
    /** A pre-generated, language-agnostic JSON schema for the tool's return type. Can be null for void methods. */
    @Getter
    protected String responseJsonSchema;

    protected AbstractTool(@NonNull String name) {
        this.name = name;
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
     * Calculates the total token count of this tool on-the-fly.
     * The count is a provider-agnostic approximation of the token overhead,
     * calculated by summing the tokens in its description, response schema,
     * and all of its parameters.
     *
     * @return The total token count.
     */
    public int getTokenCount() {
        int totalTokens = 0;
        totalTokens += TokenizerUtils.countTokens(description);
        totalTokens += TokenizerUtils.countTokens(responseJsonSchema);

        for (AbstractToolParameter<?> param : parameters) {
            totalTokens += param.getTokenCount();
        }

        return totalTokens;
    }
}
