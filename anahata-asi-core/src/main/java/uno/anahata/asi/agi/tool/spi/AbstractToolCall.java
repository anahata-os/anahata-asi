/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool.spi;

import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.ToolPermission;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.agi.ExpandToolsPreference;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.ThoughtSignature;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.tool.ToolResponseAttachment;

/**
 * Represents a request to execute a specific tool. It holds a direct reference
 * to the tool's definition and its corresponding response.
 * <p>
 * In the V2 architecture, the tool call is the primary message part. It owns
 * the response lifecycle entirely. Pruning the call effectively prunes the
 * response for context purposes.
 * </p>
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
    private final String id;

    /**
     * The tool definition that this call corresponds to. Ignored during
     * serialization to avoid issues with non-serializable members (e.g.
     * Method).
     */
    @NonNull
    @JsonIgnore
    private final T tool;

    /**
     * The original, raw arguments received from the AI provider (e.g., Gemini).
     * These are stored as a plain JSON-style Map for debugging and "Final Gate"
     * safety.
     */
    @NonNull
    private final Map<String, Object> rawArgs;

    /**
     * The enriched arguments for the tool, which may contain rich POJOs (like
     * JavaType). This map is used for tool execution and application logic.
     */
    @NonNull
    private final Map<String, Object> args;

    /**
     * Map of arguments that were modified by the user in the UI. This
     * represents the "draft" state of the arguments.
     */
    private final Map<String, Object> modifiedArgs = new HashMap<>();

    /**
     * The single, final response object associated with this call. This is
     * ignored during schema generation to prevent a circular reference.
     */
    @NonNull
    @JsonIgnore
    private final R response;

    /**
     * The signature of the thought process as a byte array.
     */
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
    public AbstractToolCall(AbstractModelMessage message, String id, @NonNull T tool, @NonNull Map<String, Object> rawArgs, @NonNull Map<String, Object> args) {
        super(message);
        this.id = id;
        this.tool = tool;
        this.rawArgs = rawArgs;
        this.args = args;

        // 1. Initialize the response object.
        this.response = createResponse();
        ExpandToolsPreference expandPref = getAgiConfig().getExpandTools();
        if (expandPref == ExpandToolsPreference.ALL) {
            setExpanded(true);
        } else if (expandPref == ExpandToolsPreference.PROMPT) {
            setExpanded(tool.getPermission() == ToolPermission.PROMPT);
        } else {
            setExpanded(false);
        }
        // 2. Publication: Add only the call to the message. 
        // The response is accessed via the call.
        message.addPart(this);
    }

    /**
     * Gets the name of the tool to be invoked.
     *
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
     * Creates the corresponding response object for this tool call. This acts
     * as a factory method and is called once by the constructor.
     *
     * @return A new, un-executed tool response.
     */
    protected abstract R createResponse();

    /**
     * Updates a modified argument in the draft state and fires a property
     * change event.
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
     * @return a map of effective arguments.
     */
    public Map<String, Object> getEffectiveArgs() {
        Map<String, Object> effective = new HashMap<>(args);
        effective.putAll(modifiedArgs);
        return effective;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Performs the lazy calculation of the total tool call and response tokens.
     * </p>
     */
    @Override
    protected void calculateTokenCount() {
        setTokenCount(calculateTotalTokens());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Resets the cached token counts of both the tool call and its nested
     * response, forcing a lazy recalculation under the new selected model.
     * </p>
     */
    @Override
    public void resetTokenCount() {
        super.resetTokenCount();
        if (response != null) {
            response.resetTokenCount();
        }
    }

    /**
     * Calculates the total tokens consumed by this tool call, including its
     * nested execution response, by delegating to the selected model's offline
     * tokenizer.
     *
     * @return The total token count.
     */
    private int calculateTotalTokens() {
        AbstractModel model = getAgi() != null ? getAgi().getSelectedModel() : null;
        if (model == null) {
            return 0;
        }
        int callTokens = model.countTokens(this);
        int responseTokens = response != null ? response.getTokenCount() : 0;
        return callTokens + responseTokens;
    }

    /**
     * {@inheritDoc}
     * <p>
     * An actively executing tool call is immune to pruning, guaranteeing that it
     * remains visible in the prompt and is never garbage collected while its
     * background execution is still underway.
     * </p>
     */
    @Override
    public boolean isEffectivelyPruned() {
        if (response.getStatus() == ToolExecutionStatus.EXECUTING) {
            return false;
        }
        return super.isEffectivelyPruned();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected int getDefaultMaxDepth() {
        int toolMaxDepth = tool.getMaxDepth();
        if (toolMaxDepth != -1) {
            return toolMaxDepth;
        }
        return getAgiConfig().getDefaultToolMaxDepth();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the complete text representation of the tool call, including its
     * Java-like arguments signature and the full execution response details.
     * </p>
     */
    @Override
    public String asText() {
        String callStr = getToolName() + "(" + getArgumentsString(true) + ")";
        if (response != null) {
            return new StringBuilder(callStr).append("\nResponse:\n").append(response.asText()).toString();
        }
        return callStr;
    }

    /**
     * Formats the tool call's arguments into a concise, Java-like
     * comma-separated string. Honors
     * {@link uno.anahata.asi.Displayable#getDisplayValue()} and bracketed
     * collections via {@link TextUtils#formatValue}.
     *
     * @param effective If true, uses effective args (including user
     * modifications); otherwise original args.
     * @return The formatted comma-separated arguments string (e.g. "true,
     * [FlightContact, Airport]").
     */
    public String getArgumentsString(boolean effective) {
        Map<String, Object> targetArgs = effective ? getEffectiveArgs() : getArgs();
        if (targetArgs.isEmpty()) {
            return "";
        }
        return tool.getParameters().stream()
                .map(p -> {
                    Object val = targetArgs.get(p.getName());
                    return TextUtils.formatValue(val);
                })
                .collect(Collectors.joining(", "));
    }

    /**
     * {@inheritDoc} Overridden to provide a rich, detailed execution summary in
     * the pruned metadata header.
     */
    @Override
    public String getPrunedHint() {
        StringBuilder sb = new StringBuilder();
        sb.append("Tool: ").append(getToolName());
        sb.append(" | Status: ").append(response.getStatus());

        sb.append(" | Args: ").append(TextUtils.formatValue(response.getExecutedArgs()));
        if (!response.getModifiedArgs().isEmpty()) {
            sb.append(" | Modified Args: ").append(TextUtils.formatValue(response.getModifiedArgs().keySet()));
        }

        if (response.getResult() != null) {
            sb.append(" | Result: ").append(TextUtils.formatValue(response.getResult()));
        }

        String feedback = response.getUserFeedback();
        if (feedback != null && !feedback.isBlank()) {
            sb.append(" | User Feedback: ").append(feedback);
        }

        if (response.getErrors() != null && !response.getErrors().isBlank()) {
            sb.append(" | Errors: ").append(TextUtils.formatValue(response.getErrors()));
        }

        return sb.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void appendMetadata(StringBuilder sb) {
        if (response != null) {
            String feedback = response.getUserFeedback();
            if (feedback != null && !feedback.isBlank()) {
                sb.append(" | User Feedback: ").append(feedback);
            }
            if (!response.getAttachments().isEmpty()) {
                sb.append(" | Attachments: ").append(
                        response.getAttachments().stream()
                                .map(ToolResponseAttachment::getDisplayValue)
                                .collect(Collectors.joining(", ", "[", "]"))
                );
            }
        }
    }

}
