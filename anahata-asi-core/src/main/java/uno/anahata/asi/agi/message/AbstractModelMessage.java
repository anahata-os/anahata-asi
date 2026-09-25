/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message;

import uno.anahata.asi.agi.message.web.GroundingMetadata;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * Represents a message originating from the AI model. It extends
 * {@link AbstractMessage}, sets its role to {@code MODEL}, and provides
 * convenience methods for accessing tool calls. In the V2 simplified
 * architecture, this class acts as the atomic unit of a "turn", containing both
 * model content and any resulting tool calls/responses.
 *
 * @author anahata-gemini-pro-2.5
 * @param <R> The type of the model response.
 */
@Getter
@Setter
public abstract class AbstractModelMessage<R extends Response> extends AbstractMessage {

    /**
     * If an AI provider produced this part and assigned an Id to it (e.g. an
     * openai message id).
     *
     */
    private String providerId;

    /**
     * The UUID of the AbstractAiProvider that generated this message.
     */
    private String providerUuid;

    /**
     * The ID of the AbstractModel that generated this message.
     */
    private String modelId;

    /**
     * The reason why the model stopped generating content for this candidate.
     */
    @Setter(AccessLevel.NONE)
    private FinishReason finishReason;

    /**
     * The message explaining why the model stopped generating content for this
     * candidate.
     */
    private String finishMessage;

    /**
     * The grounding metadata for the response.
     */
    @Setter(AccessLevel.NONE)
    private GroundingMetadata groundingMetadata;

    /**
     * The safety ratings for the response, summarized as a string.
     */
    private String safetyRatings;

    /**
     * The number of billed prompt (input) tokens for this candidate, as
     * reported by the API.
     */
    private int billedPromptTokens;
    /**
     * The number of billed completion (output) tokens for this candidate, as
     * reported by the API.
     */
    private int billedCompletionTokens;
    /**
     * The raw JSON response from the model.
     */
    @Setter(AccessLevel.NONE)
    private String rawJson;

    /**
     * The citation metadata for the response, summarized as a string.
     */
    private String citationMetadata;

    /**
     * The response that returned this message.
     */
    private R response;

    /**
     * Whether the model is currently streaming content for this message.
     */
    private boolean streaming = false;

    /**
     * Whether a batch execution of all pending tools is actively underway on
     * this message.
     */
    private volatile boolean runningAllPending = false;
    /**
     * A turn scoped map for tools to store turn-scoped attributes.
     */
    @Setter(AccessLevel.NONE)
    private final Map turnAttributes = new HashMap();

    /**
     * Constructs a new AbstractModelMessage.
     *
     * @param agi The parent agi session.
     * @param modelId The ID of the model.
     */
    public AbstractModelMessage(@NonNull Agi agi, @NonNull String modelId) {
        super(agi);
        this.modelId = modelId;
        if (agi.getSelectedModel() != null) {
            this.providerUuid = agi.getSelectedModel().getProvider().getUuid();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Role getRole() {
        return Role.MODEL;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getFrom() {
        return modelId;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDevice() {
        return "Cloud";
    }

    /**
     * Sets the raw JSON representation of the model's response and fires a
     * property change event. This method replaces any existing content.
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
     * Sets the billed prompt token count and fires a property change event.
     *
     * @param billedPromptTokens The new billed prompt token count.
     */
    public void setBilledPromptTokens(int billedPromptTokens) {
        int oldBilledPromptTokens = this.billedPromptTokens;
        this.billedPromptTokens = billedPromptTokens;
        propertyChangeSupport.firePropertyChange("billedPromptTokens", oldBilledPromptTokens, billedPromptTokens);
    }

    /**
     * Sets the billed completion token count and fires a property change event.
     *
     * @param billedCompletionTokens The new billed completion token count.
     */
    public void setBilledCompletionTokens(int billedCompletionTokens) {
        int oldBilledCompletionTokens = this.billedCompletionTokens;
        this.billedCompletionTokens = billedCompletionTokens;
        propertyChangeSupport.firePropertyChange("billedCompletionTokens", oldBilledCompletionTokens, billedCompletionTokens);
    }

    /**
     * Sets the finish reason and fires a property change event.
     *
     * @param finishReason The new finish reason.
     */
    public void setFinishReason(FinishReason finishReason) {
        FinishReason oldReason = this.finishReason;
        this.finishReason = finishReason;
        propertyChangeSupport.firePropertyChange("finishReason", oldReason, finishReason);
    }

    /**
     * Sets the grounding metadata and fires a property change event.
     *
     * @param groundingMetadata The new grounding metadata.
     */
    public void setGroundingMetadata(GroundingMetadata groundingMetadata) {
        GroundingMetadata oldMetadata = this.groundingMetadata;
        this.groundingMetadata = groundingMetadata;
        propertyChangeSupport.firePropertyChange("groundingMetadata", oldMetadata, groundingMetadata);
    }

    /**
     * {@inheritDoc} A model message is never effectively pruned while it is
     * streaming.
     */
    @Override
    public boolean isEffectivelyPruned() {
        return !streaming && super.isEffectivelyPruned();
    }

    /**
     * Checks if this model message is the current tool prompt message for the
     * agi.
     *
     * @return {@code true} if this message is the tool prompt message.
     */
    public boolean isToolPromptMessage() {
        return getAgi() != null && getAgi().getToolPromptMessage() == this;
    }

    /**
     * {@inheritDoc}
     * <p>Checks tool prompt completion after a part removal.</p>
     */
    @Override
    public void removePart(AbstractPart part) {
        super.removePart(part);
        if (getAgi() != null) {
            getAgi().checkToolPromptCompletion();
        }
    }

    /**
     * {@inheritDoc}
     * <p>Checks tool prompt completion after message removal.</p>
     */
    @Override
    public void remove() {
        super.remove();
        if (getAgi() != null) {
            getAgi().checkToolPromptCompletion();
        }
    }

    /**
     * {@inheritDoc}
     * <p>Reacts to pruning state changes on tool calls, broadcasting remainingTools
     * updates and re-evaluating tool prompt completion.</p>
     */
    @Override
    protected void onPartPruningStateChanged(AbstractPart part, PruningState oldState, PruningState newState) {
        super.onPartPruningStateChanged(part, oldState, newState);
        if (part instanceof AbstractToolCall) {
            propertyChangeSupport.firePropertyChange("remainingTools", null, getRemainingToolCallsCount());
            if (getAgi() != null) {
                getAgi().checkToolPromptCompletion();
            }
        }
    }

    /**
     * Filters and returns only the tool call parts from this message.
     *
     * @return A list of {@link AbstractToolCall} parts, or an empty list if
     * none exist.
     */
    public List<AbstractToolCall<?, ?>> getToolCalls() {
        return getParts().stream()
                .filter(AbstractToolCall.class::isInstance)
                .map(p -> (AbstractToolCall<?, ?>) p)
                .collect(Collectors.toList());
    }

    /**
     * Filters and returns all tool calls belonging to this model message that
     * are currently awaiting execution in {@link ToolExecutionStatus#PENDING}
     * state.
     * <p>
     * This method acts as the authoritative domain selector for batch tool
     * dispatching, UI pending counter badges, and human-in-the-loop review
     * actions.
     * </p>
     *
     * @return a list of pending {@link AbstractToolCall} instances in this
     * message.
     */
    public List<AbstractToolCall<?, ?>> getPendingToolCalls() {
        return getToolCalls().stream()
                .filter(call -> !call.isEffectivelyPruned())
                .filter(call -> call.getResponse().getStatus() == ToolExecutionStatus.PENDING)
                .collect(Collectors.toList());
    }

    /**
     * Calculates the number of tool calls in this message that are currently
     * active in the execution queue (either actively EXECUTING or still
     * PENDING).
     *
     * @return The count of remaining tools in the execution pipeline.
     */
    public int getRemainingToolCallsCount() {
        return (int) getToolCalls().stream()
                .filter(c -> !c.isEffectivelyPruned())
                .filter(c -> c.getResponse().getStatus() == ToolExecutionStatus.PENDING
                        || c.getResponse().getStatus() == ToolExecutionStatus.EXECUTING)
                .count();
    }
    /**
     * Returns all tool responses associated with the tool calls in this
     * message.
     *
     * @return A list of tool responses.
     */
    public List<AbstractToolResponse<?>> getToolResponses() {
        return getToolCalls().stream()
                .map(AbstractToolCall::getResponse)
                .collect(Collectors.toList());
    }

    /**
     * Determines if this entire batch of tool calls can be executed
     * automatically without user intervention.
     *
     * @return {@code true} if all conditions for automatic execution are met.
     */
    public boolean isAutoRunnable() {
        if (isStreaming()) {
            return false;
        }

        if (!getAgi().getConfig().isLocalToolsEnabled()) {
            return false;
        }

        List<AbstractToolCall<?, ?>> calls = getPendingToolCalls();
        if (calls.isEmpty()) {
            return false;
        }

        for (AbstractToolCall<?, ?> call : calls) {
            if (call.getTool().getPermission() != ToolPermission.APPROVE_ALWAYS) {
                return false;
            }
        }

        return true;
    }

    /**
     * Checks if there are any tool calls with responses in a PENDING state.
     *
     * @return {@code true} if at least one tool is pending.
     */
    public boolean hasPendingTools() {
        return getToolCalls().stream()
                .filter(call -> !call.isEffectivelyPruned())
                .anyMatch(call -> call.getResponse().getStatus() == ToolExecutionStatus.PENDING);
    }

    /**
     * Executes all tool calls in this message that are currently in a PENDING
     * state, sequentially checking runningAllPending before starting each tool
     * call. Broadcasts remainingTools count updates for live countdown UI
     * display and automatically collapses successfully executed tools.
     *
     * @return true if all pending tool calls were executed, false if execution
     * was stopped early and tools remain pending.
     */
    public boolean executeAllPending() {
        this.runningAllPending = true;
        propertyChangeSupport.firePropertyChange("runningAllPending", false, true);
        propertyChangeSupport.firePropertyChange("remainingTools", null, getRemainingToolCallsCount());
        try {
            for (AbstractToolCall<?, ?> call : getPendingToolCalls()) {
                if (!runningAllPending) {
                    break;
                }
                propertyChangeSupport.firePropertyChange("remainingTools", null, getRemainingToolCallsCount());
                call.getResponse().execute();
                if (call.getResponse().getStatus() == ToolExecutionStatus.EXECUTED) {
                    call.setExpanded(false);
                }
                propertyChangeSupport.firePropertyChange("remainingTools", null, getRemainingToolCallsCount());
            }
            return !hasPendingTools();
        } finally {
            this.runningAllPending = false;
            propertyChangeSupport.firePropertyChange("runningAllPending", true, false);
            propertyChangeSupport.firePropertyChange("remainingTools", null, getRemainingToolCallsCount());
        }
    }

    /**
     * Signals the sequential tool execution on this message to halt after the
     * currently executing tool finishes, leaving any subsequent unstarted tools
     * in PENDING state.
     */
    public void stopRunningAllPending() {
        if (this.runningAllPending) {
            this.runningAllPending = false;
            propertyChangeSupport.firePropertyChange("runningAllPending", true, false);
        }
    }

    /**
     * Sets all tool calls in this message that are currently in a PENDING state
     * to DECLINED and collapses them.
     */
    public void declineAllPending() {
        for (AbstractToolCall<?, ?> call : getPendingToolCalls()) {
            call.getResponse().setStatus(ToolExecutionStatus.DECLINED);
            call.setExpanded(false);
        }
    }

    /**
     * {@inheritDoc} Creates and adds a {@link ModelTextPart} without thought
     * metadata.
     */
    @Override
    public final TextPart addTextPart(String text) {
        return addTextPart(text, null, false);
    }

    /**
     * {@inheritDoc} Creates and adds a {@link ModelBlobPart} without thought
     * metadata.
     */
    @Override
    public final BlobPart addBlobPart(String mimeType, byte[] data) {
        return addBlobPart(mimeType, data, null);
    }

    /**
     * {@inheritDoc} Creates and adds a {@link ModelBlobPart} from a file path,
     * without thought metadata.
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
     * {@inheritDoc}
     */
    @Override
    protected void appendMetadata(StringBuilder sb) {
        // No additional metadata needed at the message level.
    }
}
