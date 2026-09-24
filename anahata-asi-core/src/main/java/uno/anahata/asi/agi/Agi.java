/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.AsiExecutors;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.context.ContextManager;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.message.UserMessage;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ApiCallInterruptedException;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.resource.ResourceManager;
import uno.anahata.asi.agi.status.ApiErrorRecord;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.agi.status.StatusManager;
import uno.anahata.asi.agi.provider.RetryableApiException;
import uno.anahata.asi.agi.tool.ToolManager;

/**
 * The central, provider-agnostic artificial super intelligence (ASI)
 * orchestrator.
 * <p>
 * This class serves as the 'Anahata' (heart) of a conversation session,
 * managing the high-level lifecycle of an agentic turn. It orchestrates the
 * flow between user input, tool execution, context assembly via
 * {@link ContextManager}, and multimodal generation via registered
 * {@link AbstractAiProvider}s.
 * </p>
 * <p>
 * <b>Thread Safety:</b> This class is designed to be thread-safe, utilizing
 * reentrant locks for state transitions and atomic booleans for lifecycle
 * flags.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
public class Agi extends BasicPropertyChangeSource {

    /**
     * The configuration for this agi session.
     */
    private final AgiConfig config;

    /**
     * The user-defined nickname for this agi session.
     */
    private String nickname;

    /**
     * The manager for all AI tools available in this session.
     */
    private final ToolManager toolManager;

    /**
     * The manager for the conversation history and context assembly.
     */
    private final ContextManager contextManager;

    /**
     * The V2 URI-centric Resource Manager.
     */
    private final ResourceManager resourceManager;

    /**
     * The executor service for background tasks and API calls.
     */
    private transient ExecutorService executor;

    /**
     * The manager for the agi's operational status and error reporting.
     */
    private final StatusManager statusManager;

    /**
     * The currently selected model for the agi session.
     */
    private transient AbstractModel selectedModel;

    /**
     * The request configuration for this agi session.
     */
    private final RequestConfig requestConfig;

    /**
     * A thread-safe flag indicating if the main agi loop is currently active.
     */
    private transient volatile boolean running = false;

    /**
     * The thread currently executing the agi turn. Used for interruption.
     */
    private transient volatile Thread currentExecutionThread;

    /**
     * A message that has been submitted via {@link #sendMessage(UserMessage)}
     * while the agi was busy. It will be picked up and processed as soon as the
     * current conversation turn is complete.
     */
    private UserMessage stagedUserMessage;

    /**
     * A thread-safe flag indicating if the agi session has been shut down.
     */
    private transient AtomicBoolean shutdown = new AtomicBoolean(false);

    /**
     * Whether this session is currently 'Open' in the host UI (e.g. tab
     * visible). This field is persisted to restore UI state across application
     * restarts.
     */
    private boolean open = false;

    /**
     * The last response received from the model. Used for state persistence and
     * status panel initialization on deserialization.
     */
    private Response<? extends AbstractModelMessage> lastResponse;

    /**
     * The list of candidate messages currently being generated or waiting for
     * selection. These are NOT yet part of the context history.
     */
    private final List<AbstractModelMessage> activeCandidates = new ArrayList<>();


    /**
     * A ReentrantLock to synchronize access to shared mutable state (e.g.,
     * `running`, `stagedUserMessage`).
     */
    private transient ReentrantLock runningLock = new ReentrantLock();

    /**
     * The message currently active in the execution turn (either the incoming user message or the resulting model message). Acts as the single source of truth for turn ownership.
     */
    private volatile AbstractMessage activeTurnMessage;
    /**
     * A high-level summary of the conversation's current state or topic.
     */
    private String conversationSummary;

    /**
     * Constructs a new Agi session with the provided configuration.
     *
     * @param config The agi configuration.
     */
    @SneakyThrows
    public Agi(@NonNull AgiConfig config) {
        this.config = config;
        // Crucially, set the back-reference *before* initializing managers
        this.config.setAgi(this);

        log.info("Constructing agi with config: " + config);
        this.executor = AsiExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.contextManager = new ContextManager(this);
        this.resourceManager = new ResourceManager(this);
        this.statusManager = new StatusManager(this);
        this.toolManager = new ToolManager(this);
        this.requestConfig = new RequestConfig(this);

        // Final manager initialization cascade
        contextManager.init();
    }

    /**
     * Gets a list of AI providers available for this session.
     * <p>
     * Implementation details: This method performs a dynamic lookup against the
     * container's master registry for each provider class defined in the
     * AgiConfig.
     * </p>
     *
     * @return The list of shared provider instances.
     */
    public List<AbstractAiProvider> getProviders() {
        return config.getProviderUuids().stream()
                .map(config.getAsiContainer()::getProvider)
                .filter(Objects::nonNull)
                .filter((t) -> t.isEnabled())
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Re-initializes the transient lifecycle flags and ensures the session is
     * ready for active generation after deserialization.
     * </p>
     */
    @Override
    public void rebind() {
        super.rebind();
        log.info("Kryo rebind hook called for Agi session {}", config.getSessionId());
        this.shutdown = new AtomicBoolean(false);
    }

    /**
     * Re-binds this agi session to an AsiContainer after deserialization. This
     * method re-initializes transient fields and propagates the container
     * reference to the AgiConfig.
     *
     * @param container The AsiContainer to bind to.
     */
    public void bindToContainer(@NonNull AbstractAsiContainer container) {
        log.info("Rebinding agi session {} to container {}", config.getSessionId(), container.getHostApplicationId());
        // Re-initialize transient fields that require external context (like the container)
        this.config.setAsiContainer(container);
        this.executor = AsiExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.runningLock = new ReentrantLock();
        this.running = false;
        this.currentExecutionThread = null;

        // Late-Binding restoration of the selected model from the master registry
        String providerId = config.getSelectedProviderUuid();
        String modelId = config.getSelectedModelId();
        if (providerId != null) {            
            if (modelId != null) {
                AbstractAiProvider prov = container.getProvider(providerId);            
                Optional<? extends AbstractModel> model = prov.getModel(modelId);
                if (model.isPresent()) {
                    log.info("Restoring transient selected model: {}", modelId);
                    setSelectedModel(model.get());
                }
                
            }
        }

        log.info("Triggering environmental bootstrapping for agi session {}", config.getSessionId());

        // Warm up managers and toolkits before we start any processing loops
        this.postActivate();
        // Zombie Message Recovery: If we reloaded a session with a staged message, 
        // kickstart the processing loop to consume it.
        if (stagedUserMessage != null) {
            log.info("Recovered staged user message during rebind. Triggering send loop.");
            executor.submit(() -> sendMessage(null));
        }
    }

    /**
     * Performs an automatic backup of the session to the active sessions
     * directory.
     * 
     * @param reason the reason for the autosave
     */
    public void autoSave(String reason)  {
        try {
            config.getAsiContainer().autoSaveSession(this, reason);
        } catch (Exception e) {
            log.error("Could not auto save session " + this, e);
        }
        
    }

    /**
     * Checks if this session is a template managed by the container.
     *
     * @return true if this session is in the container's templates list.
     */
    public boolean isTemplate() {
        return config != null && config.getAsiContainer() != null && config.getAsiContainer().isTemplate(this);
    }

    /**
     * Manually saves the session to the 'saved' directory.
     * @throws java.io.IOException
     */
    public void save() throws IOException {
        config.getAsiContainer().manualSaveSession(this);
    }

    /**
     * Sets the visibility status of the agi in the host UI.
     *
     * @param open True if a tab/window is currently showing this session.
     */
    public void setOpen(boolean open) {
        boolean old = this.open;
        if (old != open) {
            this.open = open;
            propertyChangeSupport.firePropertyChange("open", old, open);
            // Authoritative Persistence: always auto-save on visibility changes 
            // to ensure UI layout is remembered across restarts.
            autoSave("open " + open);
        }
    }

    /**
     * Sets the selected model and fires a property change event.
     *
     * @param selectedModel The new model to select.
     */
    public void setSelectedModel(AbstractModel selectedModel) {
        AbstractModel oldModel = this.selectedModel;
        if (Objects.equals(oldModel, selectedModel)) {
            return;
        }
        this.selectedModel = selectedModel;

        boolean modelIdChanged;
        // Mirror state to the DNA (AgiConfig)
        if (selectedModel != null) {
            String newProviderUuid = selectedModel.getProvider().getUuid();
            String newModelId = selectedModel.getModelId();
            modelIdChanged = !Objects.equals(this.config.getSelectedProviderUuid(), newProviderUuid)
                    || !Objects.equals(this.config.getSelectedModelId(), newModelId);
            this.config.setSelectedProviderUuid(newProviderUuid);
            this.config.setSelectedModelId(newModelId);
        } else {
            modelIdChanged = this.config.getSelectedProviderUuid() != null || this.config.getSelectedModelId() != null;
            this.config.setSelectedProviderUuid(null);
            this.config.setSelectedModelId(null);
        }

        if (selectedModel != null) {
            // 1. Sync existing selected server tools with the new model's capabilities
            List<ServerTool> available = selectedModel.getAvailableServerTools();
            requestConfig.getEnabledServerTools().removeIf(st
                    -> available.stream().noneMatch(a -> a.getId().equals(st.getId()))
            );

            // 2. Add the new model's default server tools
            for (ServerTool def : selectedModel.getDefaultServerTools()) {
                if (requestConfig.getEnabledServerTools().stream().noneMatch(st -> st.getId().equals(def.getId()))) {
                    requestConfig.getEnabledServerTools().add(def);
                }
            }
        }

        // Reset cached token counts across all tools, history parts, and resources lazily on model change
        getToolManager().resetTokenCounts();
        getContextManager().resetTokenCounts();
        getResourceManager().resetTokenCounts();

        propertyChangeSupport.firePropertyChange("selectedModel", oldModel, selectedModel);
        if (modelIdChanged) {
            autoSave("model changed to: " + (selectedModel != null ? selectedModel.getModelId() : "none"));
        }
    }

    /**
     * Sets the running state and fires a property change event.
     *
     * @param running The new running state.
     */
    private void setRunning(boolean running) {
        boolean oldRunning = this.running;
        this.running = running;
        propertyChangeSupport.firePropertyChange("running", oldRunning, running);
    }

    /**
     * Sets or appends to the staged user message and fires a property change
     * event. Delegates directly to UserMessage.append to consolidate text and
     * attachments across turns, forcing event broadcast even on in-place
     * mutations.
     *
     * @param stagedUserMessage The message to stage or append, or null to
     * clear.
     */
    public void setStagedUserMessage(UserMessage stagedUserMessage) {
        UserMessage oldMessage = this.stagedUserMessage;
        if (stagedUserMessage == null) {
            this.stagedUserMessage = null;
            propertyChangeSupport.firePropertyChange("stagedUserMessage", oldMessage, null);
        } else if (this.stagedUserMessage != null) {
            this.stagedUserMessage.append(stagedUserMessage);
            // Force event broadcast because this.stagedUserMessage was mutated in-place
            propertyChangeSupport.firePropertyChange("stagedUserMessage", null, this.stagedUserMessage);
        } else {
            this.stagedUserMessage = stagedUserMessage;
            propertyChangeSupport.firePropertyChange("stagedUserMessage", null, this.stagedUserMessage);
        }
    }

    /**
     * The primary entry point for sending a user message. Non-null contract enforced. Claims activeTurnMessage ownership. If an API call is actively in progress, the message is staged; during WAITING_WITH_BACKOFF or tool execution, the new message supersedes previous activity.
     *
     * @param message The non-null user message to send.
     */
    public void sendMessage(@NonNull UserMessage message) {
        AbstractMessage myTurnMessage = message;

        runningLock.lock();
        try {
            AbstractMessage previous = this.activeTurnMessage;
            setActiveTurnMessage(myTurnMessage);

            if (previous instanceof AbstractModelMessage amm) {
                amm.stopRunningAllPending();
            }

            if (running) {
                if (statusManager.getCurrentStatus() == AgiStatus.WAITING_WITH_BACKOFF) {
                    log.info("User sent message during WAITING_WITH_BACKOFF. Adding directly to context.");
                    contextManager.addMessage(message);
                    autoSave("sendMessage added message " + message.getSequentialId() + " to context");
                    return;
                }

                if (statusManager.getCurrentStatus() == AgiStatus.API_CALL_IN_PROGRESS) {
                    log.info("API call in flight. Staging message.");
                    setStagedUserMessage(message);
                    return;
                }
            }

            setRunning(true);
        } finally {
            runningLock.unlock();
        }

        try {
            statusManager.fireStatusChanged(AgiStatus.AWAKENING_KUNDALINI);
            log.info("Adding user message to context  {}", message);
            contextManager.addMessage(message);
            autoSave("sendMessage added message " + message.getSequentialId() + " to context");
            executeTurnLoop();
        } finally {
            if (this.activeTurnMessage == myTurnMessage) {
                setRunning(false);
                processStagedMessage();
            } else {
                log.info("Turn for message #{} was superseded by #{}. Exiting without modifying running state.",
                        myTurnMessage.getSequentialId(),
                        activeTurnMessage != null ? activeTurnMessage.getSequentialId() : "null");
            }
        }
    }

    /**
     * Processes any staged message that arrived while the agi was busy.
     */
    private void processStagedMessage() {
        UserMessage staged;
        runningLock.lock();
        try {
            staged = stagedUserMessage;
            if (staged != null) {
                setStagedUserMessage(null);
            }
        } finally {
            runningLock.unlock();
        }

        if (staged != null) {
            log.info("Processing staged message.");
            sendMessage(staged);
        }
    }

    /**
     * Stops the current agi execution by interrupting the execution thread.
     */
    public void stop() {
        Thread thread = currentExecutionThread;
        if (thread != null && thread.isAlive()) {
            log.info("Stopping agi execution by interrupting thread: {}", thread.getName());
            thread.interrupt();
        }
    }

    /**
     * Prepares the request by building the history from the context manager.
     *
     * @return A GenerationRequest containing the config and history.
     * @throws IllegalStateException if no model is selected.
     */
    private GenerationRequest prepareRequest() {
        if (selectedModel == null) {
            throw new IllegalStateException("A model must be selected before sending a message.");
        }

        List<AbstractMessage> history = contextManager.buildVisibleHistory();
        return new GenerationRequest(requestConfig, history);
    }

    /**
     * Orchestrates the conversation turn loop, handling both synchronous and
     * streaming modes, retries, and candidate selection.
     */
    private void executeTurnLoop() {
        this.currentExecutionThread = Thread.currentThread();
        try {
            boolean turnComplete = false;
            while (!turnComplete) {
                turnComplete = performSingleTurn();
            }
            // Auto-save after turn loop completes
            autoSave("turn loop finished");
        } finally {
            this.currentExecutionThread = null;
        }
    }

    /**
     * Performs a single generation turn, including retries.
     *
     * @return true if the conversation turn is complete, false if it should
     * continue (e.g. tool auto-run).
     */
    private boolean performSingleTurn() {
        int maxRetries = config.getApiMaxRetries();
        long initialDelayMillis = config.getApiInitialDelayMillis();
        long maxDelayMillis = config.getApiMaxDelayMillis();

        for (int attempt = 0; attempt < maxRetries; attempt++) {
            try {
                // Just-in-time staged message consumption
                UserMessage staged;
                runningLock.lock();
                try {
                    staged = stagedUserMessage;
                    setStagedUserMessage(null);
                } finally {
                    runningLock.unlock();
                }
                if (staged != null && !staged.isEmpty()) {
                    log.info("Picking up staged message before API call.");
                    contextManager.addMessage(staged);
                    autoSave("performSingleTurn added stagedMessage to context");
                }

                statusManager.fireStatusChanged(AgiStatus.AWAKENING_KUNDALINI);
                GenerationRequest request = prepareRequest();
                statusManager.fireStatusChanged(AgiStatus.API_CALL_IN_PROGRESS);
                log.info("Sending request to model '{}' (attempt {}/{}) with {} messages.",
                        selectedModel.getModelId(), attempt + 1, maxRetries, request.history().size());

                List<? extends AbstractModelMessage> candidates;
                if (config.isStreaming()) {
                    candidates = performStreamingTurn(request);
                } else {
                    candidates = performSyncTurn(request);
                }

                return handleTurnResult(candidates);

            } catch (Exception e) {
                if (e instanceof ApiCallInterruptedException || e instanceof InterruptedException || e instanceof InterruptedIOException) {
                    log.info("API call interrupted.");
                    statusManager.fireStatusChanged(AgiStatus.IDLE);
                    return true;
                }
                log.error("Exception in performSingleTurn", e);
                ApiErrorRecord.ApiErrorRecordBuilder<?, ?> errorRecordBuilder = ApiErrorRecord.builder()
                        .modelId(selectedModel.getModelId())
                        .timestamp(Instant.now())
                        .retryAttempt(attempt)
                        .stackTrace(ExceptionUtils.getStackTrace(e));

                if (e instanceof RetryableApiException rae) {
                    errorRecordBuilder.apiKey(rae.getApiKey());
                    long delay = (long) (initialDelayMillis * Math.pow(2, attempt)) + (long) (Math.random() * 500);
                    long backoffAmount = Math.min(delay, maxDelayMillis);
                    errorRecordBuilder.backoffAmount(backoffAmount);

                    if (attempt < maxRetries - 1) {
                        log.warn("API Error on attempt {}: {}. Retrying...", attempt + 1, e.toString());
                        try {
                            statusManager.fireApiError(errorRecordBuilder.build(), AgiStatus.WAITING_WITH_BACKOFF, "Retrying in " + backoffAmount + "ms");
                            Thread.sleep(backoffAmount);
                        } catch (InterruptedException ie) {
                            Thread.currentThread().interrupt();
                            statusManager.fireStatusChanged(AgiStatus.IDLE);
                            return true;
                        }
                    } else {
                        log.error("Max retries reached. Aborting.", e);
                        statusManager.fireApiError(errorRecordBuilder.build(), AgiStatus.MAX_RETRIES_REACHED, null);
                        throw new RuntimeException("Failed after " + (attempt + 1) + " attempts.", e);
                    }
                } else {
                    statusManager.fireApiError(errorRecordBuilder.build(), AgiStatus.ERROR, null);
                    throw new RuntimeException("Non-retryable API error occurred.", e);
                }
            }
        }
        return true; // Should not reach here
    }

    /**
     * Performs a synchronous generation turn.
     *
     * @param request The generation request.
     * @return The list of candidate messages.
     */
    private List<? extends AbstractModelMessage> performSyncTurn(GenerationRequest request) {
        Response<?> response = selectedModel.generateContent(request);
        this.lastResponse = response;
        statusManager.clearApiErrors();
        return response.getCandidates();
    }

    /**
     * Performs an asynchronous streaming generation turn.
     *
     * @param request The generation request.
     * @return The list of candidate messages.
     */
    private List<? extends AbstractModelMessage> performStreamingTurn(GenerationRequest request) {
        final List<AbstractModelMessage> result = new ArrayList<>();
        selectedModel.generateContentStream(request, new StreamObserver<>() {
            @Override
            public void onStart(List<? extends AbstractModelMessage> candidates) {
                result.addAll((List) candidates);
                handleCandidatesStart(candidates);
            }

            @Override
            public void onNext(Response<? extends AbstractModelMessage> response) {
                lastResponse = response;
            }

            @Override
            public void onComplete() {
                log.info("Streaming complete. {} candidates received.", result.size());
                result.forEach(c -> c.setStreaming(false));
            }

            @Override
            public void onError(Throwable t) {
                log.error("Error in streaming response", t);
                result.forEach(c -> c.setStreaming(false));
                // Rethrow to be caught by the retry loop in performSingleTurn
                if (t instanceof RuntimeException re) {
                    throw re;
                }
                throw new RuntimeException(t);
            }
        });
        return result;
    }

    /**
     * Handles the initial set of candidates received from a stream.
     *
     * @param candidates The list of candidate messages.
     */
    private void handleCandidatesStart(List<? extends AbstractModelMessage> candidates) {
        if (candidates.size() == 1) {
            // OPTIMIZATION: If there's only one candidate, we add it to the history 
            // immediately. This allows the ConversationPanel to render it as it 
            // streams, providing a more natural experience.
            AbstractModelMessage candidate = candidates.get(0);
            contextManager.addMessage(candidate);

            // We keep activeCandidates empty so the selection panel stays hidden.
            setActiveCandidates(Collections.emptyList());
        } else {
            // If there are multiple candidates, we don't add them to history yet.
            // They are held in the activeCandidates list for the selection panel.
            setActiveCandidates(new ArrayList<>(candidates));
        }
    }

    /**
     * Handles the final result of a generation turn (sync or stream).
     *
     * @param candidates The list of candidate messages.
     * @return true if the conversation turn is complete, false if it should
     * continue.
     */
    private boolean handleTurnResult(List<? extends AbstractModelMessage> candidates) {
        statusManager.clearApiErrors();
        if (candidates.size() == 1) {
            // Finalize the single candidate (e.g., trigger tool execution).
            return chooseCandidate(candidates.get(0));
        } else if (candidates.size() > 1) {
            // Prompt the user to choose between multiple candidates.
            statusManager.fireStatusChanged(AgiStatus.CANDIDATE_CHOICE_PROMPT);
            return true;
        } else {
            // Fallback for empty candidate list
            statusManager.fireStatusChanged(AgiStatus.IDLE);
            return true;
        }
    }

    /**
     * Adds a chosen model message to the history, handles tool execution, and continues the conversation turn loop if auto-replying on full batch completion.
     * @param message The model message to add.
     * @return true if the conversation turn is complete, false if it should
     * continue.
     */
    public boolean chooseCandidate(@NonNull AbstractModelMessage message) {
        setActiveCandidates(Collections.emptyList());

        if (!contextManager.getHistory().contains(message)) {
            contextManager.addMessage(message);
        }
        autoSave("chooseCandidate " + message.getSequentialId());

        setActiveTurnMessage(message);

        if (message.isAutoRunnable()) {
            log.info("Auto-executing {} tool calls.", message.getToolCalls().size());
            statusManager.fireStatusChanged(AgiStatus.AUTO_EXECUTING_TOOLS);
            boolean allCompleted = message.executeAllPending();

            if (this.activeTurnMessage != message) {
                log.info("Tool execution for message #{} was superseded by #{}. Halting auto-reply loop.",
                        message.getSequentialId(), activeTurnMessage != null ? activeTurnMessage.getSequentialId() : "null");
                return true;
            }

            if (config.isAutoReplyTools() && allCompleted) {
                log.info("Auto-replying after tool execution.");
                return false;
            }
        }

        if (message.hasPendingTools()) {
            statusManager.fireStatusChanged(AgiStatus.TOOL_PROMPT);
        } else {
            statusManager.fireStatusChanged(AgiStatus.IDLE);
        }
        return true;
    }

    /**
     * Sets the active turn message and fires both activeTurnMessage and derived
     * toolPromptMessage property change events.
     *
     * @param message The new active turn message.
     */
    public void setActiveTurnMessage(AbstractMessage message) {
        AbstractMessage old = this.activeTurnMessage;
        this.activeTurnMessage = message;
        propertyChangeSupport.firePropertyChange("activeTurnMessage", old, message);
    }

    /**
     * Resolves the model message whose tool calls are currently awaiting review
     *      * or execution, derived directly from the active turn message single
     * source
  
     *
     f truth.
     *
     * @return The active model message if it has pending tools, or null.
     */
    public AbstractModelMessage getToolPromptMessage() {
        if (activeTurnMessage instanceof AbstractModelMessage amm && amm.hasPendingTools()) {
            return amm;
        }
        return null;
    }
    /**
     * Sets the active candidates and fires a property change event.
     *
     * @param candidates The new list of active candidates.
     */
    private void setActiveCandidates(List<AbstractModelMessage> candidates) {
        List<AbstractModelMessage> oldCandidates = new ArrayList<>(this.activeCandidates);
        this.activeCandidates.clear();
        this.activeCandidates.addAll(candidates);
        propertyChangeSupport.firePropertyChange("activeCandidates", oldCandidates, this.activeCandidates);
    }





    /**
     * Gets an unmodifiable view of the active candidates.
     *
     * @return The list of active candidates.
     */
    public List<AbstractModelMessage> getActiveCandidates() {
        return Collections.unmodifiableList(activeCandidates);
    }

    /**
     * Resets the entire agi session history to a clean slate.
     */
    public void clear() {
        log.info("Clearing agi session {}", config.getSessionId());
        contextManager.clear();
        statusManager.reset();
        toolManager.reset();
        conversationSummary = "Conversation history cleared.";
        setActiveCandidates(Collections.emptyList());
        this.activeTurnMessage = null;
        setStagedUserMessage(null);
        log.info("Agi history cleared. Managed resources still in context");
    }

    /**
     * Gets a flattened list of all models available from all registered
     * providers.
     *
     * @return A list of all available models.
     */
    public List<AbstractModel> getAllModels() {
        return getProviders().stream()
                .flatMap(provider -> provider.getModels().stream())
                .collect(Collectors.toList());
    }

    /**
     * Checks if the agi session has been shut down.
     *
     * @return True if the agi is shut down, false otherwise.
     */
    public boolean isShutdown() {
        return shutdown.get();
    }

    /**
     * Gets the last response received from the AI model.
     *
     * @return An Optional containing the last response, or empty if none
     * exists.
     */
    public Optional<Response<? extends AbstractModelMessage>> getLastResponse() {
        return Optional.ofNullable(lastResponse);
    }

    /**
     * Gets the total token count from the last response, if available.
     *
     * @return The total token count of the last response, or 0 if no response
     * is available.
     */
    public int getLastTotalTokenCount() {
        return getLastResponse()
                .map(Response::getTotalTokenCount)
                .orElse(0);
    }

    /**
     * Gets the current context window usage as a percentage (0.0 to 1.0).
     *
     * @return The context window usage percentage.
     */
    public double getContextWindowUsage() {
        int totalTokens = getLastTotalTokenCount();
        int totalThreshold = config.getTokenThreshold();
        if (totalThreshold <= 0) {
            return 0.0;
        }
        return (double) totalTokens / totalThreshold;
    }

    /**
     * Resolves the effective maximum output tokens configured for this session.
     * <p>
     * Evaluates the user's explicit request configuration override first
     * ({@link RequestConfig#maxOutputTokens}). If not explicitly specified by
     * the user, it falls back to the selected model's default maximum output tokens
     * ({@link AbstractModel#maxOutputTokens}). If no model is active or both
     * values are null, returns null.
     * </p>
     *
     * @return the effective maximum output tokens, or {@code null} if unconstrained.
     */
    public Integer getEffectiveUserMaxOutputTokens() {
        if (requestConfig != null && requestConfig.getMaxOutputTokens() != null) {
            return requestConfig.getMaxOutputTokens();
        }
        if (selectedModel != null && selectedModel.getMaxOutputTokens() != null) {
            return selectedModel.getMaxOutputTokens();
        }
        return null;
    }

    /**
     * Gets a human-readable display name for the session.
     *
     * @return The session display name (nickname or short ID).
     */
    public String getDisplayName() {
        return nickname != null && !nickname.isBlank() ? nickname : getShortId();
    }

    /**
     * Sets the nickname for the session and fires a property change event.
     *
     * @param nickname The new nickname.
     */
    public void setNickname(String nickname) {
        if (!Objects.equals(this.nickname, nickname)) {
            String old = this.nickname;
            log.info("Setting nickname for session {}: {} -> {}", config.getSessionId(), old, nickname);
            this.nickname = nickname;
            propertyChangeSupport.firePropertyChange("nickname", old, nickname);
            autoSave("nickname changed to " + nickname);
        }

    }

    /**
     * Sets the summary for the session and fires a property change event.
     *
     * @param summary The new summary.
     */
    public void setSummary(String summary) {
        if (!Objects.equals(this.conversationSummary, summary)) {
            String old = this.conversationSummary;
            log.info("Setting summary for session {}: {} -> {}", config.getSessionId(), old, summary);
            this.conversationSummary = summary;
            propertyChangeSupport.firePropertyChange("summary", old, summary);
        }
    }

    /**
     * Gets a short version of the session ID.
     *
     * @return The short session ID.
     */
    public String getShortId() {
        String id = config.getSessionId();
        return id.length() > 7 ? id.substring(id.length() - 7) : id;
    }

    /**
     * Shuts down the agi session, releasing resources and unregistering from
     * global config.
     */
    public void shutdown() {
        shutdown.set(true);
        log.info("Shuts down Agi for session {}", config.getSessionId());
        //this line seems unnecessary as the only caller of this method already unregisters the agi
        //config.getAsiContainer().unregisterAgi(this);
        if (executor != null && !executor.isShutdown()) {
            executor.shutdown();
        }
    }

    /**
     * Convenience method to retrieve a toolkit instance from the tool manager.
     *
     * @param <T> The type of the toolkit.
     * @param toolkitClass The class of the toolkit to find.
     * @return An Optional containing the toolkit instance.
     */
    public <T> Optional<T> getToolkit(Class<T> toolkitClass) {
        return toolManager.getToolkitInstance(toolkitClass);
    }

    /**
     * Performs post-restoration logic, warming up managers and toolkits. This
     * is called by the container after the session is bound and restored.
     */
    public void postActivate() {
        log.info("Post-activating Agi session: {}", config.getSessionId());
        toolManager.postActivate();
    }

    /**
     * Retrieves the parent session that spawned this Agi, if any.
     *
     * @return An Optional containing the parent session.
     */
    public Optional<Agi> getParent() {
        String parentUuid = config.getParentUuid();
        if (parentUuid == null) {
            return Optional.empty();
        }
        return config.getAsiContainer().getActiveAgis().stream()
                .filter(a -> parentUuid.equals(a.getConfig().getSessionId()))
                .findFirst();
    }

    /**
     * Retrieves all active sessions spawned by this Agi.
     *
     * @return A list of child sessions.
     */
    public List<Agi> getChildren() {
        return config.getAsiContainer().getChildrenAgis(config.getSessionId());
    }
}
