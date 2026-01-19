/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.chat;

import java.io.InterruptedIOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AiExecutors;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.GenerationRequest;
import uno.anahata.asi.model.core.InputUserMessage;
import uno.anahata.asi.model.core.ModelBlobPart;
import uno.anahata.asi.model.core.ModelTextPart;
import uno.anahata.asi.model.core.RequestConfig;
import uno.anahata.asi.model.core.Response;
import uno.anahata.asi.model.core.StreamObserver;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.UserMessage;
import uno.anahata.asi.model.provider.AbstractAiProvider;
import uno.anahata.asi.model.provider.AbstractModel;
import uno.anahata.asi.model.provider.ApiCallInterruptedException;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.status.ApiErrorRecord;
import uno.anahata.asi.status.ChatStatus;
import uno.anahata.asi.status.StatusManager;
import uno.anahata.asi.tool.RetryableApiException;
import uno.anahata.asi.tool.ToolManager;

/**
 * The central, provider-agnostic orchestrator for a single chat session in the
 * V2 architecture. This class manages the conversation flow, orchestrates calls
 * to the AI provider, and delegates context management to a specialized
 * ContextManager.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@Getter
public class Chat extends BasicPropertyChangeSource {

    private final ChatConfig config;
    private final ToolManager toolManager;
    private final ContextManager contextManager;
    private final ResourceManager resourceManager;
    private transient ExecutorService executor;
    private final StatusManager statusManager;
    private final List<AbstractAiProvider> providers = new ArrayList<>();

    /**
     * The currently selected model for the chat session.
     */
    private AbstractModel selectedModel;

    /**
     * A thread-safe flag indicating if the main chat loop is currently active.
     */
    private transient volatile boolean running = false;

    /**
     * The thread currently executing the chat turn. Used for interruption.
     */
    private transient volatile Thread currentExecutionThread;

    /**
     * A message that has been submitted via {@link #sendMessage(InputUserMessage)}
     * while the chat was busy. It will be picked up and processed as soon as
     * the current conversation turn is complete.
     */
    @Getter
    private InputUserMessage stagedUserMessage;

    /**
     * A thread-safe flag indicating if the chat session has been shut down.
     */
    private final AtomicBoolean shutdown = new AtomicBoolean(false);

    /**
     * The last response received from the model. Used for state persistence and
     * status panel initialization on deserialization.
     */
    private Response<? extends AbstractModelMessage> lastResponse;

    /**
     * The list of candidate messages currently being generated or waiting for selection.
     * These are NOT yet part of the context history.
     */
    private final List<AbstractModelMessage> activeCandidates = new ArrayList<>();

    /**
     * The model message that initiated the tool calls currently awaiting user approval.
     * This is only non-null when the status is {@link ChatStatus#TOOL_PROMPT}.
     */
    private AbstractModelMessage toolPromptMessage;

    /**
     * A ReentrantLock to synchronize access to shared mutable state (e.g.,
     * `running`, `stagedUserMessage`).
     */
    private transient ReentrantLock runningLock = new ReentrantLock();

    /**
     * A high-level summary of the conversation's current state or topic.
     */
    private String summary;

    /**
     * Constructs a new Chat session with the provided configuration.
     * 
     * @param config The chat configuration.
     */
    @SneakyThrows
    public Chat(@NonNull ChatConfig config) {
        this.config = config;
        log.info("Constructing chat with config: " + config);
        this.executor = AiExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.contextManager = new ContextManager(this);
        this.toolManager = new ToolManager(this);
        this.resourceManager = new ResourceManager();
        this.statusManager = new StatusManager(this);

        // Crucially, set the back-reference *before* initializing managers
        this.config.setChat(this);

        contextManager.init();

        // Discover and instantiate providers
        log.info("Attempting to instantiate AI providers: " + config.getProviderClasses());
        for (Class<? extends AbstractAiProvider> providerClass : config.getProviderClasses()) {
            try {
                // Instantiate the provider, passing this ToolManager instance.
                AbstractAiProvider provider = providerClass.getDeclaredConstructor().newInstance();
                this.providers.add(provider);
                log.info("Successfully instantiated and registered provider: {}", provider.getProviderId());
            } catch (Exception e) {
                log.error("Failed to instantiate provider class: {}", providerClass.getName(), e);
            }
        }
        
        config.getAsiConfig().register(this);
    }

    /**
     * Re-binds this chat session to an AsiContainer after deserialization.
     * This method re-initializes transient fields and propagates the container
     * reference to the ChatConfig.
     * 
     * @param container The AsiContainer to bind to.
     */
    public void rebind(@NonNull AsiContainer container) {
        log.info("Rebinding chat session {} to container {}", config.getSessionId(), container.getHostApplicationId());
        this.config.rebind(container);
        this.executor = AiExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.runningLock = new ReentrantLock();
        this.running = false;
        this.currentExecutionThread = null;
    }

    /**
     * Saves this chat session to the application's session directory.
     */
    public void save() {
        config.getAsiConfig().saveSession(this);
    }

    /**
     * Sets the selected model and fires a property change event.
     * 
     * @param selectedModel The new model to select.
     */
    public void setSelectedModel(AbstractModel selectedModel) {
        AbstractModel oldModel = this.selectedModel;
        this.selectedModel = selectedModel;
        propertyChangeSupport.firePropertyChange("selectedModel", oldModel, selectedModel);
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
     * Sets the staged user message and fires a property change event.
     * 
     * @param stagedUserMessage The new staged message.
     */
    public void setStagedUserMessage(InputUserMessage stagedUserMessage) {
        InputUserMessage oldMessage = this.stagedUserMessage;
        this.stagedUserMessage = stagedUserMessage;
        propertyChangeSupport.firePropertyChange("stagedUserMessage", oldMessage, stagedUserMessage);
    }

    /**
     * The primary entry point for the UI to send a message. This method is
     * designed to be called from a background thread (e.g., a SwingWorker).
     * <ul>
     * <li>If the chat is idle, this method will start the processing loop and
     * block the calling thread until the entire conversation turn is
     * complete.</li>
     * <li>If the chat is already busy, it will stage the message for later
     * processing and return immediately. The ongoing loop will pick up the
     * staged message when its current turn is complete.</li>
     * </ul>
     *
     * @param message The user's message. Can be null or empty to trigger a context resend.
     */
    public void sendMessage(InputUserMessage message) {
        runningLock.lock();
        try {
            if (running) {
                if (message != null && !message.isEmpty()) {
                    log.info("Chat is busy. Staging message.");
                    setStagedUserMessage(message);
                } else {
                    log.info("Chat is busy. Ignoring empty message/resend request.");
                }
                return;
            }
            
            // If both are empty, we have nothing to do.
            if ((message == null || message.isEmpty()) && contextManager.buildVisibleHistory().isEmpty()) {
                log.warn("Attempted to send empty message with empty history. Ignoring.");
                return;
            }

            setRunning(true);
        } finally {
            runningLock.unlock();
        }

        try {
            processPendingTools();
            if (message != null && !message.isEmpty()) {
                contextManager.addMessage(message);
            }
            executeTurnLoop();
        } finally {
            setRunning(false);
            processStagedMessage();
        }
    }

    /**
     * Processes any staged message that arrived while the chat was busy.
     */
    private void processStagedMessage() {
        InputUserMessage staged;
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
     * Stops the current chat execution by interrupting the execution thread.
     */
    public void stop() {
        Thread thread = currentExecutionThread;
        if (thread != null && thread.isAlive()) {
            log.info("Stopping chat execution by interrupting thread: {}", thread.getName());
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

        RequestConfig requestConfig = config.getRequestConfig();
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
        } finally {
            this.currentExecutionThread = null;
        }
    }

    /**
     * Performs a single generation turn, including retries.
     * 
     * @return true if the conversation turn is complete, false if it should continue (e.g. tool auto-run).
     */
    private boolean performSingleTurn() {
        int maxRetries = config.getApiMaxRetries();
        long initialDelayMillis = config.getApiInitialDelayMillis();
        long maxDelayMillis = config.getApiMaxDelayMillis();

        for (int attempt = 0; attempt < maxRetries; attempt++) {
            try {
                // Just-in-time staged message consumption
                InputUserMessage staged;
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
                }

                GenerationRequest request = prepareRequest();
                statusManager.fireStatusChanged(ChatStatus.API_CALL_IN_PROGRESS);
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
                    statusManager.fireStatusChanged(ChatStatus.IDLE);
                    return true;
                }
                log.error("Exception in performSingleTurn", e);
                ApiErrorRecord.ApiErrorRecordBuilder<?, ?> errorRecordBuilder = ApiErrorRecord.builder()
                        .modelId(selectedModel.getModelId())
                        .timestamp(java.time.Instant.now())
                        .retryAttempt(attempt)
                        .exception(e);

                if (e instanceof RetryableApiException rae) {
                    errorRecordBuilder.apiKey(rae.getApiKey());
                    long delay = (long) (initialDelayMillis * Math.pow(2, attempt)) + (long) (Math.random() * 500);
                    long backoffAmount = Math.min(delay, maxDelayMillis);
                    errorRecordBuilder.backoffAmount(backoffAmount);

                    if (attempt < maxRetries - 1) {
                        log.warn("API Error on attempt {}: {}. Retrying...", attempt + 1, e.toString());
                        try {
                            statusManager.fireApiError(errorRecordBuilder.build(), ChatStatus.WAITING_WITH_BACKOFF, "Retrying in " + backoffAmount + "ms");
                            Thread.sleep(backoffAmount);
                        } catch (InterruptedException ie) {
                            Thread.currentThread().interrupt();
                            statusManager.fireStatusChanged(ChatStatus.IDLE);
                            return true;
                        }
                    } else {
                        log.error("Max retries reached. Aborting.", e);
                        statusManager.fireApiError(errorRecordBuilder.build(), ChatStatus.MAX_RETRIES_REACHED, null);
                        throw new RuntimeException("Failed after " + (attempt + 1) + " attempts.", e);
                    }
                } else {
                    statusManager.fireApiError(errorRecordBuilder.build(), ChatStatus.ERROR, null);
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
                result.addAll((List)candidates);
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
                if (t instanceof RuntimeException re) throw re;
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
     * @return true if the conversation turn is complete, false if it should continue.
     */
    private boolean handleTurnResult(List<? extends AbstractModelMessage> candidates) {
        statusManager.clearApiErrors();
        if (candidates.size() == 1) {
            // Finalize the single candidate (e.g., trigger tool execution).
            return chooseCandidate(candidates.get(0));
        } else if (candidates.size() > 1) {
            // Prompt the user to choose between multiple candidates.
            statusManager.fireStatusChanged(ChatStatus.CANDIDATE_CHOICE_PROMPT);
            return true;
        } else {
            // Fallback for empty candidate list
            statusManager.fireStatusChanged(ChatStatus.IDLE);
            return true;
        }
    }

    /**
     * Adds a chosen model message to the history, handles tool execution, and
     * continues the conversation if necessary.
     *
     * @param message The model message to add.
     * @return true if the conversation turn is complete, false if it should continue.
     */
    public boolean chooseCandidate(@NonNull AbstractModelMessage message) {
        // Clear active candidates and add the chosen one to the history.
        setActiveCandidates(Collections.emptyList());
        
        if (!contextManager.getHistory().contains(message)) {
            contextManager.addMessage(message);
        }

        // Ensure the tool message is initialized and populated with responses for all calls.
        AbstractToolMessage toolMessage = message.getToolMessage();

        if (toolMessage.isAutoRunnable()) {
            log.info("Auto-executing {} tool calls.", toolMessage.getToolResponses().size());
            statusManager.fireStatusChanged(ChatStatus.AUTO_EXECUTING_TOOLS);
            toolMessage.executeAllPending();
            
            if (config.isAutoReplyTools()) {
                log.info("Auto-replying after tool execution.");
                return false; // Continue loop
            }
        }

        // The turn has ended. Determine the final status based on whether there are pending tool calls.
        if (!message.getToolCalls().isEmpty()) {
            setToolPromptMessage(message);
            statusManager.fireStatusChanged(ChatStatus.TOOL_PROMPT);
        } else {
            setToolPromptMessage(null);
            statusManager.fireStatusChanged(ChatStatus.IDLE);
        }
        return true;
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
     * Sets the tool prompt message and fires a property change event.
     * 
     * @param toolPromptMessage The new tool prompt message.
     */
    private void setToolPromptMessage(AbstractModelMessage toolPromptMessage) {
        AbstractModelMessage oldMessage = this.toolPromptMessage;
        this.toolPromptMessage = toolPromptMessage;
        propertyChangeSupport.firePropertyChange("toolPromptMessage", oldMessage, toolPromptMessage);
    }

    /**
     * Processes all tool responses associated with the current tool prompt message.
     * Tools with APPROVE_ALWAYS permission are executed, while others are rolled to NOT_EXECUTED.
     */
    public void processPendingTools() {
        if (statusManager.getCurrentStatus() == ChatStatus.TOOL_PROMPT && toolPromptMessage != null) {
            toolPromptMessage.processPendingTools();
        }
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
     * Resets the entire chat session to a clean slate.
     */
    public void clear() {
        log.info("Clearing chat session {}", config.getSessionId());
        contextManager.clear();
        statusManager.reset();
        toolManager.reset();
        setActiveCandidates(Collections.emptyList());
        setToolPromptMessage(null);
        setStagedUserMessage(null);
        String newSessionId = UUID.randomUUID().toString();
        config.setSessionId(newSessionId);
        config.setName(null);
        log.info("Chat session cleared. New session ID: {}", newSessionId);
    }

    /**
     * Gets a flattened list of all models available from all registered
     * providers.
     *
     * @return A list of all available models.
     */
    public List<AbstractModel> getAllModels() {
        return providers.stream()
                .flatMap(provider -> provider.getModels().stream())
                .collect(Collectors.toList());
    }

    /**
     * Checks if the chat session has been shut down.
     *
     * @return True if the chat is shut down, false otherwise.
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
     * Gets a human-readable nickname for the session.
     * 
     * @return The session nickname or short ID.
     */
    public String getNickname() {
        return config.getName() != null ? config.getName() : getShortId();
    }

    /**
     * Sets the nickname for the session and fires a property change event.
     * 
     * @param nickname The new nickname.
     */
    public void setNickname(String nickname) {
        String old = config.getName();
        config.setName(nickname);
        propertyChangeSupport.firePropertyChange("nickname", old, nickname);
    }

    /**
     * Sets the summary for the session and fires a property change event.
     * 
     * @param summary The new summary.
     */
    public void setSummary(String summary) {
        String old = this.summary;
        this.summary = summary;
        propertyChangeSupport.firePropertyChange("summary", old, summary);
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
     * Shuts down the chat session, releasing resources and unregistering from global config.
     */
    public void shutdown() {
        shutdown.set(true);
        log.info("Shutting down Chat for session {}", config.getSessionId());
        config.getAsiConfig().unregister(this);
        if (executor != null && !executor.isShutdown()) {
            executor.shutdown();
        }
    }
    
    public void setProviderAndModel(String providerId, String modelId) {
        getProviders().stream()
            .filter(p -> p.getProviderId().equals(providerId))
            .findFirst()
            .flatMap(provider -> provider.findModel(modelId))
            .ifPresentOrElse(
                this::setSelectedModel,
                () -> log.error("Model not for: " + providerId + " " + modelId)
            );
    }
}
