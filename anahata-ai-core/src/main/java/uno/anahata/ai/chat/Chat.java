/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.chat;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
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
import uno.anahata.ai.AiExecutors;
import uno.anahata.ai.context.ContextManager;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.AbstractToolMessage;
import uno.anahata.ai.model.core.GenerationRequest;
import uno.anahata.ai.model.core.InputUserMessage;
import uno.anahata.ai.model.core.ModelBlobPart;
import uno.anahata.ai.model.core.ModelTextPart;
import uno.anahata.ai.model.core.PropertyChangeSource;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.StreamObserver;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.resource.ResourceManager;
import uno.anahata.ai.status.ApiErrorRecord;
import uno.anahata.ai.status.ApiErrorRecord.ApiErrorRecordBuilder;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.status.StatusManager;
import uno.anahata.ai.tool.RetryableApiException;
import uno.anahata.ai.tool.ToolManager;

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
public class Chat implements PropertyChangeSource {

    private final ChatConfig config;
    private final ToolManager toolManager;
    private final ContextManager contextManager;
    private final ResourceManager resourceManager;
    private final ExecutorService executor;
    private final StatusManager statusManager;
    private final List<AbstractAiProvider> providers = new ArrayList<>();

    /** Support for firing property change events. */
    private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

    /**
     * The currently selected model for the chat session.
     */
    private AbstractModel selectedModel;

    /**
     * A thread-safe flag indicating if the main chat loop is currently active.
     */
    private volatile boolean running = false;

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
    private final ReentrantLock runningLock = new ReentrantLock();

    @SneakyThrows
    public Chat(@NonNull ChatConfig config) {
        this.config = config;
        this.executor = AiExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.contextManager = new ContextManager(this);
        this.toolManager = new ToolManager(this);
        this.resourceManager = new ResourceManager();
        this.statusManager = new StatusManager(this);

        // Crucially, set the back-reference *before* initializing managers
        this.config.setChat(this);

        contextManager.init();

        // Discover and instantiate providers
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
        
        ChatRegistry.register(this);
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
     * @param message The user's message.
     */
    public void sendMessage(@NonNull InputUserMessage message) {
        runningLock.lock();
        try {
            if (running) {
                log.info("Chat is busy. Staging message.");
                setStagedUserMessage(message);
                return;
            }
            rollPendingToolsToNotExecuted();
            contextManager.addMessage(message);
            executeTurn();
        } finally {
            runningLock.unlock();
        }
    }

    /**
     * Sends the current context to the model without adding a new user message.
     * This is useful for re-triggering the model after an API error or when the
     * input is empty.
     */
    public void sendContext() {
        runningLock.lock();
        try {
            if (running) {
                log.info("Chat is busy. Cannot send context.");
                return;
            }
            rollPendingToolsToNotExecuted();
            executeTurn();
        } finally {
            runningLock.unlock();
        }
    }

    /**
     * Prepares the request by consuming any staged messages and building the history.
     * 
     * @return A GenerationRequest containing the config and history.
     * @throws IllegalStateException if no model is selected.
     */
    private GenerationRequest prepareRequest() {
        if (selectedModel == null) {
            throw new IllegalStateException("A model must be selected before sending a message.");
        }

        // Atomically consume any staged message "just-in-time" before building the history.
        InputUserMessage messageToProcess = this.stagedUserMessage;
        if (messageToProcess != null) {
            setStagedUserMessage(null); // Consume it
            contextManager.addMessage(messageToProcess);
            log.info("Processing staged message.");
        }

        RequestConfig requestConfig = config.getRequestConfig();
        List<AbstractMessage> history = contextManager.buildVisibleHistory();
        return new GenerationRequest(requestConfig, history);
    }

    /**
     * Orchestrates a single conversation turn, handling both synchronous and
     * streaming modes, retries, and candidate selection.
     */
    private void executeTurn() {
        setRunning(true);
        try {
            boolean turnComplete = false;
            while (!turnComplete) {
                turnComplete = performSingleTurn();
            }
        } finally {
            setRunning(false);
            // Atomically check and process any staged message that arrived while we were busy.
            InputUserMessage staged = stagedUserMessage;
            if (staged != null) {
                setStagedUserMessage(null);
                sendMessage(staged);
            }
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
                log.error("Exception in performSingleTurn", e);
                ApiErrorRecordBuilder<?, ?> errorRecordBuilder = ApiErrorRecord.builder()
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
                            throw new RuntimeException("Chat interrupted during retry delay.", ie);
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
        selectedModel.generateContentStream(request, new StreamObserver<Response<? extends AbstractModelMessage>, AbstractModelMessage>() {
            @Override
            public void onStart(List<AbstractModelMessage> candidates) {
                result.addAll(candidates);
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
            setActiveCandidates((List)candidates);
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
     * Rolls any pending tool calls in the current tool prompt message to NOT_EXECUTED.
     */
    public void rollPendingToolsToNotExecuted() {
        if (statusManager.getCurrentStatus() == ChatStatus.TOOL_PROMPT && toolPromptMessage != null) {
            toolPromptMessage.rollPendingToolsToNotExecuted();
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
     * Gets a short version of the session ID.
     * 
     * @return The short session ID.
     */
    public String getShortId() {
        String id = config.getSessionId();
        return id.length() > 7 ? id.substring(0, 7) : id;
    }

    public void shutdown() {
        shutdown.set(true);
        log.info("Shutting down Chat for session {}", config.getSessionId());
        ChatRegistry.unregister(this);
        if (executor != null && !executor.isShutdown()) {
            executor.shutdown();
        }
    }

    /**
     * Adds a PropertyChangeListener to this chat session.
     * 
     * @param listener The listener to add.
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes a PropertyChangeListener from this chat session.
     * 
     * @param listener The listener to remove.
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }

    /** {@inheritDoc} */
    @Override
    public PropertyChangeSupport getPropertyChangeSupport() {
        return propertyChangeSupport;
    }

}
