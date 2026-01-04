/*
 * Licensed under the Anahata Software License (AS IS) v 108. See the LICENSE file for details. Força Barça!
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
     * A message that has been submitted via {@link #sendMessage(UserMessage)}
     * while the chat was busy. It will be picked up and processed as soon as
     * the current conversation turn is complete.
     */
    private volatile UserMessage stagedUserMessage;

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
    public void sendMessage(@NonNull UserMessage message) {
        runningLock.lock();
        try {
            if (running) {
                log.info("Chat is busy. Staging message.");
                this.stagedUserMessage = message;
                return;
            }
            contextManager.addMessage(message);
            
            if (config.isStreaming()) {
                sendContextStreaming();
            } else {
                sendContext();
            }
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

            setRunning(true);
            sendToModel();
        } finally {
            setRunning(false);
            runningLock.unlock();
        }
    }

    /**
     * Prepares the request by consuming any staged messages and building the history.
     * 
     * @return A RequestContext containing the config and history.
     * @throws IllegalStateException if no model is selected.
     */
    private RequestContext prepareRequest() {
        if (selectedModel == null) {
            throw new IllegalStateException("A model must be selected before sending a message.");
        }

        // Atomically consume any staged message "just-in-time" before building the history.
        UserMessage messageToProcess = this.stagedUserMessage;
        if (messageToProcess != null) {
            this.stagedUserMessage = null; // Consume it
            contextManager.addMessage(messageToProcess);
            log.info("Processing staged message.");
        }

        RequestConfig requestConfig = config.getRequestConfig();
        List<AbstractMessage> history = contextManager.buildVisibleHistory();
        return new RequestContext(requestConfig, history);
    }

    /**
     * The core method for interacting with the model. It builds the history,
     * sends it, and processes the response, including handling retries and
     * multiple candidates.
     */
    private void sendToModel() {
        int maxRetries = config.getApiMaxRetries();
        long initialDelayMillis = config.getApiInitialDelayMillis();
        long maxDelayMillis = config.getApiMaxDelayMillis();
        
        for (int attempt = 0; attempt < maxRetries; attempt++) {
            try {
                RequestContext rc = prepareRequest();

                statusManager.fireStatusChanged(ChatStatus.API_CALL_IN_PROGRESS);
                log.info("Sending request to model '{}' (attempt {}/{}) with {} messages.",
                        selectedModel.getModelId(), attempt + 1, maxRetries, rc.history().size());

                Response<?> response = selectedModel.generateContent(this, rc.config(), rc.history());
                this.lastResponse = response;
                
                statusManager.clearApiErrors();

                if (response.getCandidates().size() == 1) {
                    chooseCandidate((AbstractModelMessage) response.getCandidates().get(0));
                } else {
                    log.info("Model returned multiple candidates. Pausing for user selection.");
                    setActiveCandidates(response.getCandidates().stream()
                            .map(c -> (AbstractModelMessage) c)
                            .collect(Collectors.toList()));
                    statusManager.fireStatusChanged(ChatStatus.CANDIDATE_CHOICE_PROMPT);
                }
                return;

            } catch (Exception e) {
                log.error("Exception in sendToModel", e);
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
    }

    /**
     * Sends a message and processes the response using token streaming.
     * 
     * @param message The user's message.
     */
    public void sendMessageStreaming(@NonNull UserMessage message) {
        runningLock.lock();
        try {
            if (running) {
                log.info("Chat is busy. Staging message for streaming.");
                this.stagedUserMessage = message;
                return;
            }
            contextManager.addMessage(message);
            sendContextStreaming();
        } finally {
            runningLock.unlock();
        }
    }

    /**
     * Sends the current context to the model using token streaming.
     */
    public void sendContextStreaming() {
        runningLock.lock();
        try {
            if (running) {
                log.info("Chat is busy. Cannot send context for streaming.");
                return;
            }
            setRunning(true);
            sendToModelStreaming();
        } finally {
            runningLock.unlock();
        }
    }

    /**
     * Orchestrates the asynchronous streaming interaction with the model.
     */
    private void sendToModelStreaming() {
        RequestContext rc = prepareRequest();

        statusManager.fireStatusChanged(ChatStatus.API_CALL_IN_PROGRESS);
        log.info("Starting streaming request to model '{}' with {} messages.",
                selectedModel.getModelId(), rc.history().size());

        selectedModel.generateContentStream(this, rc.config(), rc.history(), new StreamObserver<Response<? extends AbstractModelMessage>, AbstractModelMessage>() {
            /** 
             * Tracks the candidates received from the stream. 
             * This is used in onComplete to finalize the turn.
             */
            private List<AbstractModelMessage> candidatesFromStream;

            @Override
            public void onStart(List<AbstractModelMessage> candidates) {
                this.candidatesFromStream = candidates;
                
                if (candidates.size() == 1) {
                    // OPTIMIZATION: If there's only one candidate, we add it to the history 
                    // immediately. This allows the ConversationPanel to render it as it 
                    // streams, providing a more natural experience.
                    AbstractModelMessage candidate = candidates.get(0);
                    log.info("On Start adding message to the context manager before: " + contextManager.getHistory().size());
                    contextManager.addMessage(candidate);
                    log.info("On Start added message to the context manager after: " + contextManager.getHistory().size());
                    
                    // We keep activeCandidates empty so the selection panel stays hidden.
                    setActiveCandidates(Collections.emptyList());
                } else {
                    // If there are multiple candidates, we don't add them to history yet.
                    // They are held in the activeCandidates list for the selection panel.
                    setActiveCandidates(candidates);
                }
            }

            @Override
            public void onNext(Response<? extends AbstractModelMessage> response) {
                lastResponse = response;
            }

            @Override
            public void onComplete() {
                setRunning(false);
                statusManager.clearApiErrors();
                log.info("Streaming complete. {} candidates received.", candidatesFromStream != null ? candidatesFromStream.size() : 0);
                
                if (candidatesFromStream != null) {
                    candidatesFromStream.forEach(c -> c.setStreaming(false));
                    if (candidatesFromStream.size() == 1) {
                        // Finalize the single candidate (e.g., trigger tool execution).
                        chooseCandidate(candidatesFromStream.get(0));
                    } else if (candidatesFromStream.size() > 1) {
                        // Prompt the user to choose between multiple candidates.
                        statusManager.fireStatusChanged(ChatStatus.CANDIDATE_CHOICE_PROMPT);
                    }
                }
            }

            @Override
            public void onError(Throwable t) {
                setRunning(false);
                log.error("Error in streaming response", t);
                if (candidatesFromStream != null) {
                    candidatesFromStream.forEach(c -> c.setStreaming(false));
                }
                statusManager.fireStatusChanged(ChatStatus.ERROR);
            }
        });
    }

    /**
     * Adds a chosen model message to the history, handles tool execution, and
     * continues the conversation if necessary.
     *
     * @param message The model message to add.
     */
    public void chooseCandidate(@NonNull AbstractModelMessage message) {
        // Clear active candidates and add the chosen one to the history.
        setActiveCandidates(Collections.emptyList());
        
        if (!contextManager.getHistory().contains(message)) {
            contextManager.addMessage(message);
        }

        if (message.getToolMessage().isAutoRunnable()) {
            log.info("Auto-running {} tool calls.", message.getToolMessage().getToolResponses().size());
            message.getToolMessage().executeAllPending();
            log.info("Tool execution complete. Sending results back to the model.");
            
            if (config.isStreaming()) {
                sendContextStreaming();
            } else {
                sendContext();
            }
        }
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
     * Resets the entire chat session to a clean slate.
     */
    public void clear() {
        log.info("Clearing chat session {}", config.getSessionId());
        contextManager.clear();
        statusManager.reset();
        toolManager.reset();
        setActiveCandidates(Collections.emptyList());
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
        int threshold = config.getTokenThreshold();
        if (threshold <= 0) {
            return 0.0;
        }
        return (double) totalTokens / threshold;
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

    /**
     * Internal record to hold the context for a request.
     */
    private record RequestContext(RequestConfig config, List<AbstractMessage> history) {}
}
