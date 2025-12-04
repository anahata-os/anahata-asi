/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.chat;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.AiExecutors;
import uno.anahata.ai.context.ContextManager;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.resource.ResourceManager;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.status.StatusManager;
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
public class Chat {

    private final ChatConfig config;
    private final ToolManager toolManager;
    private final ContextManager contextManager;
    private final ResourceManager resourceManager;
    private final ExecutorService executor;
    private final StatusManager statusManager;
    private final List<AbstractAiProvider> providers = new ArrayList<>();

    /**
     * The currently selected model for the chat session.
     */
    @Setter
    private AbstractModel selectedModel;

    /**
     * A thread-safe flag indicating if the main chat loop is currently active.
     */
    private volatile boolean running = false;

    @SneakyThrows
    public Chat(@NonNull ChatConfig config) {
        this.config = config;
        this.executor = AiExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.toolManager = new ToolManager(this);
        this.contextManager = new ContextManager(this);
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
    }

    /**
     * The primary entry point for the UI to send a message. This method is
     * designed to be called from a background thread (e.g., a SwingWorker). It
     * adds the message to the context and then ensures the processing loop is
     * running.
     * <ul>
     * <li>If the chat is idle, this method will start the processing loop and
     * block the calling thread until the entire conversation turn is
     * complete.</li>
     * <li>If the chat is already busy, it simply adds the message to the
     * context and returns immediately. The ongoing loop will pick up the new
     * message on its next iteration.</li>
     * </ul>
     *
     * @param message The user's message.
     */
    public void sendMessage(@NonNull UserMessage message) {
        contextManager.addMessage(message);

        if (running) {
            log.info("Chat is busy. Message added to context and will be processed in the current turn.");
            return;
        }

        running = true;
        try {
            sendToModel();
        } finally {
            running = false;
        }
    }

    /**
     * The core method for interacting with the model. It builds the history,
     * sends it, and processes the response, including handling retries and
     * multiple candidates.
     */
    private void sendToModel() {
        if (selectedModel == null) {
            throw new IllegalStateException("A model must be selected before sending a message.");
        }

        int maxRetries = config.getApiMaxRetries();
        long initialDelayMillis = config.getApiInitialDelayMillis();
        long maxDelayMillis = config.getApiMaxDelayMillis();

        for (int attempt = 0; attempt < maxRetries; attempt++) {
            try {
                // Build config and history *inside* the loop for freshness on retries
                RequestConfig requestConfig = config.getRequestConfig();
                List<AbstractMessage> history = contextManager.buildVisibleHistory();

                statusManager.fireStatusChanged(ChatStatus.API_CALL_IN_PROGRESS);
                log.info("Sending request to model '{}' (attempt {}/{}) with {} messages.",
                         selectedModel.getModelId(), attempt + 1, maxRetries, history.size());

                Response<?> response = selectedModel.generateContent(this, requestConfig, history);

                if (response.getCandidates().size() == 1) {
                    chooseCandidate(response.getCandidates().get(0));
                } else {
                    log.info("Model returned multiple candidates. Pausing for user selection.");
                    statusManager.fireStatusChanged(ChatStatus.CANDIDATE_CHOICE_PROMPT);
                }
                return; // Success, exit the retry loop.

            } catch (Exception e) {
                log.warn("API Error on attempt {}: {}", attempt + 1, e.toString());

                if (attempt == maxRetries - 1) {
                    log.error("Max retries reached. Aborting.", e);
                    statusManager.fireStatusChanged(ChatStatus.MAX_RETRIES_REACHED);
                    // Re-throw the final exception to be handled by the UI layer
                    throw new RuntimeException("Failed after " + maxRetries + " attempts.", e);
                }

                // Calculate exponential backoff with jitter
                long delay = (long) (initialDelayMillis * Math.pow(2, attempt)) + (long) (Math.random() * 500);
                long backoffAmount = Math.min(delay, maxDelayMillis);

                try {
                    statusManager.fireStatusChanged(ChatStatus.WAITING_WITH_BACKOFF, "Retrying in " + backoffAmount + "ms");
                    Thread.sleep(backoffAmount);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new RuntimeException("Chat interrupted during retry delay.", ie);
                }
            }
        }
    }

    /**
     * Adds a chosen model message to the history, handles tool execution, and
     * continues the conversation if necessary.
     *
     * @param message The model message to add.
     */
    public void chooseCandidate(@NonNull AbstractModelMessage message) {

        contextManager.addMessage(message);

        if (message.getToolMessage().isAutoRunnable()) {
            log.info("Auto-running {} tool calls.", message.getToolMessage().getToolResponses().size());
            message.getToolMessage().executeAllPending();

            // Automatically send the results back to the model to continue the conversation.
            log.info("Tool execution complete. Sending results back to the model.");
            sendToModel();
        }
    }

    /**
     * Resets the entire chat session to a clean slate, ready for a new
     * conversation. This clears all history, resources, and resets the session
     * ID and all counters.
     */
    public void clear() {
        log.info("Clearing chat session {}", config.getSessionId());

        // 1. Clear history and all context-related counters.
        contextManager.clear();

        // 2. Reset status
        statusManager.reset();

        // 3. Reset tool call counters
        toolManager.reset();

        // 4. Start a new session by updating the ID and name
        String newSessionId = UUID.randomUUID().toString();
        config.setSessionId(newSessionId);
        config.setName(null); // Clear the name, let it default or be set by user

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

    public void shutdown() {
        log.info("Shutting down Chat for session {}", config.getSessionId());
        if (executor != null && !executor.isShutdown()) {
            executor.shutdown();
        }
    }
}
