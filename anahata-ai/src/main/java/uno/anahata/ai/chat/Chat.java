/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.chat;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.AiExecutors;
import uno.anahata.ai.chat.ChatConfig;
import uno.anahata.ai.context.ContextManager;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.ModelMessage;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.ToolMessage;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.model.tool.AbstractToolCall;
import uno.anahata.ai.status.StatusManager;
import uno.anahata.ai.tool.ToolManager;

/**
 * The central, provider-agnostic orchestrator for a single chat session in the V2 architecture.
 * This class manages the conversation flow, orchestrates calls to the AI provider,
 * and delegates context management to a specialized ContextManager.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@Getter
public class Chat {

    private final ChatConfig config;
    private final ToolManager toolManager;
    private final ContextManager contextManager;
    private final ExecutorService executor;
    private final StatusManager statusManager;
    private final List<AbstractAiProvider> providers = new ArrayList<>();

    /** The user-defined name for this chat session. */
    @Setter
    private String name;
    
    /** The currently selected model for the chat session. */
    @Setter
    private AbstractModel selectedModel;
    
    /** A thread-safe flag indicating if the main chat loop is currently active. */
    private volatile boolean running = false;

    /** A queue for a single user message, used to handle input that arrives while the chat loop is busy. */
    private UserMessage stagedUserMessage;

    @SneakyThrows
    public Chat(@NonNull ChatConfig config) {
        this.config = config;
        this.name = config.getSessionId(); // Default name to session ID
        this.executor = AiExecutors.newCachedThreadPoolExecutor(config.getSessionId());
        this.toolManager = new ToolManager(this);
        this.contextManager = new ContextManager(this);
        this.statusManager = new StatusManager(this);
        
        // Crucially, set the back-reference *before* initializing managers
        this.config.setChat(this);
        
        contextManager.init();
        
        // Discover and instantiate providers
        for (Class<? extends AbstractAiProvider> providerClass : config.getProviderClasses()) {
            try {
                // Instantiate the provider, passing this Chat instance.
                AbstractAiProvider provider = providerClass.getConstructor(Chat.class).newInstance(this);
                this.providers.add(provider);
                log.info("Successfully instantiated and registered provider: {}", provider.getProviderId());
            } catch (Exception e) {
                log.error("Failed to instantiate provider class: {}", providerClass.getName(), e);
            }
        }
    }

    /**
     * Sends a user message to the currently selected AI model and returns the response.
     * This method does NOT modify the conversation history. The caller is responsible
     * for choosing a candidate from the response and adding it to the history via
     * the {@link #chooseCandidate(ModelMessage)} method.
     *
     * @param message The user's message.
     * @return The model's response.
     */
    public Response sendMessage(UserMessage message) {
        if (selectedModel == null) {
            throw new IllegalStateException("A model must be selected before sending a message. Call setSelectedModel().");
        }
        
        contextManager.addMessage(message);
        
        RequestConfig requestConfig = config.getRequestConfig();
        List<AbstractMessage> history = contextManager.buildVisibleHistory();

        log.info("Sending request to model '{}' with {} messages in history ({} sent to API).",
                 selectedModel.getModelId(), contextManager.getHistory().size(), history.size());
        
        Response response = selectedModel.generateContent(requestConfig, history);
        
        // TODO: Implement the full tool processing logic from V1's Chat.java
        
        return response;
    }
    
    /**
     * Adds a chosen model message (a candidate from a Response) to the conversation history.
     * If the message contains tool calls, it automatically creates and adds the corresponding
     * ToolMessage to the history as well.
     * @param message The model message to add.
     */
    public void chooseCandidate(@NonNull ModelMessage message) {
        contextManager.addMessage(message);
        
        List<AbstractToolCall> toolCalls = message.getToolCalls();
        if (!toolCalls.isEmpty()) {
            ToolMessage toolMessage = new ToolMessage();
            toolCalls.forEach(tc -> toolMessage.getParts().add(tc.getResponse()));
            contextManager.addMessage(toolMessage);
        }
    }
    
    /**
     * Gets a flattened list of all models available from all registered providers.
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
