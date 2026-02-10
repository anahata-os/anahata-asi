/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.context;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.provider.CoreContextProvider;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.ModelTextPart;
import uno.anahata.asi.model.core.PropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.core.Role;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.ThoughtSignature;
import uno.anahata.asi.model.core.UserMessage;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.tool.ToolManager;

/**
 * The definitive manager for a chat session's context in the V2 architecture.
 * This class owns the conversation history and orchestrates the hybrid context
 * assembly process, combining the V2 dynamic history with the hierarchical
 * provider model.
 * <p>
 * It manages a list of {@link ContextProvider}s, which now includes the
 * {@link uno.anahata.asi.resource.ResourceManager}, unifying stateful resources
 * with standard context injection.
 * </p>
 *
 * @author anahata-ai
 */
@Slf4j
@Getter
public class ContextManager extends BasicPropertyChangeSource implements Rebindable {

    /**
     * The parent chat session.
     */
    private final Chat chat;

    /**
     * The canonical conversation history. Access is manually synchronized to
     * avoid Kryo serialization issues with JDK synchronized wrappers.
     */
    private final List<AbstractMessage> history = new ArrayList<>();

    /**
     * Counter for assigning unique, sequential IDs to messages.
     */
    private final AtomicLong messageIdCounter = new AtomicLong(0);

    /**
     * Counter for assigning unique, sequential IDs to parts.
     */
    private final AtomicLong partIdCounter = new AtomicLong(0);

    /**
     * List of registered context providers.
     */
    private final List<ContextProvider> providers = new ArrayList<>();

    /**
     * The maximum number of tokens for the context window.
     */
    @Setter
    private int contextWindowSize;

    /**
     * Constructs a new ContextManager.
     *
     * @param chat The parent chat session.
     */
    public ContextManager(@NonNull Chat chat) {
        this.chat = chat;
    }

    /**
     * Initializes the manager and registers default providers. Implementation
     * details: Registers the Core provider, the ToolManager, and the
     * ResourceManager.
     */
    public void init() {
        registerContextProvider(new CoreContextProvider());
        registerContextProvider(chat.getToolManager());
        registerContextProvider(chat.getResourceManager());
    }

    /**
     * Clears the entire conversation history and resets all internal counters
     * to zero. Fires a property change event for the "history" property.
     */
    public void clear() {
        synchronized (history) {
            history.clear();
            messageIdCounter.set(0);
            partIdCounter.set(0);
        }
        log.info("ContextManager cleared for session {}", chat.getConfig().getSessionId());
        propertyChangeSupport.firePropertyChange("history", null, history);
    }

    /**
     * Registers a new context provider.
     *
     * @param provider The provider to register.
     */
    public void registerContextProvider(ContextProvider provider) {
        providers.add(provider);
        log.info("Registered context provider: {}", provider.getName());
    }

    /**
     * Gets the list of system instructions by processing all enabled providers
     * and their descendants in the hierarchy.
     *
     * @return A list of formatted system instruction strings.
     */
    public List<String> getSystemInstructions() {
        List<String> allSystemInstructions = new ArrayList<>();

        // Process providers and their children recursively
        for (ContextProvider rootProvider : providers) {
            for (ContextProvider provider : rootProvider.getFlattenedHierarchy(true)) {
                if (provider.isProviding()) {
                    long start = System.currentTimeMillis();
                    try {
                        List<String> systemInstructions = provider.getSystemInstructions();
                        long duration = System.currentTimeMillis() - start;
                        if (!systemInstructions.isEmpty()) {
                            StringBuilder sb = new StringBuilder();
                            sb.append(provider.getHeader());
                            for (String instruction : systemInstructions) {
                                sb.append("\n\n").append(instruction);
                            }
                            sb.append("\n\n(Provider ").append(provider.getName()).append(" took: ").append(duration).append("ms)");
                            allSystemInstructions.add(sb.toString());
                        }
                    } catch (Exception e) {
                        log.error("Error executing system instruction provider: {}", provider.getName(), e);
                        allSystemInstructions.add("Error executing system instruction provider: " + provider.getName() 
                                + "\n" + ExceptionUtils.getStackTrace(e));
                    }
                }
            }
        }

        return allSystemInstructions;
    }

    /**
     * Builds the final, filtered list of messages to be sent to the API. This
     * includes the main conversation history and a synthetic RAG message
     * populated by all active context providers.
     *
     * @return The filtered list of messages.
     */
    public List<AbstractMessage> buildVisibleHistory() {
        List<AbstractMessage> visibleHistory = new ArrayList<>();

        // 1. Add all messages from history. 
        synchronized (history) {
            visibleHistory.addAll(history);
        }
        
        log.info("Built visible history with {} messages (total history: {})", visibleHistory.size(), history.size());

        // 2. Add the synthetic RAG message for prompt augmentation.
        RagMessage augmentedMessage = new RagMessage(chat);
        augmentedMessage.addTextPart("--- Augmented Workspace Context ---\n"
                + "The following is high-salience, just-in-time context provided by the host environment for this turn. "
                + "It is dynamically generated and populated by enabled context providers. "
                + "This is NOT direct input from the user.");

        for (ContextProvider rootProvider : providers) {
            log.info("Augmenting context with Root provider: {}", rootProvider.getName());
            for (ContextProvider provider : rootProvider.getFlattenedHierarchy(true)) {
                if (provider.isProviding()) {
                    long start = System.currentTimeMillis();
                    try {
                        augmentedMessage.addTextPart(provider.getHeader());
                        provider.populateMessage(augmentedMessage);
                        long duration = System.currentTimeMillis() - start;
                        augmentedMessage.addTextPart("\n(Provider " + provider.getName() + " took: " + duration + "ms)");
                        log.info("Provider {} took {}ms", provider.getName(), duration);
                    } catch (Exception e) {
                        log.error("Error populating rag message for provider: {}", provider.getName(), e);
                        augmentedMessage.addTextPart("\nError populating rag message for provider: " + provider.getName() 
                                + "\n" + ExceptionUtils.getStackTrace(e));
                    }
                }
            }
        }
        visibleHistory.add(augmentedMessage);

        return visibleHistory;
    }

    /**
     * The definitive method for adding any message to the chat history. It
     * injects the chat reference, assigns sequential IDs to all parts, and
     * triggers the hard-pruning process.
     *
     * @param message The message to add.
     */
    public void addMessage(AbstractMessage message) {
        addMessageInternal(message);
        hardPrune();
    }

    /**
     * Checks it the model message has an associated tool message and adds it to
     * the history.
     *
     * @param modelMessage - the model message
     * @return The ensured tool message.
     * @throws IllegalStateException if the model message doesnt have an
     * associated tool message.
     */
    public AbstractToolMessage ensureToolMessageFollowsModelMessage(AbstractModelMessage modelMessage) {
        AbstractToolMessage toolMessage = modelMessage.getToolMessage();
        if (toolMessage == null) {
            throw new IllegalStateException("Model message does not contain a tool message");
        }

        synchronized (history) {
            if (history.contains(modelMessage)) {
                // CRITICAL FIX: Assign sequential IDs to the tool message and its parts before insertion.
                // This ensures the model recognizes the tool results in the next turn.
                identifyMessage(toolMessage);
                if (!history.contains(toolMessage)) {
                    // Insert it exactly after the model message
                    history.add(history.indexOf(modelMessage) + 1, toolMessage);
                    log.info("Inserted tool message {} into history after model message {}", toolMessage.getSequentialId(), modelMessage.getSequentialId());
                } 
            } else {
                log.error("cannotEnsure tool message follows model message because model message is not in history: {}", modelMessage);
            }
        }
        return toolMessage;
    }

    /**
     * Assigns sequential IDs to the message and all its parts.
     *
     * @param msg The message to identify.
     */
    private void identifyMessage(AbstractMessage msg) {
        log.info("identifyMessage {} ", msg);
        if (msg.getSequentialId() == 0) {
            log.info("Setting id for {} ", msg);
            msg.setSequentialId(messageIdCounter.incrementAndGet());
        }
        for (AbstractPart part : msg.getParts()) {
            if (part.getSequentialId() == 0) {
                log.info("Setting id for {} ", part);
                part.setSequentialId(partIdCounter.incrementAndGet());
            }
        }
    }

    /**
     * Adds a message to the history without triggering hard pruning. Assigns
     * sequential IDs to the message and all its parts. Fires a property change
     * event for the "history" property.
     *
     * @param message The message to add.
     */
    private void addMessageInternal(AbstractMessage message) {

        synchronized (history) {
            if (!history.contains(message)) {

                identifyMessage(message);
                log.info("Adding message {} to history", message);
                history.add(message);
                if (message instanceof AbstractModelMessage amm) {
                    if (!amm.getToolCalls().isEmpty()) {//its got tool calls
                        ensureToolMessageFollowsModelMessage(amm);
                    }
                }
            }
        }

        log.info("Added message {} to history size: {} firing event", message, history.size());
        propertyChangeSupport.firePropertyChange("history", null, history);
    }

    /**
     * Removes a message from the history. Fires a property change event for the
     * "history" property.
     *
     * @param message The message to remove.
     */
    public void removeMessage(AbstractMessage message) {
        boolean removed;
        synchronized (history) {
            removed = history.remove(message);
        }
        if (removed) {
            log.info("Removed message {} from history.", message.getSequentialId());
            propertyChangeSupport.firePropertyChange("history", null, history);
        }
    }

    /**
     * Performs a hard prune on the entire chat history, permanently deleting
     * parts that have reached their retention limit (remainingDepth <= 0) and are 
     * not explicitly pinned.
     */
    private void hardPrune() {
        synchronized (history) {
            for (AbstractMessage message : history) {
                // Create a copy to avoid ConcurrentModificationException when calling ap.remove()
                List<AbstractPart> allParts = new ArrayList<>(message.getParts(true));
                for (AbstractPart ap : allParts) {
                    // Hard prune if remainingDepth <= 0 AND it's not pinned (directly or via parent message).
                    if (ap.getRemainingDepth() <= 0 && !Boolean.FALSE.equals(ap.getPruned()) && !Boolean.FALSE.equals(message.isPruned())) {
                        ap.remove();
                    }
                }
            }
            // The 'Garbage Collector': Remove messages that are eligible for permanent deletion.
            history.removeIf(AbstractMessage::isGarbageCollectable);
        }
    }

    /**
     * Gets the complete, canonical conversation history for this session.
     *
     * @return A synchronized, unmodifiable list of all messages.
     */
    public List<AbstractMessage> getHistory() {
        synchronized (history) {
            return new ArrayList<>(history);
        }
    }

    /**
     * Gets the current total token count of all parts in the context.
     *
     * @return The total token count.
     */
    public int getTotalTokenCount() {
        return chat.getLastTotalTokenCount();
    }

    /**
     * Gets the maximum number of tokens allowed in the context window.
     *
     * @return The token threshold.
     */
    public int getTokenThreshold() {
        return chat.getConfig().getTokenThreshold();
    }

    /**
     * {@inheritDoc} Restores the standard context providers after
     * deserialization.
     */
    @Override
    public void rebind() {
        super.rebind();
        log.info("Rebinding ContextManager for session: {}", chat.getConfig().getSessionId());

        // Ensure standard providers are registered (they might be missing if session was saved in an older version)
        boolean hasCore = false;
        boolean hasTools = false;
        boolean hasResources = false;

        for (ContextProvider p : providers) {
            if (p instanceof CoreContextProvider) {
                hasCore = true;
            }
            if (p instanceof ToolManager) {
                hasTools = true;
            }
            if (p instanceof ResourceManager) {
                hasResources = true;
            }
        }

        if (!hasCore) {
            registerContextProvider(new CoreContextProvider());
        }
        if (!hasTools) {
            registerContextProvider(chat.getToolManager());
        }
        if (!hasResources) {
            registerContextProvider(chat.getResourceManager());
        }
    }
}
