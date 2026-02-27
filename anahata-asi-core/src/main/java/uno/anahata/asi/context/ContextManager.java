/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.context;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicLong;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.provider.CoreContextProvider;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.tool.ToolManager;

/**
 * The definitive manager for a agi session's context in the V2 architecture.
 * This class owns the conversation history and orchestrates the hybrid context
 * assembly process, combining the V2 dynamic history with the hierarchical
 * provider model.
 *
 * @author anahata
 */
@Slf4j
@Getter
public class ContextManager extends BasicPropertyChangeSource implements Rebindable {

    /**
     * The parent agi session.
     */
    private final Agi agi;

    /**
     * The canonical conversation history. Uses CopyOnWriteArrayList to allow 
     * thread-safe iteration during background serialization (e.g., auto-save) 
     * without blocking streaming updates.
     */
    private final List<AbstractMessage> history = new CopyOnWriteArrayList<>();

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
     * @param agi The parent agi session.
     */
    public ContextManager(@NonNull Agi agi) {
        this.agi = agi;
    }

    /**
     * Initializes the manager and registers default providers.
     */
    public void init() {
        registerContextProvider(new CoreContextProvider());
        registerContextProvider(agi.getToolManager());
        registerContextProvider(agi.getResourceManager());
    }

    /**
     * Clears the entire conversation history and resets all internal counters
     * to zero. Fires a property change event for the "history" property.
     */
    public void clear() {
        history.clear();
        messageIdCounter.set(0);
        partIdCounter.set(0);
        log.info("ContextManager cleared for session {}", agi.getConfig().getSessionId());
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
     * Gets the list of system instructions by processing all enabled providers.
     *
     * @return A list of formatted system instruction strings.
     */
    public List<String> getSystemInstructions() {
        List<String> allSystemInstructions = new ArrayList<>();

        for (ContextProvider rootProvider : providers) {
            for (ContextProvider provider : rootProvider.getFlattenedHierarchy(true)) {
                if (provider.isProviding()) {
                    try {
                        List<String> systemInstructions = provider.getSystemInstructions();
                        if (provider instanceof AbstractResource) {
                            allSystemInstructions.add(provider.getHeader());
                        }
                        allSystemInstructions.addAll(systemInstructions);
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
     * Builds the final, filtered list of messages to be sent to the API.
     *
     * @return The filtered list of messages.
     */
    public List<AbstractMessage> buildVisibleHistory() {
        List<AbstractMessage> visibleHistory = new ArrayList<>(history);

        log.info("Built visible history with {} messages (total history: {})", visibleHistory.size(), history.size());

        RagMessage augmentedMessage = new RagMessage(agi);
        augmentedMessage.addTextPart("--- RAG message ---\n"
                + "The following is high-salience, just-in-time context provided by the host environment for this turn. "
                + "It is dynamically generated and populated by enabled context providers. "
                + "This is NOT direct input from the user.");

        for (ContextProvider rootProvider : providers) {
            for (ContextProvider provider : rootProvider.getFlattenedHierarchy(true)) {
                if (provider.isProviding()) {
                    long start = System.currentTimeMillis();
                    try {
                        augmentedMessage.addTextPart(provider.getHeader());
                        provider.populateMessage(augmentedMessage);
                        long duration = System.currentTimeMillis() - start;
                        augmentedMessage.addTextPart("\n(Provider " + provider.getName() + " took: " + duration + "ms)");
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
     * The definitive method for adding any message to the agi history.
     *
     * @param message The message to add.
     */
    public void addMessage(AbstractMessage message) {
        addMessageInternal(message);
        hardPrune();
    }

    /**
     * Assigns sequential IDs to the message and all its parts.
     *
     * @param msg The message to identify.
     */
    private void identifyMessage(AbstractMessage msg) {
        if (msg.getSequentialId() == 0) {
            msg.setSequentialId(messageIdCounter.incrementAndGet());
        }
        for (AbstractPart part : msg.getParts()) {
            identifyPart(part);
        }
    }

    /**
     * Assigns a sequential ID to a single part if it doesn't already have one.
     *
     * @param part The part to identify.
     */
    public void identifyPart(AbstractPart part) {
        if (part.getSequentialId() == 0) {
            part.setSequentialId(partIdCounter.incrementAndGet());
        }
    }

    /**
     * Adds a message to the history without triggering hard pruning.
     *
     * @param message The message to add.
     */
    private void addMessageInternal(AbstractMessage message) {
        if (!history.contains(message)) {
            identifyMessage(message);
            log.info("Adding message {} to history", message.getSequentialId());
            history.add(message);
        }
        propertyChangeSupport.firePropertyChange("history", null, history);
    }

    /**
     * Removes a message from the history.
     *
     * @param message The message to remove.
     */
    public void removeMessage(AbstractMessage message) {
        if (history.remove(message)) {
            log.info("Removed message {} from history.", message.getSequentialId());
            propertyChangeSupport.firePropertyChange("history", null, history);
        }
    }

    /**
     * Performs a hard prune on the entire agi history. 
     * This implementing the Atomic Message model: messages stay structurally 
     * intact until all their parts are collectable. Heavy parts (Blobs/Thoughts) 
     * are surgically removed when they expire.
     */
    private void hardPrune() {
        // Pass 1: Surgical removal of collectable parts (Blobs, Thoughts, etc.)
        for (AbstractMessage message : history) {
            List<AbstractPart> allParts = new ArrayList<>(message.getParts(true));
            for (AbstractPart ap : allParts) {
                if (ap.isGarbageCollectable()) {
                    ap.remove();
                }
            }
        }
        // Pass 2: Atomic removal of collectable messages
        history.removeIf(AbstractMessage::isGarbageCollectable);
    }

    /**
     * Gets the complete, canonical conversation history for this session.
     *
     * @return A synchronized, unmodifiable list of all messages.
     */
    public List<AbstractMessage> getHistory() {
        return Collections.unmodifiableList(history);
    }

    /**
     * Gets the current total token count of all parts in the context.
     *
     * @return The total token count.
     */
    public int getTotalTokenCount() {
        return agi.getLastTotalTokenCount();
    }

    /**
     * Gets the maximum number of tokens allowed in the context window.
     *
     * @return The token threshold.
     */
    public int getTokenThreshold() {
        return agi.getConfig().getTokenThreshold();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void rebind() {
        super.rebind();
        log.info("Rebinding ContextManager for session: {}", agi.getConfig().getSessionId());

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
            registerContextProvider(agi.getToolManager());
        }
        if (!hasResources) {
            registerContextProvider(agi.getResourceManager());
        }
    }
}
