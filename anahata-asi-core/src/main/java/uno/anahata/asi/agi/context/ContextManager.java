/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi.context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.context.core.CoreContextProvider;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.resource.Resource;

/**
 * The definitive manager for an Agi session's context in the V2 architecture.
 * <p>
 * This manager orchestrates the 'Working Memory' of Anahata, maintaining the 
 * conversation history, coordinating Retrieval-Augmented Generation (RAG), 
 * and enforcing context window limits via the {@link ContextWindowGarbageCollector}.
 * </p>
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
     * The specialized garbage collector for this context.
     */
    private final ContextWindowGarbageCollector garbageCollector = new ContextWindowGarbageCollector(this);

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
        registerContextProvider(new CoreContextProvider(agi));
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
        garbageCollector.clearLog();
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
                        if (provider instanceof Resource ar) {
                            if (ar.getContextPosition() != ContextPosition.SYSTEM_INSTRUCTIONS) {
                                continue;
                            }
                            ar.reloadIfNeeded();
                            allSystemInstructions.add(provider.getHeader());
                        }

                        allSystemInstructions.addAll(provider.getSystemInstructions());
                    } catch (Exception e) {
                        log.error("Error executing system instruction provider: {}", provider.getName(), e);
                        allSystemInstructions.add("Exception getting content from system instructions provider: " + provider.getName()
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

        visibleHistory.add(buildRagMessage());

        return visibleHistory;
    }

    /**
     * Constructs the RAG (Retrieval-Augmented Generation) message by
     * aggregating content from all enabled context providers.
     *
     * @return A fully populated RagMessage.
     */
    public RagMessage buildRagMessage() {
        long ts = System.currentTimeMillis();
        RagMessage augmentedMessage = new RagMessage(agi);
        augmentedMessage.addTextPart("--- **RAG message** ---\n"
                + "The following is high-salience, just-in-time live context provided by for this turn.\n"                
                + "It has been dynamically generated and populated by all managed resources with a `PROMPT_AUGMENTATION` context position"
                + " and by all effectively providing context providers. The RAG message is and will always be the last message in the prompt"
                + " on every turn and as you can see, it has no message-level or part-level metadata because is not a part of the history of the conversation (its just dinamycally generated on every turn). Instead of message level metadata or part level metadata, it has the id of all registered context providers and the uuid of all registered reources that make the content of this message.\n"
                + "All resources with a `LIVE` refresh policy have been reloaded from disk and are garanteed to be up to date and in sync with the underlying device regardless of what the last tool calls did.\n"
                + "Use the `lastModified` timestamp provided on the header of each resource when you need to modify a resource and take the line numbers on the `LIVE` resources as the only source of truth.\n"
                + "Even though this message has a user role, it is not direct input from the user. As a matter of fact, the user probably doesnt even know what the RAG message is.");

        for (ContextProvider rootProvider : providers) {
            for (ContextProvider provider : rootProvider.getFlattenedHierarchy(true)) {
                if (provider.isProviding()) {

                    long start = System.currentTimeMillis();
                    try {
                        if (provider instanceof Resource r) {
                            if (r.getContextPosition() != ContextPosition.PROMPT_AUGMENTATION) {
                                continue;
                            }
                            r.reloadIfNeeded();
                        }
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
        log.info("buildRagMessage took " + ts + " ms.");
        return augmentedMessage;
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
     * Performs a hard prune on the entire agi history. In the strict atomic
     * model, messages stay structurally intact until all their parts are
     * collectable.
     */
    private void hardPrune() {
        List<AbstractMessage> toRemove = history.stream()
                .filter(AbstractMessage::isGarbageCollectable)
                .collect(Collectors.toList());

        for (AbstractMessage msg : toRemove) {
            log.info("Garbage collecting message turn {} ({} tokens recycled)", msg.getSequentialId(), msg.getTokenCount(true));
            garbageCollector.recordCollection(msg);
            history.remove(msg);
        }

        if (!toRemove.isEmpty()) {
            propertyChangeSupport.firePropertyChange("history", null, history);
        }
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
    public int getLastTotalTokenCount() {
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
}
