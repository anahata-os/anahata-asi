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
import java.util.stream.Collectors;
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
import uno.anahata.asi.model.resource.AbstractPathResource;
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
        registerContextProvider(new CoreContextProvider(agi));
        registerContextProvider(agi.getToolManager());
        registerContextProvider(agi.getResourceManager2());
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
                        if (provider instanceof AbstractResource ar) {
                            if (ar.getContextPosition() != ContextPosition.SYSTEM_INSTRUCTIONS) {
                                continue;
                            }
                            allSystemInstructions.add(provider.getHeader());
                        }
                        
                        allSystemInstructions.addAll(provider.getSystemInstructions());
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
                    // CRITICAL: Respect ContextPosition for resources. 
                    // Resources in SYSTEM_INSTRUCTIONS must not appear in the RAG message.
                    if (provider instanceof AbstractResource ar && ar.getContextPosition() != ContextPosition.PROMPT_AUGMENTATION) {
                        continue;
                    }
                    
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
     * In the strict atomic model, messages stay structurally intact until 
     * all their parts are collectable. 
     */
    private void hardPrune() {
        List<AbstractMessage> toRemove = history.stream()
                .filter(AbstractMessage::isGarbageCollectable)
                .collect(Collectors.toList());
        
        for (AbstractMessage msg : toRemove) {
            int tokens = msg.getTokenCount(true);
            log.info("Garbage collecting message turn {} ({} tokens recycled)", msg.getSequentialId(), tokens);
            history.remove(msg);
        }
        
        if (!toRemove.isEmpty()) {
            propertyChangeSupport.firePropertyChange("history", null, history);
        }
    }

    /**
     * Calculates the total token count of all parts in the history that 
     * are currently effectively pruned.
     * 
     * @return The total pruned token count.
     */
    public int getAllEffectivelyPrunedPartsTokenCount() {
        return history.stream()
                .flatMap(m -> m.getParts().stream())
                .filter(AbstractPart::isEffectivelyPruned)
                .mapToInt(AbstractPart::getTokenCount)
                .sum();
    }

    /**
     * Calculates the total token count of the metadata headers for all parts 
     * in the history that are currently effectively pruned.
     * 
     * @return The total pruned metadata token count.
     */
    public int getAllEffectivelyPrunedMetadataTokenCount() {
        return history.stream()
                .flatMap(m -> m.getParts().stream())
                .filter(AbstractPart::isEffectivelyPruned)
                .mapToInt(AbstractPart::getMetadataTokenCount)
                .sum();
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
            registerContextProvider(new CoreContextProvider(agi));
        }
        if (!hasTools) {
            registerContextProvider(agi.getToolManager());
        }
        if (!hasResources) {
            registerContextProvider(agi.getResourceManager());
        }
    }
}
