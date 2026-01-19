/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.context;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.provider.CoreContextProvider;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.PropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.TextPart;

/**
 * The definitive manager for a chat session's context in the V2
 * architecture. This class owns the conversation history and orchestrates the
 * hybrid context assembly process, combining the V2 dynamic history with the V1
 * provider model.
 *
 * @author anahata-ai
 */
@Slf4j
@Getter
public class ContextManager implements PropertyChangeSource {

    /** Support for firing property change events. */
    private final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

    /** The parent chat session. */
    private final Chat chat;
    
    /** The canonical conversation history, stored as a thread-safe list. */
    private final List<AbstractMessage> history = Collections.synchronizedList(new ArrayList<>());
    
    /** Counter for assigning unique, sequential IDs to messages. */
    private final AtomicLong messageIdCounter = new AtomicLong(0);
    
    /** Counter for assigning unique, sequential IDs to parts. */
    private final AtomicLong partIdCounter = new AtomicLong(0);
    
    /** List of registered context providers. */
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
     * Initializes the manager and registers default providers.
     */
    public void init() {
        
        registerContextProvider(new CoreContextProvider());
        // Register toolkits as context providers
        for (ContextProvider cp : chat.getToolManager().getContextProviderToolkits()) {
            registerContextProvider(cp);
        }
        
    }
    
    /**
     * Clears the entire conversation history and resets all internal counters to zero.
     * Fires a property change event for the "history" property.
     */
    public void clear() {
        history.clear();
        messageIdCounter.set(0);
        partIdCounter.set(0);
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
     * and all managed resources at the SYSTEM_INSTRUCTIONS position.
     *
     * @return A list of TextParts for system instructions.
     */
    public List<String> getSystemInstructions() {
        List<String> allSystemInstructions = new ArrayList<>();

        // 1. Process providers
        for (ContextProvider provider : providers) {
            String providerChunk = provider.getHeader();
            if (provider.isEnabled()) {                                
                try {                  
                    List<String> systemInstructions = provider.getSystemInstructions(chat);
                    for (String string : systemInstructions) {
                        providerChunk+= "\n" + string;
                    }
                } catch (Exception e) {
                    log.error("Error executing system instruction provider: {}", provider.getName(), e);
                }
            }
            allSystemInstructions.add(providerChunk);
        }
        
        
        return allSystemInstructions;
    }

    /**
     * Builds the final, filtered list of messages to be sent to the API. This
     * includes the main conversation history and any just-in-time context from
     * PROMPT_AUGMENTATION providers and resources.
     *
     * @return The filtered list of messages.
     */
    public List<AbstractMessage> buildVisibleHistory() {
        boolean includePruned = chat.getConfig().getRequestConfig().isIncludePruned();

        List<AbstractMessage> visibleHistory = new ArrayList<>();
        RagMessage augmentedMessage = new RagMessage(chat);

        synchronized (history) {
            // 1. Get the filtered main history
            history.stream()
                    .filter(msg -> includePruned || !msg.isEffectivelyPruned())
                    .forEach(visibleHistory::add);

            // 2. Inject pruned message ranges summary if not including pruned messages.
            if (!includePruned) {
                List<String> prunedRanges = findPrunedMessageRanges();
                if (!prunedRanges.isEmpty()) {
                    new TextPart(augmentedMessage, "--- PRUNED MESSAGE RANGES ---");
                    for (String range : prunedRanges) {
                        new TextPart(augmentedMessage, range);
                    }
                }
            }
        }

        for (ContextProvider provider : providers) {            
            if (provider.isEnabled()) {                                
                try {                  
                    String providerChunk = provider.getHeader();
                    new TextPart(augmentedMessage, providerChunk);
                    provider.populateMessage(augmentedMessage);
                } catch (Exception e) {
                    log.error("Error populating rag message for provider: {}", provider.getName(), e);
                }
            }
        }
        visibleHistory.add(augmentedMessage);

        return visibleHistory;
    }

    /**
     * Identifies contiguous ranges of effectively pruned messages in the history.
     * This method assumes it is called within a synchronized block on 'history'.
     * 
     * @return A list of strings describing the pruned ranges.
     */
    private List<String> findPrunedMessageRanges() {
        List<String> ranges = new ArrayList<>();
        int start = -1;
        for (int i = 0; i < history.size(); i++) {
            AbstractMessage msg = history.get(i);
            if (msg.isEffectivelyPruned()) {
                if (start == -1) {
                    start = i;
                }
            } else {
                if (start != -1) {
                    ranges.add(formatRange(start, i - 1));
                    start = -1;
                }
            }
        }
        if (start != -1) {
            ranges.add(formatRange(start, history.size() - 1));
        }
        return ranges;
    }

    /**
     * Formats a range of message indices into a human-readable string.
     */
    private String formatRange(int startIdx, int endIdx) {
        long startId = history.get(startIdx).getSequentialId();
        long endId = history.get(endIdx).getSequentialId();
        if (startId == endId) {
            return String.format("- Message ID: %d is PRUNED", startId);
        } else {
            return String.format("- Messages ID: %d to %d are PRUNED", startId, endId);
        }
    }

    /**
     * The definitive method for adding any message to the chat history. It
     * injects the chat reference, assigns sequential IDs to all parts, and
     * triggers the hard-pruning process.
     *
     * @param message The message to add.
     */
    public synchronized void addMessage(AbstractMessage message) {

        addMessageInternal(message);
        
        hardPrune();
    }
    
    /**
     * Checks it the model message has an associated tool message and adds it to the history.
     * 
     * @param modelMessage - the model message
     * @throws IllegalStateException if the model message doesnt have an associated tool message.
     */
    public synchronized void ensureToolMessageFolllowsModelMessage(AbstractModelMessage modelMessage) {
        if (modelMessage.getToolMessage() == null) {
            throw new IllegalStateException("Model message does not contain a tool message");
        }
        
        if (history.contains(modelMessage) && !history.contains(modelMessage.getToolMessage())) {
            //insert it exactly after the model message
            history.add(history.indexOf(modelMessage) + 1, modelMessage.getToolMessage());
        } 
    }

    /**
     * Adds a message to the history without triggering hard pruning.
     * Assigns sequential IDs to the message and all its parts.
     * Fires a property change event for the "history" property.
     *
     * @param message The message to add.
     */
    private void addMessageInternal(AbstractMessage message) {
        message.setSequentialId(this.messageIdCounter.incrementAndGet());
        for (AbstractPart part : message.getParts()) {
            part.setSequentialId(partIdCounter.incrementAndGet());
        }
        history.add(message);
        if (message instanceof AbstractModelMessage amm) {
            if (amm.getToolMessage() != null) {//its got tool calls
                ensureToolMessageFolllowsModelMessage(amm);
            }
        }
        
        log.info("Added message {} to history size: {} firing event", message, history.size());
        propertyChangeSupport.firePropertyChange("history", null, history);
    }
    
    /**
     * Removes a message from the history.
     * Fires a property change event for the "history" property.
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
     * Performs a hard prune on the entire chat history, permanently deleting
     * parts that have been soft-pruned for longer than the configured delay.
     */
    private void hardPrune() {
        int hardPruneDelay = chat.getConfig().getHardPruneDelay();
        if (hardPruneDelay < 0) {
            return; // Hard pruning disabled
        }
        synchronized (history) {
            for (AbstractMessage message : history) {
                // Use getParts(true) to iterate over all parts, including soft-pruned ones
                for (AbstractPart ap : message.getParts(true)) {
                    if (ap.isEffectivelyPruned() && ap.getTurnsLeft() < -hardPruneDelay) {
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
        return new ArrayList<>(history);
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
     * Adds a PropertyChangeListener to this manager.
     * @param listener The listener to add.
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes a PropertyChangeListener from this manager.
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
