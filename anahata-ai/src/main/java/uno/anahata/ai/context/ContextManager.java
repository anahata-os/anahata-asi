/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.Chat;
import uno.anahata.ai.context.provider.AbstractContextProvider;
import uno.anahata.ai.context.provider.ChatStatusProvider;
import uno.anahata.ai.context.provider.ContextPosition;
import uno.anahata.ai.context.provider.ContextSummaryProvider;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.core.UserMessage;

/**
 * The definitive manager for a chat session's context in the V2/V3 architecture.
 * This class owns the conversation history and orchestrates the hybrid context
 * assembly process, combining the V3 dynamic history with the V1 provider model.
 *
 * @author anahata-ai
 */
@Slf4j
@Getter
public class ContextManager {
    private final Chat chat;
    private final List<AbstractMessage> history = Collections.synchronizedList(new ArrayList<>());
    private final AtomicLong partIdCounter = new AtomicLong(0);
    private final List<AbstractContextProvider> providers = new ArrayList<>();
    
    /** The maximum number of tokens for the context window. */
    @Setter
    private int contextWindowSize;

    public ContextManager(@NonNull Chat chat) {
        this.chat = chat;
    }
    
    public void init() {
        // Register default providers
        registerProvider(new ContextSummaryProvider());
        registerProvider(new ChatStatusProvider());
    }
    
    public void registerProvider(AbstractContextProvider provider) {
        providers.add(provider);
        log.info("Registered context provider: {}", provider.getName());
    }

    /**
     * Gets the list of system instructions by processing all enabled providers
     * at the SYSTEM_INSTRUCTIONS position.
     * @return A list of TextParts for system instructions.
     */
    public List<TextPart> getSystemInstructions() {
        List<TextPart> instructions = new ArrayList<>();
        for (AbstractContextProvider provider : providers) {
            if (provider.isEnabled() && provider.getPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
                try {
                    provider.getParts(chat).forEach(p -> {
                        if (p instanceof TextPart) {
                            instructions.add((TextPart) p);
                        }
                    });
                } catch (Exception e) {
                    log.error("Error executing system instruction provider: {}", provider.getName(), e);
                }
            }
        }
        return instructions;
    }

    /**
     * Builds the final, filtered list of messages to be sent to the API.
     * This includes the main conversation history and any just-in-time context
     * from PROMPT_AUGMENTATION providers.
     * @return The filtered list of messages.
     */
    public List<AbstractMessage> buildVisibleHistory() {
        boolean includePruned = chat.getConfig().getRequestConfig().isIncludePruned();
        
        // 1. Get the filtered main history
        List<AbstractMessage> visibleHistory = history.stream()
            .filter(msg -> includePruned || !msg.isEffectivelyPruned())
            .collect(Collectors.toList());
            
        // 2. Process prompt augmentation providers
        for (AbstractContextProvider provider : providers) {
            if (provider.isEnabled() && provider.getPosition() == ContextPosition.PROMPT_AUGMENTATION) {
                try {
                    List<AbstractPart> parts = provider.getParts(chat);
                    if (parts != null && !parts.isEmpty()) {
                        UserMessage augmentedMessage = new UserMessage();
                        String header = String.format("--- Augmented Workspace Context ---\nProvider: **%s** (id: **%s**):\n", provider.getName(), provider.getId());
                        augmentedMessage.getParts().add(new TextPart(header));
                        augmentedMessage.getParts().addAll(parts);
                        visibleHistory.add(augmentedMessage);
                    }
                } catch (Exception e) {
                    log.error("Error executing prompt augmentation provider: {}", provider.getName(), e);
                }
            }
        }
        
        return visibleHistory;
    }

    /**
     * The definitive method for adding any message to the chat history.
     * It injects the chat reference, assigns sequential IDs to all parts,
     * and triggers the hard-pruning process.
     * @param message The message to add.
     */
    public void addMessage(@NonNull AbstractMessage message) {
        message.setChat(chat);
        message.setSequenceNumber(history.size());
        for (AbstractPart part : message.getParts()) {
            part.setSequentialId(partIdCounter.incrementAndGet());
            part.setMessage(message);
        }
        history.add(message);
        hardPrune();
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
                message.getParts().removeIf(part ->
                    part.isEffectivelyPruned() && part.getTurnsLeft() < -hardPruneDelay
                );
            }
            // Remove empty messages
            history.removeIf(msg -> msg.getParts().isEmpty() && !msg.isEffectivelyPruned());
        }
    }
    
    /**
     * Gets the complete, canonical conversation history for this session.
     * @return A synchronized, unmodifiable list of all messages.
     */
    public List<AbstractMessage> getHistory() {
        return Collections.unmodifiableList(history);
    }
}
