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
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.context.system.AbstractSystemInstructionsProvider;
import uno.anahata.ai.context.system.ChatStatusProvider;
import uno.anahata.ai.context.system.ContextSummaryProvider;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.RagMessage;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.resource.AbstractResource;

/**
 * The definitive manager for a chat session's context in the V2/V3
 * architecture. This class owns the conversation history and orchestrates the
 * hybrid context assembly process, combining the V3 dynamic history with the V1
 * provider model.
 *
 * @author anahata-ai
 */
@Slf4j
@Getter
public class ContextManager {

    private final Chat chat;
    private final List<AbstractMessage> history = Collections.synchronizedList(new ArrayList<>());
    private final AtomicLong messageIdCounter = new AtomicLong(0);
    private final AtomicLong partIdCounter = new AtomicLong(0);
    private final List<AbstractSystemInstructionsProvider> providers = new ArrayList<>();

    /**
     * The maximum number of tokens for the context window.
     */
    @Setter
    private int contextWindowSize;

    public ContextManager(@NonNull Chat chat) {
        this.chat = chat;
    }

    public void init() {
        // Register default providers
        registerProvider(new ContextSummaryProvider(chat));
        registerProvider(new ChatStatusProvider(chat));
    }

    public void registerProvider(AbstractSystemInstructionsProvider provider) {
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
        for (AbstractSystemInstructionsProvider provider : providers) {
            if (provider.isEnabled()) {
                try {
                    allSystemInstructions.addAll(provider.getSystemInstructions());
                } catch (Exception e) {
                    log.error("Error executing system instruction provider: {}", provider.getName(), e);
                }
            }
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

        // 1. Get the filtered main history
        List<AbstractMessage> visibleHistory = history.stream()
                .filter(msg -> includePruned || !msg.isEffectivelyPruned())
                .collect(Collectors.toList());

        RagMessage augmentedMessage = new RagMessage(chat);

        // Process managed resources for prompt augmentation
        for (AbstractResource resource : chat.getResourceManager().getResources()) {
            try {
                if (resource.getContextPosition() == ContextPosition.PROMPT_AUGMENTATION) {
                    // Delegate rendering to the resource itself
                    resource.populate(augmentedMessage);
                }
            } catch (Exception e) {
                log.error("Error processing managed resource {} for prompt augmentation", resource.getName(), e);
            }
        }

        return visibleHistory;
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
        if (message instanceof AbstractModelMessage) {
            AbstractModelMessage amm = (AbstractModelMessage) message;
            if (!amm.getToolMessage().getParts().isEmpty()) {
                addMessageInternal(amm.getToolMessage());
            }
        }

        hardPrune();
    }

    /**
     * adds a message but without hard prunning
     *
     * @param message
     */
    private void addMessageInternal(AbstractMessage message) {
        message.setSequentialId(this.messageIdCounter.incrementAndGet());
        for (AbstractPart part : message.getParts()) {
            part.setSequentialId(partIdCounter.incrementAndGet());
        }
        history.add(message);
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
            // Remove empty messages, but NEVER remove an explicitly pinned message (pruned=false)
            history.removeIf(msg -> msg.getParts(true).isEmpty() && !Boolean.FALSE.equals(msg.isPruned()));
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
}
