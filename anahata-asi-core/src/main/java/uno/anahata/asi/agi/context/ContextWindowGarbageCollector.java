/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.context;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.toolkit.History;

/**
 * Orchestrates the monitoring and logging of the Context Window Garbage
 * Collection (CwGC). This class calculates high-fidelity metrics for prompt
 * load and recycling efficiency using a one-pass calculation strategy.
 */
@Slf4j
@Getter
public class ContextWindowGarbageCollector extends BasicPropertyChangeSource {

    /**
     * The parent ContextManager orchestrating the active session.
     */
    private final ContextManager contextManager;

    /**
     * The concurrent log records of all garbage collection sweeps.
     */
    private final List<GarbageCollectorRecord> logRecords = new CopyOnWriteArrayList<>();

    /**
     * The results of the last token calculation pass.
     */
    private Stats stats = Stats.builder().build();

    /**
     * Constructs a ContextWindowGarbageCollector with its parent manager.
     *
     * @param contextManager The parent ContextManager instance.
     */
    public ContextWindowGarbageCollector(@NonNull ContextManager contextManager) {
        this.contextManager = contextManager;
    }

    /**
     * Performs a comprehensive, one-pass calculation of all token metrics in
     * the current context. This categorizes tokens into system instructions,
     * tools, metadata, and various history states using the active session
     * tokenizer.
     */
    public void calculate() {
        long startTime = System.currentTimeMillis();
        log.debug("Calculating high-fidelity token metabolism for session {}", contextManager.getAgi().getShortId());
        Stats.StatsBuilder sb = Stats.builder();

        AbstractModel model = contextManager.getAgi().getSelectedModel();
        if (model == null) {
            log.info("No model selected for session {}. Setting token metabolism to 0.", contextManager.getAgi().getShortId());
            Stats oldStats = this.stats;
            this.stats = sb.build();
            propertyChangeSupport.firePropertyChange("stats", oldStats, this.stats);
            return;
        }

        // 1. System Instructions
        List<String> instructions = contextManager.getSystemInstructions();
        sb.systemInstructionsTokens(model.countTokens(String.join("\n", instructions)));

        // 2. Tool Declarations
        if (contextManager.getAgi().getConfig().isLocalToolsEnabled()) {
            int toolTokens = contextManager.getAgi().getToolManager().getEnabledTools().stream()
                    .mapToInt(AbstractTool::getTokenCount)
                    .sum();
            sb.toolDeclarationsTokens(toolTokens);
        }

        // 3. History Pass (Active, Pruned, and Metadata)
        int metadata = 0;
        int activeHistory = 0;
        int prunedHistory = 0;

        boolean injectInband = contextManager.getAgi().getRequestConfig().isInjectInbandMetadata();

        for (AbstractMessage msg : contextManager.getHistory()) {
            if (injectInband && msg.shouldCreateMetadata()) {
                metadata += model.countTokens(msg.createMetadataHeader());
            }
            for (AbstractPart part : msg.getParts()) {
                if (injectInband) {
                    metadata += part.getMetadataTokenCount();
                }

                if (part.isEffectivelyPruned()) {
                    prunedHistory += part.getTokenCount();
                } else {
                    activeHistory += part.getTokenCount();
                }
            }
        }

        // 4. RAG Message Pass - High Fidelity
        RagMessage ragMessage = contextManager.buildRagMessage();
        int totalRagTokens = ragMessage.getTokenCount(true);

        if (!injectInband) {
            // In consolidated mode, calculate the History Metadata block tokens specifically
            History historyToolkit = contextManager.getAgi().getToolkit(History.class).orElse(null);
            if (historyToolkit != null) {
                metadata = model.countTokens(historyToolkit.createConsolidatedIndex());
            }
            sb.ragTokens(totalRagTokens - metadata);
        } else {
            sb.ragTokens(totalRagTokens);
        }

        sb.metadataTokens(metadata);
        sb.activeHistoryTokens(activeHistory);
        sb.prunedHistoryTokens(prunedHistory);

        // 5. Cumulative Garbage Collected (from Logs)
        int totalGarbageCollected = logRecords.stream()
                .mapToInt(GarbageCollectorRecord::getTokenCount)
                .sum();
        sb.garbageCollectedTokens(totalGarbageCollected);

        Stats oldStats = this.stats;
        this.stats = sb.build();
        propertyChangeSupport.firePropertyChange("stats", oldStats, this.stats);

        log.info("Token metabolism calculation for session {} took {}ms", contextManager.getAgi().getShortId(), (System.currentTimeMillis() - startTime));
    }

    /**
     * Generates a compact parts summary string for a message being garbage
     * collected.
     *
     * @param message The message to summarize.
     * @return The formatted summary string.
     */
    private String summarizeParts(AbstractMessage message) {
        java.util.List<String> summaries = new java.util.ArrayList<>();
        for (AbstractPart part : message.getParts(true)) {
            if (part instanceof AbstractToolCall<?, ?> toolCall) {
                String status = toolCall.getResponse() != null && toolCall.getResponse().getStatus() != null
                        ? toolCall.getResponse().getStatus().name()
                        : "UNKNOWN";
                summaries.add(part.getClass().getSimpleName() + "(tool: " + toolCall.getToolName() + ", status: " + status + ")");
            } else if (part instanceof uno.anahata.asi.agi.message.TextPart textPart) {
                if (textPart.isThought()) {
                    summaries.add(part.getClass().getSimpleName() + "(thought)");
                } else {
                    summaries.add(part.getClass().getSimpleName());
                }
            } else if (part instanceof uno.anahata.asi.agi.message.BlobPart blobPart) {
                summaries.add(part.getClass().getSimpleName() + "(mime: " + blobPart.getMimeType() + ")");
            } else {
                summaries.add(part.getClass().getSimpleName());
            }
        }
        return summaries.stream().collect(java.util.stream.Collectors.joining(", ", "[", "]"));
    }

    /**
     * Records the recycling of a message turn.
     *
     * @param message The message being garbage collected.
     */
    public void recordCollection(@NonNull AbstractMessage message) {
        GarbageCollectorRecord record = GarbageCollectorRecord.builder()
                .timestamp(System.currentTimeMillis())
                .messageId(message.getSequentialId())
                .type(message.getClass().getSimpleName())
                .tokenCount(message.getTokenCount(true))
                .partsSummary(summarizeParts(message))
                .build();
        logRecords.add(record);
        propertyChangeSupport.firePropertyChange("log", null, logRecords);
    }

    /**
     * Clears the collection logs.
     */
    public void clearLog() {
        logRecords.clear();
        propertyChangeSupport.firePropertyChange("log", null, logRecords);
    }

    /**
     * Gets an unmodifiable view of the collection log.
     *
     * @return The list of collection records.
     */
    public List<GarbageCollectorRecord> getRecords() {
        return Collections.unmodifiableList(logRecords);
    }

    /**
     * Data object containing the categorized token metrics.
     */
    @Data
    @Builder
    public static class Stats {

        /**
         * The total tokens consumed by system instructions.
         */
        private final int systemInstructionsTokens;
        /**
         * The total tokens consumed by all enabled tool declarations.
         */
        private final int toolDeclarationsTokens;
        /**
         * The total tokens consumed by active message and part metadata.
         */
        private final int metadataTokens;
        /**
         * The total tokens consumed by unpruned history parts.
         */
        private final int activeHistoryTokens;
        /**
         * The total tokens consumed by pruned history parts.
         */
        private final int prunedHistoryTokens;
        /**
         * The total tokens consumed by unpruned RAG message content.
         */
        private final int ragTokens;
        /**
         * The cumulative tokens recycled by the garbage collector.
         */
        private final int garbageCollectedTokens;

        /**
         * Calculates the total prompt load (tokens sent to the model).
         *
         * @return The total active tokens.
         */
        public int getTotalPromptLoad() {
            return systemInstructionsTokens + toolDeclarationsTokens + metadataTokens + activeHistoryTokens + ragTokens;
        }
    }
}
