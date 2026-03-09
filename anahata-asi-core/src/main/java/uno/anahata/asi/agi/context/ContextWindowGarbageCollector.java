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
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.spi.AbstractTool;

/**
 * Orchestrates the monitoring and logging of the Context Window Garbage Collection (CwGC).
 * This class calculates high-fidelity metrics for prompt load and recycling efficiency
 * using a one-pass calculation strategy.
 */
@Slf4j
@Getter
public class ContextWindowGarbageCollector extends BasicPropertyChangeSource {

    private final ContextManager contextManager;
    private final List<GarbageCollectorRecord> logRecords = new CopyOnWriteArrayList<>();
    
    /** The results of the last token calculation pass. */
    private Stats stats = Stats.builder().build();

    public ContextWindowGarbageCollector(@NonNull ContextManager contextManager) {
        this.contextManager = contextManager;
    }

    /**
     * Performs a comprehensive, one-pass calculation of all token metrics in the 
     * current context. This categorizes tokens into system instructions, tools, 
     * metadata, and various history states.
     */
    public void calculate() {
        long startTime = System.currentTimeMillis();
        log.info("Calculating high-fidelity token metabolism for session {}", contextManager.getAgi().getShortId());
        Stats.StatsBuilder sb = Stats.builder();

        // 1. System Instructions
        List<String> instructions = contextManager.getSystemInstructions();
        sb.systemInstructionsTokens(TokenizerUtils.countTokens(String.join("\n", instructions)));

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

        for (AbstractMessage msg : contextManager.getHistory()) {
            if (msg.shouldCreateMetadata()) {
                metadata += TokenizerUtils.countTokens(msg.createMetadataHeader());
            }
            for (AbstractPart part : msg.getParts()) {
                metadata += part.getMetadataTokenCount();
                if (part.isEffectivelyPruned()) {
                    prunedHistory += part.getTokenCount();
                } else {
                    activeHistory += part.getTokenCount();
                }
            }
        }
        sb.metadataTokens(metadata);
        sb.activeHistoryTokens(activeHistory);
        sb.prunedHistoryTokens(prunedHistory);

        // 4. RAG Message Pass - High Fidelity
        RagMessage ragMessage = contextManager.buildRagMessage();
        sb.ragTokens(ragMessage.getTokenCount(true));
        
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
        private final int systemInstructionsTokens;
        private final int toolDeclarationsTokens;
        private final int metadataTokens;
        private final int activeHistoryTokens;
        private final int prunedHistoryTokens;
        private final int ragTokens;
        private final int garbageCollectedTokens;

        /**
         * Calculates the total prompt load (tokens sent to the model).
         * @return The total active tokens.
         */
        public int getTotalPromptLoad() {
            return systemInstructionsTokens + toolDeclarationsTokens + metadataTokens + activeHistoryTokens + ragTokens;
        }
    }
}
