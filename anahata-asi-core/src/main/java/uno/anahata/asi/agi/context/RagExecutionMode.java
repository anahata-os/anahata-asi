/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.context;

import java.io.Serializable;

/**
 * Enumerates the execution strategies available for assembling the Retrieval-Augmented Generation (RAG) message.
 * <p>
 * This configuration dictates whether registered {@link ContextProvider} instances are queried concurrently
 * across available CPU cores or sequentially on a single thread. Regardless of execution mode, prompt content
 * is assembled in strict, deterministic hierarchical order to ensure prompt cache (KV cache) stability.
 * </p>
 *
 * @author Anahata
 */
public enum RagExecutionMode implements Serializable {

    /**
     * Dispatches context providers concurrently across the session's background executor,
     * merging their outputs in strict canonical document order once all tasks complete.
     */
    PARALLEL("Parallel (Multi-Threaded)"),

    /**
     * Executes each context provider sequentially on the main turn orchestration thread.
     */
    SEQUENTIAL("Sequential (Single-Threaded)");

    /**
     * Human-readable display label.
     */
    private final String displayName;

    /**
     * Constructs a RagExecutionMode with its display label.
     *
     * @param displayName The human-readable name for UI and telemetry.
     */
    RagExecutionMode(String displayName) {
        this.displayName = displayName;
    }

    /**
     * Gets the human-readable display label.
     *
     * @return The display name string.
     */
    public String getDisplayName() {
        return displayName;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the human-readable display label for Swing UI rendering.
     * </p>
     */
    @Override
    public String toString() {
        return displayName;
    }
}
