/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

/**
 * Defines the possible pruning and pinning states for messages and parts within 
 * the conversation context.
 * 
 * @author anahata
 */
public enum PruningState {
    /** 
     * The entity is managed automatically based on its remaining depth. 
     * It will be effectively pruned when depth reaches 0.
     */
    AUTO,
    /** 
     * The entity is explicitly pruned (hidden from the AI prompt) regardless of depth. 
     */
    PRUNED,
    /** 
     * The entity is explicitly pinned (kept in the AI prompt) regardless of depth. 
     * It will never be auto-pruned or garbage collected while in this state.
     */
    PINNED
}
