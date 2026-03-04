/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.core;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Defines the possible pruning and pinning states for messages and parts within 
 * the conversation context.
 * 
 * @author anahata
 */
@Schema(description = "Represents the pruningState attribute of an individual Part within a Message.")
public enum PruningState {
    /** 
     * The entity is managed automatically based on its remaining depth. 
     * It will be effectively pruned when depth reaches 0.
     */
    @Schema(description = "The part's visibility is managed automatically based on its remaining depth. It will be effectivelyPruned when the remainingDepth reaches 0.")
    AUTO,
    /** 
     * The entity is explicitly pruned (hidden from the AI prompt) regardless of depth. 
     */
    @Schema(description = "The part is explicitly pruned (hidden from the AI prompt) even if the remaining depth > 0. Can be set back to PINED or AUTO for as long as the message doesnt get Garbage Collected.")
    PRUNED,
    /** 
     * The entity is explicitly pinned (kept in the AI prompt) regardless of depth. 
     * It will never be auto-pruned or garbage collected while in this state.
     */
    @Schema(description = "The part's is explicitly pruned. Will remain in the prompt until unpined. The message containing this part will not get Garbage Collected.")
    PINNED
}
