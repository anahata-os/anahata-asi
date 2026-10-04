/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.vcs;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Fast-forward merge policy for Git branch integration and pull operations.
 *
 * @author anahata
 */
@Schema(description = "Fast-forward merge policy for Git branch integration and pull operations.")
public enum FastForwardPolicy {

    /** Standard merge: fast-forwards if possible, creates a merge commit otherwise. */
    @Schema(description = "Standard merge: fast-forwards if possible, creates a merge commit otherwise.")
    FAST_FORWARD,

    /** Only allow fast-forward merge. Fails if a merge commit would be required. */
    @Schema(description = "Only allow fast-forward merge. Fails if a merge commit would be required.")
    FAST_FORWARD_ONLY,

    /** Always create a merge commit even if fast-forward is possible. */
    @Schema(description = "Always create a merge commit even if fast-forward is possible.")
    NO_FAST_FORWARD
}
