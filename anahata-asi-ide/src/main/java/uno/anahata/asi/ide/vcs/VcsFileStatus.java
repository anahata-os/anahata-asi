/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.vcs;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * Version control status classification for an individual file or resource.
 *
 * @author anahata
 */
@Getter
@Schema(description = "Version control status classification for an individual file or resource.")
public enum VcsFileStatus {

    /** The file is clean and matches the repository base without uncommitted modifications. */
    @Schema(description = "The file is clean and matches the repository base without uncommitted modifications.")
    CLEAN("Clean (matches repository base)"),

    /** The file has uncommitted local modifications against the repository base. */
    @Schema(description = "The file has uncommitted local modifications against the repository base.")
    MODIFIED("Modified in working copy"),

    /** The file is newly created and tracked (staged) in version control. */
    @Schema(description = "The file is newly created and tracked (staged) in version control.")
    NEW("New file (staged in index)"),

    /** The file exists in the working tree but is not tracked by version control. */
    @Schema(description = "The file exists in the working tree but is not tracked by version control.")
    UNTRACKED("Untracked (new file)"),

    /** The file has been deleted from the working tree. */
    @Schema(description = "The file has been deleted from the working tree.")
    DELETED("Deleted in working copy"),

    /** The file is in a conflicted state following an unresolved merge or rebase. */
    @Schema(description = "The file is in a conflicted state following an unresolved merge or rebase.")
    CONFLICTED("Conflicted (unresolved merge conflicts)"),

    /** The file resides outside a version-controlled repository or VCS is unsupported. */
    @Schema(description = "The file resides outside a version-controlled repository or VCS is unsupported.")
    UNSUPPORTED("Unsupported (not under version control)");

    /** Human-readable explanation of the VCS status. */
    private final String description;

    /**
     * Constructs a VCS file status constant with its human-readable explanation.
     *
     * @param description The human-readable description of the status.
     */
    VcsFileStatus(String description) {
        this.description = description;
    }
}
