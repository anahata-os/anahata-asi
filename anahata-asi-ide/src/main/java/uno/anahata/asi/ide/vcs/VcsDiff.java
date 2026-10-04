/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.vcs;

import java.io.File;
import java.io.Serializable;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

/**
 * Strongly-typed DTO representing the unified diff comparison of a file
 * against a repository revision or working copy.
 *
 * @author anahata
 */
@Value
@Builder
@Schema(description = "Strongly-typed DTO representing the unified diff comparison of a file against a repository revision or working copy.")
public class VcsDiff implements Serializable {

    /** The path of the file being compared. */
    @NonNull
    @Schema(description = "The absolute path of the file being compared.")
    String filePath;

    /** The VCS status classification of the file. */
    @NonNull
    @Schema(description = "The VCS status classification of the file.")
    VcsFileStatus status;

    /** The base revision identifier (e.g. commit hash, HEAD, or pristine base). */
    @Schema(description = "The base revision identifier (e.g. commit hash, HEAD, or pristine base).")
    String baseRevision;

    /** The target revision identifier (e.g. WORKING_COPY or target commit hash). */
    @Schema(description = "The target revision identifier (e.g. WORKING_COPY or target commit hash).")
    String targetRevision;

    /** The unified diff text, or null if clean, untracked, or binary. */
    @Schema(description = "The unified diff text, or null if clean, untracked, or binary.")
    String diff;

    /**
     * Checks if this diff contains meaningful textual modifications.
     *
     * @return true if the diff text is non-null and not blank.
     */
    public boolean hasChanges() {
        return diff != null && !diff.isBlank();
    }

    /**
     * Checks if this file is newly created without a prior repository base.
     *
     * @return true if newly created or untracked.
     */
    public boolean isNewFile() {
        return status == VcsFileStatus.NEW || status == VcsFileStatus.UNTRACKED;
    }

    /**
     * Calculates the number of inserted lines in the diff.
     *
     * @return count of inserted lines.
     */
    public int getLinesAdded() {
        return (diff == null) ? 0 : (int) diff.lines().filter(l -> l.startsWith("+") && !l.startsWith("+++")).count();
    }

    /**
     * Calculates the number of deleted lines in the diff.
     *
     * @return count of deleted lines.
     */
    public int getLinesDeleted() {
        return (diff == null) ? 0 : (int) diff.lines().filter(l -> l.startsWith("-") && !l.startsWith("---")).count();
    }

    /**
     * Formats this diff as a clean Markdown section for the prompt.
     *
     * @return Formatted Markdown string describing the status and any modifications.
     */
    public String toMarkdown() {
        String fileName = new File(filePath).getName();
        StringBuilder sb = new StringBuilder();

        if (status == VcsFileStatus.CONFLICTED) {
            sb.append("### ⚠️ Merge Conflicts (`").append(fileName).append("`):\n");
            sb.append("- **Status**: `CONFLICTED`\n");
            if (baseRevision != null) {
                sb.append("- **Base Revision**: `").append(baseRevision).append("`\n");
            }
            sb.append("- **Action Required**: Resolve conflict markers (`<<<<<<<` / `=======` / `>>>>>>>`), stage with `gitAdd`, and commit with `gitCommit`.\n\n");
            if (hasChanges()) {
                sb.append("```diff\n").append(diff.trim()).append("\n```");
            }
            return sb.toString().trim();
        }

        if (isNewFile()) {
            sb.append("### VCS Status (`").append(fileName).append("`):\n");
            sb.append("- **Status**: `").append(status).append("` (New file, not yet committed to repository)");
            return sb.toString().trim();
        }

        if (status == VcsFileStatus.CLEAN) {
            sb.append("### VCS Status (`").append(fileName).append("`):\n");
            sb.append("- **Status**: `CLEAN`\n");
            if (baseRevision != null) {
                sb.append("- **Base Revision**: `").append(baseRevision).append("`\n");
            }
            sb.append("- Working copy is clean and matches the repository base.");
            return sb.toString().trim();
        }

        if (status == VcsFileStatus.DELETED) {
            sb.append("### VCS Status (`").append(fileName).append("`):\n");
            sb.append("- **Status**: `DELETED` (File deleted in working tree)");
            return sb.toString().trim();
        }

        if (status == VcsFileStatus.UNSUPPORTED) {
            sb.append("### VCS Status (`").append(fileName).append("`):\n");
            sb.append("- **Status**: `UNSUPPORTED` (File is not managed by version control)");
            return sb.toString().trim();
        }

        if (hasChanges()) {
            sb.append("### Diff to Base (`").append(fileName).append("`):\n");
            sb.append("- **Status**: `").append(status).append("` (+").append(getLinesAdded()).append(" / -").append(getLinesDeleted()).append(" lines)\n");
            if (baseRevision != null) {
                sb.append("- **Base Revision**: `").append(baseRevision).append("`\n");
            }
            if (targetRevision != null && !targetRevision.isBlank()) {
                sb.append("- **Target**: `").append(targetRevision).append("`\n");
            }
            sb.append("\n```diff\n").append(diff.trim()).append("\n```");
            return sb.toString().trim();
        }

        sb.append("### VCS Status (`").append(fileName).append("`):\n- **Status**: `").append(status).append("`");
        return sb.toString().trim();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Returns the formatted Markdown representation of this diff,
     * or a fallback status summary when clean.
     * </p>
     */
    @Override
    public String toString() {
        String md = toMarkdown();
        return md != null ? md : "Status: " + status + " (" + filePath + ")";
    }
}
