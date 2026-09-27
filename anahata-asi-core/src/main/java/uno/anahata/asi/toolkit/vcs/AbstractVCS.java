/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.vcs;

import java.io.File;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.vcs.HistoryEntry;
import uno.anahata.asi.agi.resource.vcs.VcsDiff;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * Universal base abstraction for Version Control System (VCS) and Local History toolkits.
 * <p>
 * Standardizes core method signatures, parameter names, and contracts across host environments
 * (NetBeans, IntelliJ, Eclipse, CLI). Host-specific subclasses implement the abstract operations
 * using the underlying IDE platform APIs and declare the appropriate tool annotations.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public abstract class AbstractVCS extends AnahataToolkit {

    /**
     * Gets the Version Control metadata and repository root for a file or directory.
     *
     * @param path The absolute path of the file or directory to inspect.
     * @return Formatted Markdown summary of VCS metadata.
     * @throws Exception if resolution fails.
     */
    public abstract String getInfo(String path) throws Exception;

    /**
     * Generates a unified diff for a file against its repository base or a specific revision.
     *
     * @param filePath The absolute path of the file to inspect.
     * @param revision Optional revision identifier. If omitted, diffs against repository base.
     * @return A {@link VcsDiff} DTO containing diff text and status classification.
     * @throws Exception if diff generation fails.
     */
    public abstract VcsDiff getDiff(String filePath, String revision) throws Exception;

    /**
     * Queries the unified chronological history of a file, combining VCS commits and IDE Local History.
     *
     * @param filePath The absolute path of the file.
     * @param maxEntries Maximum number of history entries to return. Defaults to 10.
     * @return A list of {@link HistoryEntry} DTOs sorted in reverse chronological order.
     * @throws Exception if querying history fails.
     */
    public abstract List<HistoryEntry> getHistory(String filePath, Integer maxEntries) throws Exception;

    /**
     * Discards unstaged modifications in a file, reverting it to the repository pristine base revision.
     *
     * @param filePath The absolute path of the file to revert.
     * @return Confirmation message of the revert operation.
     * @throws Exception if revert fails.
     */
    public abstract String revert(String filePath) throws Exception;


    /**
     * Checks if a given directory path is the root of a repository.
     *
     * @param path The directory path to check.
     * @return true if the directory is a repository root.
     */
    public abstract boolean isRepoRoot(String path);

    /**
     * Builds a structured Markdown overview of a repository including branch, tracking,
     * remotes, working tree status, and recent commits.
     *
     * @param repoPath Path of the repository or project directory.
     * @return Structured Markdown overview of the repository state, or null if unmanaged.
     * @throws Exception if repository querying fails.
     */
    public abstract String getRepositoryOverview(String repoPath) throws Exception;

}
