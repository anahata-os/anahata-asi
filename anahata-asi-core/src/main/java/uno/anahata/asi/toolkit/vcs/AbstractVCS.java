/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.vcs;

import java.io.File;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.vcs.FastForwardPolicy;
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

    /**
     * Initializes a new Git repository in the specified directory.
     *
     * @param directoryPath The absolute path of the directory to initialize. If omitted or null, defaults to project root.
     * @return Confirmation message of repository initialization.
     * @throws Exception if initialization fails.
     */
    public abstract String gitInit(String directoryPath) throws Exception;

    /**
     * Retrieves the Git status for a repository or specific file/directory path.
     *
     * @param path The path of the repository root, directory, or file to check. If omitted, checks active project repository.
     * @return Formatted Markdown table containing the branch and status of modified/added/untracked files.
     * @throws Exception if status retrieval fails.
     */
    public abstract String gitStatus(String path) throws Exception;

    /**
     * Stages one or more files into the Git index.
     *
     * @param filePaths List of file paths to stage into the Git index.
     * @return Confirmation message of staged files.
     * @throws Exception if staging fails.
     */
    public abstract String gitAdd(List<String> filePaths) throws Exception;

    /**
     * Commits changes headlessly in a single shot (auto-stages files if specified).
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param filePaths Optional list of specific files to stage and commit. If omitted, commits all staged files.
     * @param message The commit message.
     * @param authorName Optional author name.
     * @param authorEmail Optional author email.
     * @return Details of the created commit revision.
     * @throws Exception if commit fails.
     */
    public abstract String gitCommit(String repoPath, List<String> filePaths, String message, String authorName, String authorEmail) throws Exception;

    /**
     * Opens the native IDE Git Commit UI (tool window or dialog) on the Swing EDT with pre-filled message and selected files.
     *
     * @param repoPath Path of the repository or project directory.
     * @param filePaths Optional list of files to pre-select in the commit UI.
     * @param message The initial commit message to pre-fill.
     * @param authorName Optional author name.
     * @param authorEmail Optional author email.
     * @return Confirmation message of dialog/tool window dispatch.
     * @throws Exception if dispatch fails.
     */
    public abstract String gitOpenCommitDialog(String repoPath, List<String> filePaths, String message, String authorName, String authorEmail) throws Exception;

    /**
     * Pushes committed revisions to a remote Git repository.
     *
     * @param repoPath Path of the repository or project directory.
     * @param remote The remote name (e.g. 'origin', 'helder'). If omitted, defaults to tracked remote or 'origin'.
     * @param branch Optional branch name to push. If omitted, pushes current active branch.
     * @param force Whether to force-push (allow non-fast-forward updates). Defaults to false.
     * @return Confirmation message of push result.
     * @throws Exception if push fails.
     */
    public abstract String gitPush(String repoPath, String remote, String branch, Boolean force) throws Exception;

    /**
     * Pulls changes from a remote Git repository into the current active branch (fetch and merge).
     *
     * @param repoPath Path of the repository or project directory.
     * @param remote The remote name (e.g. 'origin', 'helder'). Defaults to tracked remote or 'origin'.
     * @return Markdown summary of pull updates and merge outcome.
     * @throws Exception if pull fails.
     */
    public abstract String gitPull(String repoPath, String remote) throws Exception;

    /**
     * Fetches updates from a remote Git repository into local tracking branches without modifying working files.
     *
     * @param repoPath Path of the repository or project directory.
     * @param remote The remote name (e.g. 'origin', 'helder'). Defaults to tracked remote or 'origin'.
     * @return Markdown summary of fetched updates.
     * @throws Exception if fetch fails.
     */
    public abstract String gitFetch(String repoPath, String remote) throws Exception;

    /**
     * Lists local and optionally remote branches in a Git repository.
     *
     * @param repoPath Path of the repository or project directory.
     * @param includeRemote Whether to include remote tracking branches. Defaults to true.
     * @return Markdown table of all branches, their active state, and commit details.
     * @throws Exception if listing branches fails.
     */
    public abstract String gitBranches(String repoPath, Boolean includeRemote) throws Exception;

    /**
     * Checks out a Git branch or revision, optionally creating the branch if missing.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchOrRevision The branch name or revision hash to checkout.
     * @param createIfMissing Whether to create the branch if it does not exist. Defaults to false.
     * @return Confirmation message of the checkout operation.
     * @throws Exception if checkout fails.
     */
    public abstract String gitCheckout(String repoPath, String branchOrRevision, Boolean createIfMissing) throws Exception;

    /**
     * Creates a new Git branch at a specified revision or current HEAD.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchName The name of the new branch to create.
     * @param startRevision Optional starting revision hash or branch name. Defaults to 'HEAD'.
     * @return Confirmation message of branch creation.
     * @throws Exception if branch creation fails.
     */
    public abstract String gitCreateBranch(String repoPath, String branchName, String startRevision) throws Exception;

    /**
     * Deletes a local Git branch.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchName The name of the branch to delete.
     * @param force Whether to force delete unmerged commits. Defaults to false.
     * @return Confirmation message of branch deletion.
     * @throws Exception if branch deletion fails.
     */
    public abstract String gitDeleteBranch(String repoPath, String branchName, Boolean force) throws Exception;

    /**
     * Merges a branch or revision into the current active branch.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchOrRevision The branch name or revision hash to merge into the active branch.
     * @param fastForwardPolicy Fast-forward merge policy. Defaults to FAST_FORWARD.
     * @return Markdown summary of the merge outcome.
     * @throws Exception if merge fails.
     */
    public abstract String gitMerge(String repoPath, String branchOrRevision, FastForwardPolicy fastForwardPolicy) throws Exception;

    /**
     * Retrieves the commit history log for a Git repository, branch, or revision.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchOrRevision Optional branch name or commit hash. If omitted, uses active HEAD.
     * @param maxEntries Maximum number of commits to retrieve. Defaults to 10.
     * @param filePath Optional file or directory path to limit the commit log to.
     * @param author Optional author name or email filter.
     * @param noMerges Whether to exclude merge commits. Defaults to false.
     * @param grep Optional regex or keyword filter for commit messages.
     * @return Markdown table of commit revisions, authors, dates, and messages.
     * @throws Exception if retrieving log fails.
     */
    public abstract String gitLog(String repoPath, String branchOrRevision, Integer maxEntries, String filePath, String author, Boolean noMerges, String grep) throws Exception;

    /**
     * Generates a Git diff between two branches or revisions, optionally filtered to a specific file.
     *
     * @param repoPath Path of the repository or project directory.
     * @param baseRevision The base branch or revision hash (e.g. 'main', 'HEAD~1').
     * @param targetRevision The target branch or revision hash (e.g. 'helder/main', 'HEAD').
     * @param filePath Optional specific file or folder path to limit the diff to.
     * @param summaryOnly If true, returns only the list of modified/added/deleted file paths instead of the full patch text. Defaults to false.
     * @return Unified diff output as text or summary.
     * @throws Exception if diff generation fails.
     */
    public abstract String gitDiff(String repoPath, String baseRevision, String targetRevision, String filePath, Boolean summaryOnly) throws Exception;

    /**
     * Reads and returns the content of a file from a specific Git branch or revision without switching branches.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchOrRevision The branch name or revision hash (e.g. 'main', 'HEAD~2').
     * @param filePath The relative or absolute file path to read.
     * @return The raw text content of the file at that revision.
     * @throws Exception if reading the file fails.
     */
    public abstract String gitShow(String repoPath, String branchOrRevision, String filePath) throws Exception;

    /**
     * Retrieves line-by-line authorship and commit history (Git Blame) for a file.
     *
     * @param filePath The absolute path of the file to inspect.
     * @param startLine Optional 1-based starting line number. Defaults to 1.
     * @param endLine Optional 1-based ending line number. If omitted, blames to end of file.
     * @param revision Optional revision to blame against. Defaults to HEAD.
     * @return Formatted Markdown table containing line numbers, commit hashes, authors, dates, and line contents.
     * @throws Exception if blame retrieval fails.
     */
    public abstract String gitBlame(String filePath, Integer startLine, Integer endLine, String revision) throws Exception;

    /**
     * Checks whether a revision string represents the local uncommitted working tree or index.
     * Treats {@code null}, blank, and aliases such as {@code "WORKING_COPY"} as referring to the active working copy.
     *
     * @param revision The revision string to inspect.
     * @return {@code true} if the revision refers to the active working copy or index.
     */
    public static boolean isWorkingCopyAlias(String revision) {
        if (revision == null || revision.isBlank()) {
            return true;
        }
        String rev = revision.trim();
        return rev.equalsIgnoreCase("WORKING_COPY")
                || rev.equalsIgnoreCase("WORKING_TREE")
                || rev.equalsIgnoreCase("WORKDIR")
                || rev.equalsIgnoreCase("WORK_DIR")
                || rev.equalsIgnoreCase("UNCOMMITTED")
                || rev.equalsIgnoreCase("LOCAL")
                || rev.equalsIgnoreCase("CURRENT");
    }

}
