/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.vcs;

import com.github.difflib.DiffUtils;
import com.github.difflib.UnifiedDiffUtils;
import com.github.difflib.patch.Patch;
import java.awt.Component;
import java.awt.Container;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.swing.JTextArea;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.libs.git.GitBlameResult;
import org.netbeans.libs.git.GitBranch;
import org.netbeans.libs.git.GitLineDetails;
import org.netbeans.libs.git.GitMergeResult;
import org.netbeans.libs.git.GitPullResult;
import org.netbeans.libs.git.GitPushResult;
import org.netbeans.libs.git.GitRemoteConfig;
import org.netbeans.libs.git.GitRevisionInfo;
import org.netbeans.libs.git.GitStatus;
import org.netbeans.libs.git.GitTransportUpdate;
import org.netbeans.libs.git.GitUser;
import org.netbeans.libs.git.SearchCriteria;
import org.netbeans.libs.git.progress.ProgressMonitor;
import org.netbeans.modules.git.Git;
import org.netbeans.modules.git.client.GitClient;
import org.netbeans.modules.git.ui.commit.GitCommitPanel;
import org.netbeans.modules.localhistory.LocalHistory;
import org.netbeans.modules.localhistory.store.LocalHistoryStore;
import org.netbeans.modules.localhistory.store.StoreEntry;
import org.netbeans.modules.versioning.core.api.VCSFileProxy;
import org.netbeans.modules.versioning.spi.VCSHistoryProvider;
import org.netbeans.modules.versioning.spi.VCSHistoryProvider.HistoryEntry;
import org.netbeans.modules.versioning.spi.VersioningSupport;
import org.netbeans.modules.versioning.spi.VersioningSystem;
import org.netbeans.modules.versioning.spi.VCSContext;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import org.netbeans.modules.git.GitFileNode.GitLocalFileNode;
import org.netbeans.modules.versioning.util.common.VCSCommitOptions;
import org.openide.util.HelpCtx;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * Universal Version Control System (VCS) toolkit built exclusively on NetBeans Versioning and Local History APIs.
 * <p>
 * Provides provider-agnostic VCS capabilities across any VCS supported by NetBeans (Git, Subversion, Mercurial)
 * as well as NetBeans Local History, with zero external CLI subprocess execution and zero filesystem hardcoding.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Universal toolkit for NetBeans Versioning Systems and Local History.")
public class VCS extends AnahataToolkit {

    /**
     * Immutable DTO representing a normalized history entry across NetBeans VCS and Local History.
     *
     * @param date The timestamp of the revision.
     * @param revision The short revision identifier (commit hash, revision number, or 'Local').
     * @param user The author or user who made the change.
     * @param message The commit message or Local History label.
     * @param origin The source system (e.g. 'Git', 'Subversion', 'LocalHistory').
     */
    private record UnifiedHistoryEntry(Date date, String revision, String user, String message, String origin) {}

    /**
     * {@inheritDoc}
     * <p>Contributes system instructions describing how to access NetBeans's native GitClient for advanced Git operations.</p>
     */
    @Override
    public List<String> getSystemInstructions() {
        return List.of("""
        ### NetBeans VCS & Git Engine Direct Access:
        For advanced Git operations not exposed as discrete `@AgiTool` methods (such as merge, cherry-pick, rebase, tag, stash, or branch creation), you can use `NbJava.compileAndExecute` to obtain NetBeans's managed `GitClient`:
        ```java
        File repoRoot = new File("/path/to/repo");
        GitClient client = Git.getInstance().getClient(repoRoot);
        try {
            // client.merge(branch, ...);
            // client.rebase(op, branch, ...);
            // client.createBranch(branchName, revision, ...);
            // client.cherryPick(revision, ...);
        } finally {
            client.release();
        }
        ```
        This client is pre-wired with NetBeans's Keyring credentials, SSH session factory, and VFS synchronization.
        """);
    }

    /**
     * Returns versioning metadata for a file or directory using NetBeans VersioningSupport.
     *
     * @param path The absolute path of the file or directory to inspect.
     * @return Markdown summary with the VCS system name, repository root, and managed status.
     * @throws Exception if path resolution fails.
     */
    @AgiTool(value = "Gets the Version Control metadata and repository root for a file or directory.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getInfo(
            @AgiToolParam(value = "The absolute path of the file or directory.", rendererId = "path") String path) throws Exception {

        File file = resolveFile(path);
        VersioningSystem vs = VersioningSupport.getOwner(file);
        if (vs == null) {
            return "Path is not under Version Control: " + path;
        }

        String vcsName = (String) vs.getProperty(VersioningSystem.PROP_DISPLAY_NAME);
        if (vcsName == null || vcsName.isBlank()) {
            vcsName = "VCS";
        }

        File repoRoot = vs.getTopmostManagedAncestor(file);
        StringBuilder sb = new StringBuilder();
        sb.append("### VCS Information: ").append(file.getName()).append("\n\n");
        sb.append("- **VCS System**: ").append(vcsName).append("\n");
        sb.append("- **Repository Root (Topmost Managed Ancestor)**: ").append(repoRoot != null ? repoRoot.getAbsolutePath() : "None").append("\n");
        sb.append("- **File Path**: ").append(file.getAbsolutePath()).append("\n");
        sb.append("- **Is File**: ").append(file.isFile()).append("\n");

        log("Retrieved VCS info for: " + file.getAbsolutePath());
        return sb.toString().trim();
    }

    /**
     * Queries the unified chronological history of a file, combining active VCS commits with NetBeans Local History snapshots.
     *
     * @param filePath The absolute path of the file.
     * @param maxEntries Maximum number of history entries to return. Defaults to 10 if null.
     * @return A Markdown table containing the chronological history.
     * @throws Exception if querying history fails.
     */
    @AgiTool(value = "Queries the unified chronological history of a file, combining VCS commits and NetBeans Local History.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getHistory(
            @AgiToolParam(value = "The absolute path of the file.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Maximum number of history entries to return. Defaults to 10.", required = false) Integer maxEntries) throws Exception {

        File file = resolveFile(filePath);
        int limit = (maxEntries != null && maxEntries > 0) ? maxEntries : 10;
        List<UnifiedHistoryEntry> history = new ArrayList<>();

        // 1. VCS entries via NetBeans VersioningSupport (works for Git, SVN, Mercurial)
        VersioningSystem vs = VersioningSupport.getOwner(file);
        if (vs != null) {
            String vcsName = (String) vs.getProperty(VersioningSystem.PROP_DISPLAY_NAME);
            if (vcsName == null || vcsName.isBlank()) {
                vcsName = "VCS";
            }
            VCSHistoryProvider hp = vs.getVCSHistoryProvider();
            if (hp != null) {
                HistoryEntry[] entries = hp.getHistory(new File[]{file}, null);
                if (entries != null) {
                    for (HistoryEntry ge : entries) {
                        String msg = ge.getMessage() != null ? ge.getMessage().replace("\n", " ").trim() : "";
                        history.add(new UnifiedHistoryEntry(ge.getDateTime(), ge.getRevisionShort(), ge.getUsernameShort(), msg, vcsName));
                    }
                }
            }
        }

        // 2. NetBeans Local History entries via LocalHistoryStore
        LocalHistoryStore store = LocalHistory.getInstance().getLocalHistoryStore();
        if (store != null) {
            VCSFileProxy proxy = VCSFileProxy.createFileProxy(file);
            StoreEntry[] storeEntries = store.getStoreEntries(proxy);
            if (storeEntries != null) {
                for (StoreEntry se : storeEntries) {
                    Date date = new Date(se.getTimestamp());
                    String label = se.getLabel() != null ? se.getLabel().trim() : "";
                    history.add(new UnifiedHistoryEntry(date, "Local", "", label, "LocalHistory"));
                }
            }
        }

        // 3. Sort descending (newest first)
        history.sort((a, b) -> b.date().compareTo(a.date()));

        if (history.isEmpty()) {
            return "No history entries found for: " + filePath;
        }

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        StringBuilder sb = new StringBuilder();
        sb.append("### Unified History for: ").append(file.getName()).append("\n\n");
        sb.append("| Date | Revision | User | Origin | Message |\n");
        sb.append("| :--- | :--- | :--- | :--- | :--- |\n");

        int count = Math.min(limit, history.size());
        for (int i = 0; i < count; i++) {
            UnifiedHistoryEntry e = history.get(i);
            sb.append("| ")
              .append(sdf.format(e.date())).append(" | ")
              .append(e.revision()).append(" | ")
              .append(e.user() != null ? e.user() : "").append(" | ")
              .append(e.origin()).append(" | ")
              .append(e.message() != null ? e.message() : "").append(" |\n");
        }

        log("Found " + history.size() + " total history entries for " + file.getName() + ", showing " + count);
        return sb.toString().trim();
    }

    /**
     * Generates a unified diff for a file against its pristine repository base or against a specific historical revision.
     *
     * @param filePath The absolute path of the file to inspect.
     * @param revision Optional revision identifier (e.g. commit hash or revision number). If omitted, diffs against the repository pristine base.
     * @return The standard unified diff text.
     * @throws Exception if diff generation fails.
     */
    @AgiTool(value = "Generates a unified diff for a file against repository base or a specific revision using NetBeans APIs.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getDiff(
            @AgiToolParam(value = "The absolute path of the file to inspect.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Optional revision identifier (e.g. commit hash or revision number). If omitted, diffs against repository base.", required = false) String revision) throws Exception {

        File file = resolveFile(filePath);
        VersioningSystem vs = VersioningSupport.getOwner(file);
        if (vs == null) {
            throw new AgiToolException("File is not managed by any NetBeans Versioning System: " + filePath);
        }

        File tempBase = File.createTempFile("vcs-base", file.getName());
        tempBase.deleteOnExit();

        if (revision != null && !revision.isBlank()) {
            VCSHistoryProvider hp = vs.getVCSHistoryProvider();
            if (hp == null) {
                throw new AgiToolException("VCS History Provider is not available for " + vs.getProperty(VersioningSystem.PROP_DISPLAY_NAME));
            }
            HistoryEntry[] entries = hp.getHistory(new File[]{file}, null);
            HistoryEntry targetEntry = null;
            if (entries != null) {
                for (HistoryEntry e : entries) {
                    if (e.getRevisionShort() != null && e.getRevisionShort().equalsIgnoreCase(revision.trim())) {
                        targetEntry = e;
                        break;
                    }
                }
            }
            if (targetEntry == null) {
                throw new AgiToolException("Revision '" + revision + "' not found in history for: " + filePath);
            }
            targetEntry.getRevisionFile(file, tempBase);
        } else {
            vs.getOriginalFile(file, tempBase);
        }

        if (!tempBase.exists() || tempBase.length() == 0 && file.length() > 0 && !tempBase.createNewFile()) {
            // Check if getOriginalFile wrote nothing
            log("Base file not created by VersioningSystem: " + tempBase.getAbsolutePath());
        }

        List<String> originalLines = Files.readAllLines(tempBase.toPath(), StandardCharsets.UTF_8);
        List<String> revisedLines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8);

        Patch<String> patch = DiffUtils.diff(originalLines, revisedLines);
        List<String> unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(file.getName(), file.getName(), originalLines, patch, 3);

        if (unifiedDiff.isEmpty()) {
            log("File is identical to base revision: " + filePath);
            return "No differences found for: " + filePath + " against " + (revision != null ? revision : "repository base");
        }

        log("Generated unified diff (" + unifiedDiff.size() + " lines) for: " + file.getName());
        return String.join("\n", unifiedDiff);
    }

    /**
     * Discards unstaged modifications in a file, restoring it to the repository's pristine base revision.
     *
     * @param filePath The absolute path of the file to revert.
     * @return Confirmation message of the revert operation.
     * @throws Exception if revert fails.
     */
    @AgiTool("Discards unstaged modifications in a file, reverting it to the repository pristine base revision.")
    public String revert(
            @AgiToolParam(value = "The absolute path of the file to revert.", rendererId = "path") String filePath) throws Exception {

        File file = resolveFile(filePath);
        VersioningSystem vs = VersioningSupport.getOwner(file);
        if (vs == null) {
            throw new AgiToolException("File is not managed by any NetBeans Versioning System: " + filePath);
        }

        File tempBase = File.createTempFile("vcs-revert", file.getName());
        tempBase.deleteOnExit();

        vs.getOriginalFile(file, tempBase);
        if (!tempBase.exists() || tempBase.length() == 0 && file.length() > 0) {
            throw new AgiToolException("Could not obtain pristine base file from " + vs.getProperty(VersioningSystem.PROP_DISPLAY_NAME) + " for " + filePath);
        }

        Files.copy(tempBase.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING);

        // Refresh NetBeans VFS so editor buffers and filesystem caches immediately sync
        FileObject fo = FileUtil.toFileObject(file);
        if (fo != null) {
            fo.refresh();
        }

        log("Successfully reverted " + file.getName() + " to repository base.");
        return "Successfully reverted " + file.getName() + " to pristine repository base.";
    }

    /**
     * Initializes a new Git repository in the specified directory and refreshes NetBeans VFS.
     *
     * @param directoryPath The absolute path of the directory to initialize as a Git repository.
     * @return Confirmation message of repository initialization.
     * @throws Exception if initialization fails.
     */
    @AgiTool("Initializes a new Git repository in the specified directory.")
    public String gitInit(
            @AgiToolParam(value = "The absolute path of the directory to initialize.", rendererId = "path") String directoryPath) throws Exception {

        File dir = resolveDirectory(directoryPath);
        GitClient client = Git.getInstance().getClient(dir);
        try {
            client.init(new ToolProgressMonitor());
        } finally {
            client.release();
        }

        FileObject fo = FileUtil.toFileObject(dir);
        if (fo != null) {
            fo.refresh();
        }

        log("Initialized Git repository in: " + dir.getAbsolutePath());
        return "Successfully initialized Git repository in: " + dir.getAbsolutePath();
    }

    /**
     * Retrieves the Git status for a repository or specific file/directory path.
     *
     * @param path The path of the repository root, directory, or file to inspect.
     * @return Formatted Markdown table containing the branch and status of modified/added/untracked files.
     * @throws Exception if status retrieval fails.
     */
    @AgiTool(value = "Gets the Git status for a repository or file.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitStatus(
            @AgiToolParam(value = "The path of the repository, directory, or file to check.", rendererId = "path") String path) throws Exception {

        File target = resolveFileOrDirectory(path);
        File repoRoot = requireRepoRoot(path);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitBranch active = getActiveBranch(client, monitor);
            String activeBranch = active != null ? active.getName() : "HEAD";

            File[] roots = target.equals(repoRoot) ? new File[]{repoRoot} : new File[]{target};
            Map<File, GitStatus> statusMap = client.getStatus(roots, monitor);

            StringBuilder sb = new StringBuilder();
            sb.append("### Git Status: ").append(repoRoot.getName()).append(" [Branch: `").append(activeBranch).append("`]\n\n");

            int modifiedCount = 0;
            StringBuilder table = new StringBuilder();
            table.append("| Status (Index / Working Tree) | File |\n");
            table.append("| :--- | :--- |\n");

            for (Map.Entry<File, GitStatus> entry : statusMap.entrySet()) {
                GitStatus s = entry.getValue();
                GitStatus.Status headWc = s.getStatusHeadWC();
                GitStatus.Status indexWc = s.getStatusIndexWC();

                if (headWc == GitStatus.Status.STATUS_IGNORED || indexWc == GitStatus.Status.STATUS_IGNORED) {
                    continue;
                }
                if (headWc == GitStatus.Status.STATUS_NORMAL && indexWc == GitStatus.Status.STATUS_NORMAL) {
                    continue;
                }

                String relativePath = repoRoot.toPath().relativize(entry.getKey().toPath()).toString();
                table.append("| `").append(indexWc).append(" / ").append(headWc).append("` | `").append(relativePath).append("` |\n");
                modifiedCount++;
            }

            if (modifiedCount == 0) {
                sb.append("Working tree clean. No modified, added, or untracked files.\n");
            } else {
                sb.append("- **Total Modified/Untracked Files**: ").append(modifiedCount).append("\n\n");
                sb.append(table);
            }

            log("Retrieved Git status for " + target.getName() + " (" + modifiedCount + " modified files)");
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Stages one or more files into the Git index.
     *
     * @param filePaths List of file paths to stage into the Git index.
     * @return Confirmation message of staged files.
     * @throws Exception if staging fails.
     */
    @AgiTool("Stages one or more files into the Git index.")
    public String gitAdd(
            @AgiToolParam(value = "List of file paths to stage into the Git index.") List<String> filePaths) throws Exception {

        if (filePaths == null || filePaths.isEmpty()) {
            throw new AgiToolException("No files specified to stage.");
        }

        List<File> files = new ArrayList<>();
        File repoRoot = null;
        for (String p : filePaths) {
            File f = resolveFile(p);
            files.add(f);
            if (repoRoot == null) {
                repoRoot = findRepoRoot(f);
            }
        }

        if (repoRoot == null) {
            throw new AgiToolException("Files are not inside a Git repository.");
        }

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            client.add(files.toArray(File[]::new), monitor);
        } finally {
            client.release();
        }

        for (File f : files) {
            FileObject fo = FileUtil.toFileObject(f);
            if (fo != null) {
                fo.refresh();
            }
        }

        log("Staged " + files.size() + " files into Git index.");
        return "Successfully staged " + files.size() + " file(s) into Git index.";
    }

    /**
     * Commits changes headlessly in a single shot (automatically staging files if specified) using NetBeans GitClient.
     *
     * @param repoPath Path of the repository or any contained file.
     * @param filePaths Optional list of specific files to stage and commit. If omitted, commits all staged files in repository.
     * @param message The commit message.
     * @param authorName Optional author name. If omitted, uses repository gitconfig or system user.
     * @param authorEmail Optional author email. If omitted, uses repository gitconfig.
     * @return Details of the created commit revision.
     * @throws Exception if commit fails.
     */
    @AgiTool("Commits changes headlessly in a single shot (auto-stages files if specified).")
    public String gitCommit(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "Optional list of specific files to stage and commit. If omitted, commits all staged files.", required = false) List<String> filePaths,
            @AgiToolParam(value = "The commit message.") String message,
            @AgiToolParam(value = "Optional author name.", required = false) String authorName,
            @AgiToolParam(value = "Optional author email.", required = false) String authorEmail) throws Exception {

        if (message == null || message.isBlank()) {
            throw new AgiToolException("Commit message cannot be empty.");
        }

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitUser user = resolveGitUser(client, authorName, authorEmail);

            File[] filesToCommit;
            if (filePaths != null && !filePaths.isEmpty()) {
                List<File> list = new ArrayList<>();
                for (String p : filePaths) {
                    list.add(resolveFile(p));
                }
                filesToCommit = list.toArray(File[]::new);
                client.add(filesToCommit, monitor);
            } else {
                filesToCommit = new File[]{repoRoot};
            }

            GitRevisionInfo info = client.commit(filesToCommit, message.trim(), user, user, monitor);
            refreshVfs(repoRoot);

            StringBuilder sb = new StringBuilder();
            sb.append("### Git Commit Successful\n\n");
            sb.append("- **Revision**: `").append(info.getRevision()).append("`\n");
            sb.append("- **Author**: ").append(info.getAuthor().getName()).append(" <").append(info.getAuthor().getEmailAddress()).append(">\n");
            sb.append("- **Message**: ").append(info.getShortMessage()).append("\n");

            log("Committed revision " + info.getRevision() + " in " + repoRoot.getName());
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Opens the native NetBeans Git Commit dialog on the Swing EDT with the pre-filled commit message and selected files.
     *
     * @param repoPath Path of the repository or project directory.
     * @param filePaths Optional list of specific files to pre-select. If null or empty, selects repository root.
     * @param message The initial commit message to pre-fill in the dialog.
     * @param authorName Optional author name. If omitted, uses repository gitconfig.
     * @param authorEmail Optional author email. If omitted, uses repository gitconfig.
     * @return Confirmation message of dialog dispatch.
     * @throws Exception if dialog dispatch fails.
     */
    @AgiTool("Opens the native NetBeans Git Commit dialog on the UI with pre-filled message and selected files.")
    public String gitOpenCommitDialog(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "Optional list of files to pre-select in the commit dialog.", required = false) List<String> filePaths,
            @AgiToolParam(value = "The initial commit message to pre-fill.") String message,
            @AgiToolParam(value = "Optional author name.", required = false) String authorName,
            @AgiToolParam(value = "Optional author email.", required = false) String authorEmail) throws Exception {

        File target = resolveFileOrDirectory(repoPath);
        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        final GitUser user;
        try {
            user = resolveGitUser(client, authorName, authorEmail);
        } finally {
            client.release();
        }

        final List<File> files = new ArrayList<>();
        if (filePaths != null && !filePaths.isEmpty()) {
            for (String p : filePaths) {
                files.add(resolveFile(p));
            }
        } else {
            files.add(target);
        }

        List<Node> nodes = new ArrayList<>();
        for (File f : files) {
            FileObject fo = FileUtil.toFileObject(f);
            if (fo == null) {
                throw new AgiToolException("Could not resolve NetBeans FileObject for: " + f.getAbsolutePath());
            }
            DataObject dobj = DataObject.find(fo);
            nodes.add(dobj.getNodeDelegate());
        }
        final VCSContext vcsContext = VCSContext.forNodes(nodes.toArray(Node[]::new));

        final boolean[] confirmed = new boolean[1];
        final List<String> pathsToCommit = new ArrayList<>();
        final String[] commitMsg = new String[1];
        final GitUser[] selectedAuthor = new GitUser[1];

        SwingUtils.runInEDTAndWait(() -> {
            GitCommitPanel panel = GitCommitPanel.create(files.toArray(File[]::new), repoRoot, user, true);
            if (message != null && !message.isBlank()) {
                findAndSetCommitMessage(panel, message.trim());
            }

            boolean ok = panel.open(vcsContext, HelpCtx.DEFAULT_HELP, "Commit - " + repoRoot.getName());
            if (ok) {
                confirmed[0] = true;
                for (GitLocalFileNode node : panel.getCommitTable().getCommitFiles()) {
                    if (node.getCommitOptions() != VCSCommitOptions.EXCLUDE) {
                        pathsToCommit.add(node.getFile().getAbsolutePath());
                    }
                }
                commitMsg[0] = panel.getParameters().getCommitMessage();
                selectedAuthor[0] = panel.getParameters().getAuthor();
            }
        });

        if (!confirmed[0]) {
            log("NetBeans Git Commit dialog was cancelled by user.");
            return "NetBeans Git Commit dialog was cancelled by user.";
        }

        if (pathsToCommit.isEmpty()) {
            log("Git commit dialog confirmed, but no files were selected for commit.");
            return "Git commit dialog confirmed, but no files were selected for commit.";
        }

        String authorNameVal = selectedAuthor[0] != null ? selectedAuthor[0].getName() : null;
        String authorEmailVal = selectedAuthor[0] != null ? selectedAuthor[0].getEmailAddress() : null;

        return gitCommit(repoRoot.getAbsolutePath(), pathsToCommit, commitMsg[0], authorNameVal, authorEmailVal);
    }

    /**
     * Pushes committed revisions to a remote Git repository.
     *
     * @param repoPath Path of the repository or project directory.
     * @param remote The remote name (e.g. 'origin'). Defaults to 'origin' if null.
     * @param branch Optional branch name to push (e.g. 'main'). If null, pushes current branch.
     * @return Confirmation message of push result.
     * @throws Exception if push fails.
     */
    @AgiTool("Pushes committed revisions to a remote Git repository.")
    public String gitPush(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The remote name (e.g. 'origin', 'helder'). If omitted, uses the tracked remote or 'origin'.", required = false) String remote,
            @AgiToolParam(value = "Optional branch name to push. If omitted, pushes current active branch.", required = false) String branch) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitBranch activeBranch = getActiveBranch(client, monitor);
            String branchName = (branch != null && !branch.isBlank()) ? branch.trim() : (activeBranch != null ? activeBranch.getName() : "main");
            String remoteName = resolveRemoteName(client, remote, activeBranch);

            GitRemoteConfig remoteCfg = client.getRemote(remoteName, monitor);
            List<String> fetchRefSpecs = resolveFetchRefSpecs(remoteCfg, remoteName);
            List<String> pushRefSpecs = Collections.singletonList("refs/heads/" + branchName + ":refs/heads/" + branchName);

            log("Pushing branch '" + branchName + "' to remote '" + remoteName + "' in " + repoRoot.getName());
            GitPushResult pushResult = client.push(remoteName, pushRefSpecs, fetchRefSpecs, monitor);

            StringBuilder sb = new StringBuilder();
            sb.append("### Git Push Successful\n\n");
            sb.append("- **Repository**: ").append(repoRoot.getName()).append("\n");
            sb.append("- **Branch**: `").append(branchName).append("`\n");
            sb.append("- **Remote**: `").append(remoteName).append("`\n");

            if (!pushResult.getRemoteRepositoryUpdates().isEmpty()) {
                sb.append("- **Remote Updates**: ").append(pushResult.getRemoteRepositoryUpdates().keySet()).append("\n");
            }
            if (!pushResult.getLocalRepositoryUpdates().isEmpty()) {
                sb.append("- **Local Tracking Updates**: ").append(pushResult.getLocalRepositoryUpdates().keySet()).append("\n");
            }

            log("Push to " + remoteName + " completed successfully in " + repoRoot.getName());
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Fetches remote branches, tags, and commits from a remote Git repository without modifying local working files.
     *
     * @param repoPath Path of the repository or project directory.
     * @param remote The remote name (e.g. 'origin', 'helder'). If omitted, defaults to tracked remote or 'origin'.
     * @return Markdown summary of fetched updates.
     * @throws Exception if fetch fails.
     */
    @AgiTool("Fetches updates from a remote Git repository into local tracking branches without modifying working files.")
    public String gitFetch(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The remote name (e.g. 'origin', 'helder'). Defaults to tracked remote or 'origin'.", required = false) String remote) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitBranch activeBranch = getActiveBranch(client, monitor);
            String remoteName = resolveRemoteName(client, remote, activeBranch);

            GitRemoteConfig remoteCfg = client.getRemote(remoteName, monitor);
            if (remoteCfg == null) {
                throw new AgiToolException("Remote '" + remoteName + "' is not configured in repository: " + repoRoot.getName());
            }

            List<String> uris = remoteCfg.getUris();
            if (uris.isEmpty()) {
                throw new AgiToolException("No URIs configured for remote '" + remoteName + "'");
            }
            String remoteUri = uris.get(0);
            List<String> fetchRefSpecs = resolveFetchRefSpecs(remoteCfg, remoteName);

            log("Fetching from remote '" + remoteName + "' (" + remoteUri + ") in " + repoRoot.getName());
            Map<String, GitTransportUpdate> updates = client.fetch(remoteUri, fetchRefSpecs, monitor);

            StringBuilder sb = new StringBuilder();
            sb.append("### Git Fetch: ").append(remoteName).append(" (").append(repoRoot.getName()).append(")\n\n");

            if (updates.isEmpty()) {
                sb.append("Already up to date. No remote updates found.\n");
            } else {
                sb.append("| Remote Reference | Result | Old Commit | New Commit |\n");
                sb.append("| :--- | :--- | :--- | :--- |\n");
                for (Map.Entry<String, GitTransportUpdate> entry : updates.entrySet()) {
                    GitTransportUpdate u = entry.getValue();
                    String oldId = u.getOldObjectId() != null ? u.getOldObjectId().substring(0, Math.min(7, u.getOldObjectId().length())) : "none";
                    String newId = u.getNewObjectId() != null ? u.getNewObjectId().substring(0, Math.min(7, u.getNewObjectId().length())) : "none";
                    sb.append("| `").append(u.getRemoteName()).append("` | `").append(u.getResult()).append("` | `").append(oldId).append("` | `").append(newId).append("` |\n");
                }
            }

            refreshVfs(repoRoot);
            log("Fetch completed for " + remoteName + " with " + updates.size() + " updates.");
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Lists all local and optionally remote branches for a Git repository.
     *
     * @param repoPath Path of the repository or project directory.
     * @param includeRemote Whether to include remote-tracking branches. Defaults to true.
     * @return Markdown table of all branches, their active state, and commit hash.
     * @throws Exception if listing branches fails.
     */
    @AgiTool(value = "Lists local and remote branches in a Git repository.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitBranches(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "Whether to include remote tracking branches. Defaults to true.", required = false) Boolean includeRemote) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        boolean all = includeRemote == null || includeRemote;
        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            Map<String, GitBranch> branches = client.getBranches(all, monitor);
            if (branches.isEmpty()) {
                return "No branches found for repository: " + repoRoot.getName();
            }

            StringBuilder sb = new StringBuilder();
            sb.append("### Branches: ").append(repoRoot.getName()).append("\n\n");
            sb.append("| Branch | Active | Type | Commit ID | Tracked Upstream |\n");
            sb.append("| :--- | :--- | :--- | :--- | :--- |\n");

            for (Map.Entry<String, GitBranch> entry : branches.entrySet()) {
                GitBranch b = entry.getValue();
                String activeStr = b.isActive() ? "**YES**" : "no";
                String typeStr = b.isRemote() ? "Remote" : "Local";
                String commitShort = b.getId() != null ? b.getId().substring(0, Math.min(7, b.getId().length())) : "none";
                String trackedStr = b.getTrackedBranch() != null ? "`" + b.getTrackedBranch().getName() + "`" : "-";

                sb.append("| `").append(b.getName()).append("` | ")
                  .append(activeStr).append(" | ")
                  .append(typeStr).append(" | `")
                  .append(commitShort).append("` | ")
                  .append(trackedStr).append(" |\n");
            }

            log("Listed " + branches.size() + " branches for: " + repoRoot.getName());
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Retrieves the commit log for a repository, specific branch, or revision.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchOrRevision Optional branch name (e.g. 'main', 'helder/main') or revision hash. If omitted, uses active branch HEAD.
     * @param maxEntries Maximum number of commits to retrieve. Defaults to 10.
     * @param filePath Optional file or directory path to limit the commit log to.
     * @param author Optional author name or email filter.
     * @param noMerges Whether to exclude merge commits. Defaults to false.
     * @return Markdown table of commit revisions, authors, dates, and messages.
     * @throws Exception if retrieving log fails.
     */
    @AgiTool(value = "Retrieves the commit history log for a Git repository, branch, or revision.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitLog(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "Optional branch name (e.g. 'main', 'helder/main') or commit hash. If omitted, uses active HEAD.", required = false) String branchOrRevision,
            @AgiToolParam(value = "Maximum number of commits to retrieve. Defaults to 10.", required = false) Integer maxEntries,
            @AgiToolParam(value = "Optional file or directory path to limit the commit log to.", required = false, rendererId = "path") String filePath,
            @AgiToolParam(value = "Optional author name or email filter.", required = false) String author,
            @AgiToolParam(value = "Whether to exclude merge commits. Defaults to false.", required = false) Boolean noMerges) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        int limit = (maxEntries != null && maxEntries > 0) ? maxEntries : 10;
        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            SearchCriteria crit = new SearchCriteria();
            crit.setLimit(limit);
            if (branchOrRevision != null && !branchOrRevision.isBlank()) {
                crit.setRevisionTo(branchOrRevision.trim());
            }
            if (author != null && !author.isBlank()) {
                crit.setUsername(author.trim());
            }
            if (noMerges != null && noMerges) {
                crit.setIncludeMerges(false);
            }
            if (filePath != null && !filePath.isBlank()) {
                crit.setFiles(new File[]{resolveRepoFile(repoRoot, filePath)});
            }

            GitRevisionInfo[] revisions = client.log(crit, false, monitor);
            if (revisions == null || revisions.length == 0) {
                return "No commits found for " + (branchOrRevision != null ? branchOrRevision : "HEAD") + " in " + repoRoot.getName();
            }

            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
            StringBuilder sb = new StringBuilder();
            String targetLabel = (branchOrRevision != null && !branchOrRevision.isBlank()) ? branchOrRevision : "HEAD";
            sb.append("### Git Log: ").append(repoRoot.getName()).append(" [").append(targetLabel).append("]\n\n");
            sb.append("| Date | Revision | Author | Message |\n");
            sb.append("| :--- | :--- | :--- | :--- |\n");

            for (GitRevisionInfo rev : revisions) {
                String shortHash = rev.getRevision().substring(0, Math.min(7, rev.getRevision().length()));
                String dateStr = sdf.format(new Date(rev.getCommitTime()));
                String authorStr = rev.getAuthor() != null ? rev.getAuthor().getName() : "Unknown";
                String msgStr = rev.getShortMessage() != null ? rev.getShortMessage().replace("\n", " ").trim() : "";

                sb.append("| ").append(dateStr).append(" | `")
                  .append(shortHash).append("` | ")
                  .append(authorStr).append(" | ")
                  .append(msgStr).append(" |\n");
            }

            log("Retrieved " + revisions.length + " commits for " + targetLabel + " in " + repoRoot.getName());
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Generates a Git diff between two branches or revisions, optionally filtered to a specific file or folder.
     *
     * @param repoPath Path of the repository or project directory.
     * @param baseRevision The base branch or revision hash (e.g. 'main', 'HEAD~1').
     * @param targetRevision The target branch or revision hash (e.g. 'helder/feat.service-database-tool', 'HEAD').
     * @param filePath Optional file or folder path to limit the diff to.
     * @return Unified diff output as text.
     * @throws Exception if diff generation fails.
     */
    @AgiTool(value = "Generates a Git diff between two branches or revisions, optionally filtered to a specific file.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitDiff(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The base branch or revision hash (e.g. 'main', 'HEAD~1').") String baseRevision,
            @AgiToolParam(value = "The target branch or revision hash (e.g. 'helder/feat.service-database-tool', 'HEAD').") String targetRevision,
            @AgiToolParam(value = "Optional specific file or folder path to limit the diff to.", required = false, rendererId = "path") String filePath) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            File[] files = (filePath != null && !filePath.isBlank())
                    ? new File[]{resolveRepoFile(repoRoot, filePath)}
                    : new File[]{repoRoot};

            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            client.exportDiff(files, baseRevision.trim(), targetRevision.trim(), baos, monitor);

            String diff = baos.toString(StandardCharsets.UTF_8).trim();
            if (diff.isBlank()) {
                return "No differences found between " + baseRevision + " and " + targetRevision + (filePath != null ? " for " + filePath : "");
            }
            return diff;
        } finally {
            client.release();
        }
    }

    /**
     * Reads and returns the content of a file from a specific Git branch or revision without modifying working files.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchOrRevision The branch name or revision hash (e.g. 'helder/feat.service-database-tool', 'HEAD~2').
     * @param filePath The relative or absolute file path to read.
     * @return The raw text content of the file at that revision.
     * @throws Exception if reading the file fails.
     */
    @AgiTool(value = "Reads and returns the content of a file from a specific Git branch or revision without switching branches.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitShow(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The branch name or revision hash (e.g. 'helder/feat.service-database-tool', 'HEAD~2').") String branchOrRevision,
            @AgiToolParam(value = "The relative or absolute file path to read.", rendererId = "path") String filePath) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);
        File file = resolveRepoFile(repoRoot, filePath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            client.catFile(file, branchOrRevision.trim(), baos, monitor);
            return baos.toString(StandardCharsets.UTF_8);
        } finally {
            client.release();
        }
    }

    /**
     * Pulls updates from a remote repository and merges them into the current active branch.
     *
     * @param repoPath Path of the repository or project directory.
     * @param remote The remote name (e.g. 'origin', 'helder'). If omitted, defaults to tracked remote or 'origin'.
     * @return Markdown summary of fetch updates and merge outcome.
     * @throws Exception if pull or merge fails.
     */
    @AgiTool("Pulls changes from a remote Git repository into the current active branch (fetch and merge).")
    public String gitPull(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The remote name (e.g. 'origin', 'helder'). Defaults to tracked remote or 'origin'.", required = false) String remote) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitBranch activeBranch = getActiveBranch(client, monitor);
            if (activeBranch == null) {
                throw new AgiToolException("Cannot pull: repository is in detached HEAD state in " + repoRoot.getName());
            }

            String remoteName = resolveRemoteName(client, remote, activeBranch);
            GitRemoteConfig remoteCfg = client.getRemote(remoteName, monitor);
            if (remoteCfg == null) {
                throw new AgiToolException("Remote '" + remoteName + "' is not configured in repository: " + repoRoot.getName());
            }

            List<String> uris = remoteCfg.getUris();
            if (uris.isEmpty()) {
                throw new AgiToolException("No URIs configured for remote '" + remoteName + "'");
            }
            String remoteUri = uris.get(0);
            List<String> fetchRefSpecs = resolveFetchRefSpecs(remoteCfg, remoteName);
            String branchToMerge = remoteName + "/" + activeBranch.getName();

            log("Pulling branch '" + branchToMerge + "' from " + remoteName + " (" + remoteUri + ") into " + activeBranch.getName());
            GitPullResult pullResult = client.pull(remoteUri, fetchRefSpecs, branchToMerge, monitor);

            StringBuilder sb = new StringBuilder();
            sb.append("### Git Pull Result: ").append(repoRoot.getName()).append("\n\n");
            sb.append("- **Active Branch**: `").append(activeBranch.getName()).append("`\n");
            sb.append("- **Remote**: `").append(remoteName).append("`\n");

            GitMergeResult mergeResult = pullResult.getMergeResult();
            if (mergeResult != null) {
                sb.append("- **Merge Status**: `").append(mergeResult.getMergeStatus()).append("`\n");
                if (mergeResult.getNewHead() != null) {
                    String newHeadShort = mergeResult.getNewHead().substring(0, Math.min(7, mergeResult.getNewHead().length()));
                    sb.append("- **New HEAD**: `").append(newHeadShort).append("`\n");
                }
                if (mergeResult.getConflicts() != null && !mergeResult.getConflicts().isEmpty()) {
                    sb.append("\n⚠️ **Conflicts Encountered**:\n");
                    for (File conflict : mergeResult.getConflicts()) {
                        sb.append("- `").append(repoRoot.toPath().relativize(conflict.toPath())).append("`\n");
                    }
                }
            }

            refreshVfs(repoRoot);
            log("Pull completed for " + repoRoot.getName() + " mergeStatus=" + (mergeResult != null ? mergeResult.getMergeStatus() : "unknown"));
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Switches the working tree to a target branch or revision, optionally creating the branch if it does not exist.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchOrRevision The branch name or commit revision to checkout.
     * @param createIfMissing If true, creates a new branch from current HEAD if the branch does not already exist.
     * @return Confirmation message of the checkout operation.
     * @throws Exception if checkout fails or working tree has uncommitted conflicts.
     */
    @AgiTool("Checks out a Git branch or revision, optionally creating the branch if missing.")
    public String gitCheckout(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The branch name or revision hash to checkout.") String branchOrRevision,
            @AgiToolParam(value = "Whether to create the branch if it does not exist. Defaults to false.", required = false) Boolean createIfMissing) throws Exception {

        if (branchOrRevision == null || branchOrRevision.isBlank()) {
            throw new AgiToolException("Target branch or revision cannot be empty.");
        }

        File repoRoot = requireRepoRoot(repoPath);
        String targetName = branchOrRevision.trim();
        boolean create = createIfMissing != null && createIfMissing;

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            Map<String, GitBranch> branches = client.getBranches(false, monitor);
            boolean exists = branches.containsKey(targetName);

            if (!exists && create) {
                log("Branch '" + targetName + "' does not exist. Creating branch from HEAD...");
                client.createBranch(targetName, "HEAD", monitor);
            }

            log("Checking out revision/branch: " + targetName);
            client.checkoutRevision(targetName, true, monitor);
            refreshVfs(repoRoot);

            log("Successfully checked out " + targetName + " in " + repoRoot.getName());
            return "Successfully checked out branch/revision: `" + targetName + "` in " + repoRoot.getName();
        } finally {
            client.release();
        }
    }

    /**
     * Creates a new Git branch at a specific revision or current HEAD.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchName Name of the new branch to create.
     * @param startRevision Optional starting revision hash or branch name. Defaults to 'HEAD'.
     * @return Confirmation message of branch creation.
     * @throws Exception if branch creation fails.
     */
    @AgiTool("Creates a new Git branch at a specified revision or current HEAD.")
    public String gitCreateBranch(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The name of the new branch to create.") String branchName,
            @AgiToolParam(value = "Optional starting revision hash or branch name. Defaults to 'HEAD'.", required = false) String startRevision) throws Exception {

        if (branchName == null || branchName.isBlank()) {
            throw new AgiToolException("Branch name cannot be empty.");
        }

        File repoRoot = requireRepoRoot(repoPath);
        String start = (startRevision != null && !startRevision.isBlank()) ? startRevision.trim() : "HEAD";
        String name = branchName.trim();

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitBranch branch = client.createBranch(name, start, monitor);
            log("Created branch '" + name + "' at " + start + " in " + repoRoot.getName());
            return "Successfully created branch `" + name + "` at revision `" + (branch.getId() != null ? branch.getId().substring(0, Math.min(7, branch.getId().length())) : start) + "` in " + repoRoot.getName();
        } finally {
            client.release();
        }
    }

    /**
     * Deletes a local Git branch.
     *
     * @param repoPath Path of the repository or project directory.
     * @param branchName Name of the branch to delete.
     * @param force Whether to force deletion even if unmerged. Defaults to false.
     * @return Confirmation message of branch deletion.
     * @throws Exception if branch deletion fails.
     */
    @AgiTool("Deletes a local Git branch.")
    public String gitDeleteBranch(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The name of the branch to delete.") String branchName,
            @AgiToolParam(value = "Whether to force delete unmerged commits. Defaults to false.", required = false) Boolean force) throws Exception {

        if (branchName == null || branchName.isBlank()) {
            throw new AgiToolException("Branch name cannot be empty.");
        }

        File repoRoot = requireRepoRoot(repoPath);
        boolean forceDelete = force != null && force;
        String name = branchName.trim();

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            client.deleteBranch(name, forceDelete, monitor);
            log("Deleted branch '" + name + "' in " + repoRoot.getName());
            return "Successfully deleted branch `" + name + "` from " + repoRoot.getName();
        } finally {
            client.release();
        }
    }

    /**
     * Retrieves line-by-line authorship and revision history (Git Blame) for a file.
     *
     * @param filePath The absolute path of the file to blame.
     * @param startLine Optional 1-based start line number. Defaults to 1.
     * @param endLine Optional 1-based end line number. If omitted, blames to end of file.
     * @param revision Optional revision to blame against. Defaults to working tree / HEAD.
     * @return Formatted Markdown table containing line numbers, commit hashes, authors, dates, and line contents.
     * @throws Exception if blame retrieval fails.
     */
    @AgiTool(value = "Retrieves line-by-line authorship and commit history (Git Blame) for a file.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitBlame(
            @AgiToolParam(value = "The absolute path of the file to inspect.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Optional 1-based starting line number. Defaults to 1.", required = false) Integer startLine,
            @AgiToolParam(value = "Optional 1-based ending line number. If omitted, blames to end of file.", required = false) Integer endLine,
            @AgiToolParam(value = "Optional revision to blame against. Defaults to HEAD.", required = false) String revision) throws Exception {

        File file = resolveFile(filePath);
        File repoRoot = findRepoRoot(file);
        if (repoRoot == null) {
            throw new AgiToolException("File is not inside a Git repository: " + filePath);
        }

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            String rev = (revision != null && !revision.isBlank()) ? revision.trim() : null;
            GitBlameResult result = client.blame(file, rev, monitor);

            int totalLines = result.getLineCount();
            if (totalLines == 0) {
                return "File is empty: " + file.getName();
            }

            int start = (startLine != null && startLine > 0) ? Math.min(startLine, totalLines) : 1;
            int end = (endLine != null && endLine >= start) ? Math.min(endLine, totalLines) : totalLines;

            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            StringBuilder sb = new StringBuilder();
            sb.append("### Git Blame: ").append(file.getName()).append(" (lines ").append(start).append("-").append(end).append(" of ").append(totalLines).append(")\n\n");
            sb.append("| Line | Commit | Author | Date | Content |\n");
            sb.append("| :--- | :--- | :--- | :--- | :--- |\n");

            for (int i = start - 1; i < end; i++) {
                GitLineDetails details = result.getLineDetails(i);
                int lineNum = i + 1;
                if (details == null || details.getRevisionInfo() == null) {
                    sb.append("| ").append(lineNum).append(" | - | - | - | `").append(details != null ? details.getContent() : "").append("` |\n");
                    continue;
                }

                String commitShort = details.getRevisionInfo().getRevision().substring(0, Math.min(7, details.getRevisionInfo().getRevision().length()));
                String author = details.getAuthor() != null ? details.getAuthor().getName() : "Unknown";
                String dateStr = sdf.format(new Date(details.getRevisionInfo().getCommitTime()));
                String content = details.getContent() != null ? details.getContent().replace("`", "'") : "";

                sb.append("| ").append(lineNum).append(" | `")
                  .append(commitShort).append("` | ")
                  .append(author).append(" | ")
                  .append(dateStr).append(" | `")
                  .append(content).append("` |\n");
            }

            log("Retrieved blame for " + file.getName() + " lines " + start + "-" + end);
            return sb.toString().trim();
        } finally {
            client.release();
        }
    }

    /**
     * Traverses a Swing container hierarchy to locate the commit message JTextArea and set its text.
     *
     * @param container The container to search.
     * @param text The text to set.
     * @return true if a JTextArea was found and updated, false otherwise.
     */
    private static boolean findAndSetCommitMessage(Container container, String text) {
        for (Component c : container.getComponents()) {
            if (c instanceof JTextArea ta) {
                ta.setText(text);
                return true;
            } else if (c instanceof Container child) {
                if (findAndSetCommitMessage(child, text)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Resolves a GitUser from parameters, repository client defaults, or host system username.
     *
     * @param client The active GitClient.
     * @param authorName Optional provided author name.
     * @param authorEmail Optional provided author email.
     * @return Valid GitUser instance.
     */
    private GitUser resolveGitUser(GitClient client, String authorName, String authorEmail) {
        if (authorName != null && !authorName.isBlank() && authorEmail != null && !authorEmail.isBlank()) {
            return new GitUser(authorName.trim(), authorEmail.trim());
        }
        try {
            GitUser defaultUser = client.getUser();
            if (defaultUser != null && defaultUser.getName() != null && defaultUser.getEmailAddress() != null) {
                return defaultUser;
            }
        } catch (Exception ex) {
            log.warn("Could not retrieve default Git user from GitClient: {}", ex.getMessage());
        }
        String name = (authorName != null && !authorName.isBlank()) ? authorName.trim() : System.getProperty("user.name", "Anahata");
        String email = (authorEmail != null && !authorEmail.isBlank()) ? authorEmail.trim() : name + "@local";
        return new GitUser(name, email);
    }

    /**
     * Resolves the repository root for a target file or folder, throwing an AgiToolException if not inside a Git repo.
     *
     * @param repoPath Path of the repository or project directory.
     * @return The repository root directory.
     * @throws AgiToolException if path is invalid or not inside a Git repository.
     */
    private File requireRepoRoot(String repoPath) throws AgiToolException {
        File target = resolveFileOrDirectory(repoPath);
        File repoRoot = findRepoRoot(target);
        if (repoRoot == null) {
            throw new AgiToolException("Target is not inside a Git repository: " + repoPath);
        }
        return repoRoot;
    }

    /**
     * Resolves a file path against a repository root, handling both relative and absolute paths.
     *
     * @param repoRoot The repository root directory.
     * @param filePath The file path string.
     * @return Normalized File instance.
     */
    private File resolveRepoFile(File repoRoot, String filePath) {
        File file = new File(filePath);
        return file.isAbsolute() ? file : new File(repoRoot, filePath);
    }

    /**
     * Resolves the currently active branch in a Git repository.
     *
     * @param client The active GitClient.
     * @param monitor The progress monitor.
     * @return The active GitBranch, or null if detached HEAD.
     * @throws Exception if querying branches fails.
     */
    private GitBranch getActiveBranch(GitClient client, ProgressMonitor monitor) throws Exception {
        for (GitBranch b : client.getBranches(false, monitor).values()) {
            if (b.isActive()) {
                return b;
            }
        }
        return null;
    }

    /**
     * Resolves the target remote name from an explicit parameter, tracked branch configuration, or default 'origin'.
     *
     * @param client The active GitClient.
     * @param remote The explicit remote name (optional).
     * @param activeBranch The active branch (optional).
     * @return The resolved remote name.
     */
    private String resolveRemoteName(GitClient client, String remote, GitBranch activeBranch) {
        if (remote != null && !remote.isBlank()) {
            return remote.trim();
        }
        if (activeBranch != null && activeBranch.getTrackedBranch() != null) {
            String tracked = activeBranch.getTrackedBranch().getName();
            int slash = tracked.indexOf('/');
            if (slash > 0) {
                return tracked.substring(0, slash);
            }
        }
        return "origin";
    }

    /**
     * Resolves fetch refspecs from a remote config with fallback to canonical remote mapping.
     *
     * @param remoteCfg The remote configuration.
     * @param remoteName The remote name.
     * @return List of fetch refspecs.
     */
    private List<String> resolveFetchRefSpecs(GitRemoteConfig remoteCfg, String remoteName) {
        return (remoteCfg != null && !remoteCfg.getFetchRefSpecs().isEmpty())
                ? remoteCfg.getFetchRefSpecs()
                : Collections.singletonList("+refs/heads/*:refs/remotes/" + remoteName + "/*");
    }

    /**
     * Refreshes the NetBeans Virtual FileSystem for a given file or directory.
     *
     * @param file The file or directory to refresh.
     */
    private void refreshVfs(File file) {
        FileObject fo = FileUtil.toFileObject(file);
        if (fo != null) {
            fo.refresh();
        }
    }

    /**
     * Resolves the repository root for a target file or folder using NetBeans VersioningSupport.
     *
     * @param file The file or folder to inspect.
     * @return The repository root directory, or null if not managed.
     */
    private File findRepoRoot(File file) {
        VersioningSystem vs = VersioningSupport.getOwner(file);
        return vs != null ? vs.getTopmostManagedAncestor(file) : null;
    }

    /**
     * Resolves a validated, normalized File or directory from a path string.
     *
     * @param path The path string to resolve.
     * @return The normalized File.
     * @throws AgiToolException if the path is invalid or does not exist.
     */
    private File resolveFileOrDirectory(String path) throws AgiToolException {
        if (path == null || path.isBlank()) {
            throw new AgiToolException("Path cannot be empty.");
        }
        File file = FileUtil.normalizeFile(new File(path));
        if (!file.exists()) {
            throw new AgiToolException("Path does not exist: " + path);
        }
        return file;
    }

    /**
     * Resolves a validated directory from a path string.
     *
     * @param path The path string to resolve.
     * @return The normalized directory File.
     * @throws AgiToolException if the path does not exist or is not a directory.
     */
    private File resolveDirectory(String path) throws AgiToolException {
        File file = resolveFileOrDirectory(path);
        if (!file.isDirectory()) {
            throw new AgiToolException("Path is not a directory: " + path);
        }
        return file;
    }

    /**
     * Resolves a validated, normalized File from an absolute path string.
     *
     * @param path The path string to resolve.
     * @return The normalized File.
     * @throws AgiToolException if the path is invalid or the file does not exist.
     */
    private File resolveFile(String path) throws AgiToolException {
        if (path == null || path.isBlank()) {
            throw new AgiToolException("File path cannot be empty.");
        }
        File file = FileUtil.normalizeFile(new File(path));
        if (!file.exists()) {
            throw new AgiToolException("File does not exist: " + path);
        }
        return file;
    }

    /**
     * Custom NetBeans ProgressMonitor that forwards progress and diagnostic messages
     * live to the active ToolContext logs and error reporting.
     */
    private class ToolProgressMonitor extends ProgressMonitor {
        private final ToolContext ctx = getToolContext();
        private boolean canceled;

        @Override
        public synchronized boolean isCanceled() {
            return canceled;
        }

        @Override
        public void started(String command) {
            if (command != null && !command.isBlank()) {
                ctx.log("Git command started: " + command);
            }
        }

        @Override
        public void finished() {
            ctx.log("Git command finished.");
        }

        @Override
        public void preparationsFailed(String message) {
            ctx.error("Git preparation failed: " + message);
        }

        @Override
        public void notifyError(String message) {
            ctx.error("Git error: " + message);
        }

        @Override
        public void notifyWarning(String message) {
            ctx.log("Git warning: " + message);
        }

        @Override
        public void notifyMessage(String message) {
            if (message != null && !message.isBlank()) {
                ctx.log(message);
            }
        }

        @Override
        public void beginTask(String taskName, int totalWorkUnits) {
            if (taskName != null && !taskName.isBlank()) {
                ctx.log("Git task: " + taskName);
            }
        }
    }
}
