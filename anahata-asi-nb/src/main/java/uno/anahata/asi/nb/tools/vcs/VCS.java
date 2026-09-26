/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.vcs;

import com.github.difflib.DiffUtils;
import com.github.difflib.UnifiedDiffUtils;
import com.github.difflib.patch.Patch;
import java.awt.Component;
import java.awt.Container;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.libs.git.GitBranch;
import org.netbeans.libs.git.GitClient;
import org.netbeans.libs.git.GitPushResult;
import org.netbeans.libs.git.GitRepository;
import org.netbeans.libs.git.GitRevisionInfo;
import org.netbeans.libs.git.GitStatus;
import org.netbeans.libs.git.GitUser;
import org.netbeans.libs.git.progress.ProgressMonitor;
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
import org.openide.loaders.DataObjectNotFoundException;
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
        GitRepository repo = GitRepository.getInstance(dir);
        GitClient client = repo.createClient();
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
        File repoRoot = findRepoRoot(target);
        if (repoRoot == null) {
            throw new AgiToolException("Path is not inside a Git repository: " + path);
        }

        GitRepository repo = GitRepository.getInstance(repoRoot);
        GitClient client = repo.createClient();
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            Map<String, GitBranch> branches = client.getBranches(false, monitor);
            String activeBranch = "HEAD";
            for (Map.Entry<String, GitBranch> entry : branches.entrySet()) {
                if (entry.getValue().isActive()) {
                    activeBranch = entry.getKey();
                    break;
                }
            }

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

        GitRepository repo = GitRepository.getInstance(repoRoot);
        GitClient client = repo.createClient();
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

        File target = resolveFileOrDirectory(repoPath);
        File repoRoot = findRepoRoot(target);
        if (repoRoot == null) {
            throw new AgiToolException("Target is not inside a Git repository: " + repoPath);
        }

        GitRepository repo = GitRepository.getInstance(repoRoot);
        GitClient client = repo.createClient();
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
                // Single-shot execution: automatically stage the specified files first!
                client.add(filesToCommit, monitor);
            } else {
                filesToCommit = new File[]{repoRoot};
            }

            GitRevisionInfo info = client.commit(filesToCommit, message.trim(), user, user, monitor);

            FileObject fo = FileUtil.toFileObject(repoRoot);
            if (fo != null) {
                fo.refresh();
            }

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
        File repoRoot = findRepoRoot(target);
        if (repoRoot == null) {
            throw new AgiToolException("Target is not inside a Git repository: " + repoPath);
        }

        GitRepository repo = GitRepository.getInstance(repoRoot);
        GitClient client = repo.createClient();
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
            @AgiToolParam(value = "The remote name (e.g. 'origin'). Defaults to 'origin'.", required = false) String remote,
            @AgiToolParam(value = "Optional branch name to push. If omitted, pushes current branch.", required = false) String branch) throws Exception {

        File target = resolveFileOrDirectory(repoPath);
        File repoRoot = findRepoRoot(target);
        if (repoRoot == null) {
            throw new AgiToolException("Target is not inside a Git repository: " + repoPath);
        }

        String remoteName = (remote != null && !remote.isBlank()) ? remote.trim() : "origin";
        List<String> pushRefSpecs = (branch != null && !branch.isBlank()) ? Collections.singletonList("refs/heads/" + branch.trim()) : Collections.emptyList();

        GitRepository repo = GitRepository.getInstance(repoRoot);
        GitClient client = repo.createClient();
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitPushResult pushResult = client.push(remoteName, pushRefSpecs, Collections.emptyList(), monitor);
            log("Pushed to remote '" + remoteName + "' in " + repoRoot.getName());
            return "Successfully pushed to remote '" + remoteName + "' for " + repoRoot.getName();
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
