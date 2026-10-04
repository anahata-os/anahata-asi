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
import org.netbeans.libs.git.GitRepository.FastForwardOption;
import org.netbeans.libs.git.GitRevisionInfo;
import org.netbeans.libs.git.GitStatus;
import org.netbeans.libs.git.GitTransportUpdate;
import org.netbeans.libs.git.GitUser;
import org.netbeans.libs.git.SearchCriteria;
import org.netbeans.libs.git.progress.ProgressMonitor;
import org.netbeans.modules.git.Git;
import static org.netbeans.libs.git.GitClient.WORKING_TREE;
import org.netbeans.modules.git.client.GitClient;
import org.netbeans.modules.git.ui.commit.GitCommitPanel;
import org.netbeans.modules.localhistory.LocalHistory;
import org.netbeans.modules.localhistory.store.LocalHistoryStore;
import org.netbeans.modules.localhistory.store.StoreEntry;
import org.netbeans.modules.versioning.core.api.VCSFileProxy;
import org.netbeans.modules.versioning.spi.VCSHistoryProvider;
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
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.vcs.FastForwardPolicy;
import uno.anahata.asi.agi.resource.vcs.HistoryEntry;
import uno.anahata.asi.agi.resource.vcs.VcsDiff;
import uno.anahata.asi.agi.resource.vcs.VcsFileStatus;
import uno.anahata.asi.toolkit.vcs.AbstractVCS;
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
public class NbVCS extends AbstractVCS {

    /**
     * {@inheritDoc}
     * <p>Contributes system instructions describing how to access NetBeans's native GitClient for advanced Git operations.</p>
     */
    @Override
    public List<String> getSystemInstructions() {
        return List.of("""
        ### NetBeans VCS & Git Engine Direct Access:
        For advanced Git operations not exposed as discrete `@AgiTool` methods (such as cherry-pick, interactive rebase, stash, hard reset, or tag management), you can use `NbJava.compileAndExecute` to obtain NetBeans's managed `GitClient`:
        ```java
        File repoRoot = new File("/path/to/repo");
        GitClient client = Git.getInstance().getClient(repoRoot);
        try {
            // client.cherryPick(revision, ...);
            // client.rebase(op, branch, ...);
            // client.stashSave(message, ...);
            // client.createTag(tagName, revision, ...);
            // client.reset(revision, type, ...);
        } finally {
            client.release();
        }
        ```
        This client is pre-wired with NetBeans's Keyring credentials, SSH session factory, and VFS synchronization.
        """);
    }

    /**
     * {@inheritDoc}
     * <p>Populates the RAG message with application-wide Git user configuration.</p>
     */
    @Override
    public void populateMessage(RagMessage message) {
        GitUser user = getGlobalGitUser();
        if (user != null) {
            StringBuilder sb = new StringBuilder();
            sb.append("## Global Git User Configuration\n");
            sb.append("- **Name**: ").append(user.getName()).append("\n");
            sb.append("- **Email**: ").append(user.getEmailAddress()).append("\n");
            message.addTextPart(sb.toString().trim());
        }
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
     * @return A list of {@link HistoryEntry} DTOs sorted in reverse chronological order.
     * @throws Exception if querying history fails.
     */
    @AgiTool(value = "Queries the unified chronological history of a file, combining VCS commits and NetBeans Local History.", permission = ToolPermission.APPROVE_ALWAYS)
    public List<HistoryEntry> getHistory(
            @AgiToolParam(value = "The absolute path of the file.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Maximum number of history entries to return. Defaults to 10.", required = false) Integer maxEntries) throws Exception {

        File file = resolveFile(filePath);
        int limit = (maxEntries != null && maxEntries > 0) ? maxEntries : 10;
        List<HistoryEntry> history = new ArrayList<>();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        // 1. VCS entries via NetBeans VersioningSupport (works for Git, SVN, Mercurial)
        VersioningSystem vs = VersioningSupport.getOwner(file);
        if (vs != null) {
            String vcsName = (String) vs.getProperty(VersioningSystem.PROP_DISPLAY_NAME);
            if (vcsName == null || vcsName.isBlank()) {
                vcsName = "VCS";
            }
            VCSHistoryProvider hp = vs.getVCSHistoryProvider();
            if (hp != null) {
                VCSHistoryProvider.HistoryEntry[] entries = hp.getHistory(new File[]{file}, null);
                if (entries != null) {
                    for (VCSHistoryProvider.HistoryEntry ge : entries) {
                        String msg = ge.getMessage() != null ? ge.getMessage().replace("\n", " ").trim() : "";
                        Date d = ge.getDateTime();
                        history.add(new HistoryEntry(d.getTime(), sdf.format(d), vcsName, ge.getRevisionShort(), ge.getUsernameShort(), msg));
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
                    history.add(new HistoryEntry(se.getTimestamp(), sdf.format(date), "Local History", "Local", "", label));
                }
            }
        }

        // 3. Sort descending (newest first, via HistoryEntry.compareTo)
        Collections.sort(history);

        //log("Found " + history.size() + " total history entries for " + file.getName() + ", returning " + Math.min(limit, history.size()));
        return history.size() <= limit ? history : new ArrayList<>(history.subList(0, limit));
    }

    /**
     * Generates a unified diff for a file against its pristine repository base or against a specific historical revision.
     *
     * @param filePath The absolute path of the file to inspect.
     * @param revision Optional revision identifier (e.g. commit hash or revision number). If omitted, diffs against the repository pristine base.
     * @return A {@link VcsDiff} DTO containing diff text and status classification.
     * @throws Exception if diff generation fails.
     */
    @AgiTool(value = "Generates a unified diff for a file against repository base or a specific revision using NetBeans APIs.", permission = ToolPermission.APPROVE_ALWAYS)
    public VcsDiff getDiff(
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
            VCSHistoryProvider.HistoryEntry[] entries = hp.getHistory(new File[]{file}, null);
            VCSHistoryProvider.HistoryEntry targetEntry = null;
            if (entries != null) {
                for (VCSHistoryProvider.HistoryEntry e : entries) {
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
            if (!tempBase.exists() || (tempBase.length() == 0 && file.length() > 0)) {
                
                return VcsDiff.builder()
                        .filePath(file.getAbsolutePath())
                        .status(VcsFileStatus.NEW)
                        .baseRevision(revision != null ? revision : "HEAD")
                        .targetRevision("WORKING_COPY")
                        .build();
                
            }
        }

        List<String> originalLines = Files.readAllLines(tempBase.toPath(), StandardCharsets.UTF_8);
        List<String> revisedLines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8);

        Patch<String> patch = DiffUtils.diff(originalLines, revisedLines);
        List<String> unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(file.getName(), file.getName(), originalLines, patch, 3);

        if (unifiedDiff.isEmpty()) {
            //log("File is identical to base revision: " + filePath);
            
            return VcsDiff.builder()
                    .filePath(file.getAbsolutePath())
                    .status(VcsFileStatus.CLEAN)
                    .baseRevision(revision != null ? revision : "HEAD")
                    .targetRevision("WORKING_COPY")
                    .build();
            
        }

        //log("Generated unified diff (" + unifiedDiff.size() + " lines) for: " + file.getName());
        
        return VcsDiff.builder()
                .filePath(file.getAbsolutePath())
                .status(VcsFileStatus.MODIFIED)
                .baseRevision(revision != null ? revision : "HEAD")
                .targetRevision("WORKING_COPY")
                .diff(String.join("\n", unifiedDiff))
                .build();
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
     * <p>
     * Automatically differentiates between existing files (staged via {@code GitClient.add})
     * and deleted files (staged via {@code GitClient.remove}).
     * </p>
     *
     * @param filePaths List of file paths to stage into the Git index (supports newly created, modified, and deleted files).
     * @return Confirmation message of staged files.
     * @throws Exception if staging fails.
     */
    @AgiTool("Stages one or more files into the Git index (supports created, modified, and deleted files).")
    public String gitAdd(
            @AgiToolParam(value = "List of file paths to stage into the Git index (supports created, modified, and deleted files).", rendererId = "path") List<String> filePaths) throws Exception {

        if (filePaths == null || filePaths.isEmpty()) {
            throw new AgiToolException("No files specified to stage.");
        }

        List<File> toAdd = new ArrayList<>();
        List<File> toRemove = new ArrayList<>();
        File repoRoot = null;
        for (String p : filePaths) {
            if (p == null || p.isBlank()) {
                throw new AgiToolException("File path cannot be empty.");
            }
            File f = FileUtil.normalizeFile(new File(p));
            if (repoRoot == null) {
                repoRoot = findRepoRoot(f.exists() ? f : f.getParentFile());
            }
            if (f.exists()) {
                toAdd.add(f);
            } else {
                toRemove.add(f);
            }
        }

        if (repoRoot == null) {
            throw new AgiToolException("Files are not inside a Git repository.");
        }

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            if (!toAdd.isEmpty()) {
                client.add(toAdd.toArray(File[]::new), monitor);
            }
            if (!toRemove.isEmpty()) {
                client.remove(toRemove.toArray(File[]::new), false, monitor);
            }
        } finally {
            client.release();
        }

        refreshVfs(repoRoot);

        int totalStaged = toAdd.size() + toRemove.size();
        log("Staged " + totalStaged + " files into Git index.");
        return "Successfully staged " + totalStaged + " file(s) into Git index.";
    }

    /**
     * Commits changes headlessly in a single shot (automatically staging added, modified, or deleted files if specified) using NetBeans GitClient.
     * <p>
     * Differentiates between existing files (staged via {@code GitClient.add}) and deleted files
     * (verified against repository tracking and staged via {@code GitClient.remove}).
     * </p>
     *
     * @param repoPath Path of the repository or any contained file.
     * @param filePaths Optional list of specific files to stage and commit (supports added, modified, and deleted files). If omitted, commits all staged files in the repository.
     * @param message The commit message.
     * @param authorName Optional author name. If omitted, uses repository gitconfig or system user.
     * @param authorEmail Optional author email. If omitted, uses repository gitconfig.
     * @return Details of the created commit revision.
     * @throws Exception if commit fails.
     */
    @AgiTool("Commits changes headlessly in a single shot (automatically staging added, modified, or deleted files if specified).")
    public String gitCommit(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "Optional list of specific files to stage and commit (supports added, modified, and deleted files). If omitted, commits all staged files.", required = false, rendererId = "path") List<String> filePaths,
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
                List<File> toAdd = new ArrayList<>();
                List<File> toRemove = new ArrayList<>();
                List<File> allFiles = new ArrayList<>();
                for (String p : filePaths) {
                    File f = FileUtil.normalizeFile(resolveRepoFile(repoRoot, p));
                    if (f.exists()) {
                        toAdd.add(f);
                        allFiles.add(f);
                    } else {
                        Map<File, GitStatus> statusMap = client.getStatus(new File[]{f}, monitor);
                        GitStatus s = statusMap.get(f);
                        if (s != null && (s.getStatusHeadWC() == GitStatus.Status.STATUS_REMOVED
                                || s.getStatusIndexWC() == GitStatus.Status.STATUS_REMOVED
                                || s.isTracked())) {
                            toRemove.add(f);
                            allFiles.add(f);
                        } else {
                            throw new AgiToolException("File does not exist and is not tracked in repository: " + f.getAbsolutePath());
                        }
                    }
                }
                if (!toAdd.isEmpty()) {
                    client.add(toAdd.toArray(File[]::new), monitor);
                }
                if (!toRemove.isEmpty()) {
                    client.remove(toRemove.toArray(File[]::new), false, monitor);
                }
                filesToCommit = allFiles.toArray(File[]::new);
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
            @AgiToolParam(value = "Optional list of files to pre-select in the commit dialog.", required = false, rendererId = "path") List<String> filePaths,
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
                File f = resolveRepoFile(repoRoot, p);
                if (!f.exists()) {
                    throw new AgiToolException("File does not exist: " + f.getAbsolutePath());
                }
                files.add(f);
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
            @AgiToolParam(value = "Optional branch name to push. If omitted, pushes current active branch.", required = false) String branch,
            @AgiToolParam(value = "Whether to force-push (allow non-fast-forward updates). Defaults to false.", required = false) Boolean force) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitBranch activeBranch = getActiveBranch(client, monitor);
            String branchName = (branch != null && !branch.isBlank()) ? branch.trim() : (activeBranch != null ? activeBranch.getName() : "main");
            String remoteName = resolveRemoteName(client, remote, activeBranch);

            GitRemoteConfig remoteCfg = client.getRemote(remoteName, monitor);
            List<String> fetchRefSpecs = resolveFetchRefSpecs(remoteCfg, remoteName);
            boolean forcePush = force != null && force;
            String prefix = forcePush ? "+" : "";
            List<String> pushRefSpecs = Collections.singletonList(prefix + "refs/heads/" + branchName + ":refs/heads/" + branchName);

            log("Pushing branch '" + branchName + "' to remote '" + remoteName + "' in " + repoRoot.getName() + (forcePush ? " (FORCE)" : ""));
            GitPushResult pushResult = client.push(remoteName, pushRefSpecs, fetchRefSpecs, monitor);

            StringBuilder sb = new StringBuilder();
            sb.append("### Git Push Successful\n\n");
            sb.append("- **Repository**: ").append(repoRoot.getName()).append("\n");
            sb.append("- **Branch**: `").append(branchName).append("`\n");
            sb.append("- **Remote**: `").append(remoteName).append("`\n");
            if (forcePush) {
                sb.append("- **Force Push**: true\n");
            }

            if (!pushResult.getRemoteRepositoryUpdates().isEmpty()) {
                sb.append("- **Remote Updates**: ").append(pushResult.getRemoteRepositoryUpdates().keySet()).append("\n");
            }
            if (!pushResult.getLocalRepositoryUpdates().isEmpty()) {
                sb.append("- **Local Tracking Updates**: ").append(pushResult.getLocalRepositoryUpdates().keySet()).append("\n");
            }

            refreshVfs(repoRoot);
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
            @AgiToolParam(value = "Whether to exclude merge commits. Defaults to false.", required = false) Boolean noMerges,
            @AgiToolParam(value = "Optional regex or keyword filter for commit messages.", required = false) String grep) throws Exception {

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
            if (grep != null && !grep.isBlank()) {
                crit.setMessage(grep.trim());
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
            @AgiToolParam(value = "Optional target branch or revision hash (e.g. 'feat/my-branch', 'HEAD'). Omit to compare against the local working copy.", required = false) String targetRevision,
            @AgiToolParam(value = "Optional specific file or folder path to limit the diff to.", required = false, rendererId = "path") String filePath,
            @AgiToolParam(value = "If true, returns only the list of modified/added/deleted file paths instead of the full patch text. Defaults to false.", required = false) Boolean summaryOnly) throws Exception {

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            File[] files = (filePath != null && !filePath.isBlank())
                    ? new File[]{resolveRepoFile(repoRoot, filePath)}
                    : new File[]{repoRoot};

            boolean isTargetWorkingCopy = isWorkingCopyAlias(targetRevision);
            boolean isBaseWorkingCopy = isWorkingCopyAlias(baseRevision);

            String baseRev = isBaseWorkingCopy ? WORKING_TREE : baseRevision.trim();
            String targetRev = isTargetWorkingCopy ? WORKING_TREE : targetRevision.trim();

            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            client.exportDiff(files, baseRev, targetRev, baos, monitor);

            String diff = baos.toString(StandardCharsets.UTF_8).trim();
            String targetLabel = (targetRevision != null && !targetRevision.isBlank()) ? targetRevision.trim() : "WORKING_COPY";
            if (diff.isBlank()) {
                return "No differences found between " + baseRevision + " and " + targetLabel + (filePath != null ? " for " + filePath : "");
            }

            if (summaryOnly != null && summaryOnly) {
                return parseDiffSummary(diff, repoRoot.getName(), baseRevision, targetLabel);
            }

            return diff;
        } finally {
            client.release();
        }
    }

    /**
     * Parses unified diff text into a concise Markdown summary table of changed files and their change types.
     *
     * @param diff The raw unified diff output.
     * @param repoName The repository name for display.
     * @param baseRev The base revision.
     * @param targetRev The target revision.
     * @return Markdown summary table.
     */
    private static String parseDiffSummary(String diff, String repoName, String baseRev, String targetRev) {
        StringBuilder sb = new StringBuilder();
        sb.append("### Git Diff Summary: ").append(repoName).append(" [").append(baseRev).append("...").append(targetRev).append("]\n\n");
        sb.append("| Change | File |\n");
        sb.append("| :--- | :--- |\n");

        String[] lines = diff.split("\n");
        String currentFile = null;
        String changeType = "[M]";
        int count = 0;

        for (String line : lines) {
            if (line.startsWith("diff --git ")) {
                if (currentFile != null) {
                    sb.append("| `").append(changeType).append("` | `").append(currentFile).append("` |\n");
                    count++;
                }
                changeType = "[M]";
                String[] parts = line.split(" ");
                if (parts.length >= 4) {
                    currentFile = parts[3].startsWith("b/") ? parts[3].substring(2) : parts[3];
                }
            } else if (line.startsWith("new file mode ")) {
                changeType = "[A]";
            } else if (line.startsWith("deleted file mode ")) {
                changeType = "[D]";
            }
        }
        if (currentFile != null) {
            sb.append("| `").append(changeType).append("` | `").append(currentFile).append("` |\n");
            count++;
        }

        if (count == 0) {
            return "No changed files found in diff between " + baseRev + " and " + targetRev;
        }
        return "- **Total Changed Files**: " + count + "\n\n" + sb.toString().trim();
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
     * {@inheritDoc}
     * <p>Merges a branch or revision into the current active branch using NetBeans GitClient.</p>
     */
    @Override
    @AgiTool("Merges a branch or revision into the current active branch.")
    public String gitMerge(
            @AgiToolParam(value = "Path of the repository or project directory.", rendererId = "path") String repoPath,
            @AgiToolParam(value = "The branch name or revision hash to merge into the active branch.") String branchOrRevision,
            @AgiToolParam(value = "Fast-forward merge policy. Defaults to FAST_FORWARD.", required = false) FastForwardPolicy fastForwardPolicy) throws Exception {

        if (branchOrRevision == null || branchOrRevision.isBlank()) {
            throw new AgiToolException("Branch name or revision cannot be empty.");
        }

        File repoRoot = requireRepoRoot(repoPath);

        GitClient client = Git.getInstance().getClient(repoRoot);
        ToolProgressMonitor monitor = new ToolProgressMonitor();
        try {
            GitBranch activeBranch = getActiveBranch(client, monitor);
            String target = branchOrRevision.trim();
            String activeName = activeBranch != null ? activeBranch.getName() : "HEAD";
            FastForwardOption ff;
            if (fastForwardPolicy == null) {
                ff = FastForwardOption.FAST_FORWARD;
            } else {
                ff = switch (fastForwardPolicy) {
                    case FAST_FORWARD_ONLY -> FastForwardOption.FAST_FORWARD_ONLY;
                    case NO_FAST_FORWARD -> FastForwardOption.NO_FAST_FORWARD;
                    default -> FastForwardOption.FAST_FORWARD;
                };
            }

            log("Merging '" + target + "' (" + ff + ") into " + activeName + " in " + repoRoot.getName());
            GitMergeResult mergeResult = client.merge(target, ff, monitor);

            StringBuilder sb = new StringBuilder();
            sb.append("### Git Merge Result: ").append(repoRoot.getName()).append("\n\n");
            sb.append("- **Active Branch**: `").append(activeName).append("`\n");
            sb.append("- **Merged Revision/Branch**: `").append(target).append("`\n");
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
                sb.append("\nResolve the conflict markers (`<<<<<<<` / `=======` / `>>>>>>>`) in these files, stage them with `gitAdd`, and finalize with `gitCommit`.\n");
            }

            refreshVfs(repoRoot);
            log("Merge of " + target + " completed with status: " + mergeResult.getMergeStatus());
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
            refreshVfs(repoRoot);
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
            refreshVfs(repoRoot);
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
     * Checks if a given directory path is the root of a Git repository.
     *
     * @param path The directory path to check.
     * @return true if the directory is a repository root.
     */
    public boolean isRepoRoot(String path) {
        if (path == null || path.isBlank()) {
            return false;
        }
        File target = FileUtil.normalizeFile(new File(path));
        if (!target.exists() || !target.isDirectory()) {
            return false;
        }
        File repoRoot = findRepoRoot(target);
        return repoRoot != null && repoRoot.equals(target);
    }

    /**
     * Builds a structured Markdown overview of a repository including active branch,
     * upstream tracking, configured remotes, working tree modification status, and recent commits.
     * Polymorphically supports Git, Subversion, Mercurial, and any NetBeans VersioningSystem.
     *
     * @param repoPath Path of the repository or project directory.
     * @return Structured Markdown overview of the repository state, or null if unmanaged.
     * @throws Exception if repository querying fails.
     */
    public String getRepositoryOverview(String repoPath) throws Exception {
        File target = resolveFileOrDirectory(repoPath);
        VersioningSystem vs = VersioningSupport.getOwner(target);
        if (vs == null) {
            return null;
        }

        File repoRoot = vs.getTopmostManagedAncestor(target);
        if (repoRoot == null) {
            repoRoot = target;
        }

        String vcsName = (String) vs.getProperty(VersioningSystem.PROP_DISPLAY_NAME);
        if (vcsName == null || vcsName.isBlank()) {
            vcsName = "VCS";
        }

        StringBuilder sb = new StringBuilder();

        // 1. Repo-Specific Status
        if ("Git".equalsIgnoreCase(vcsName) || vcsName.toLowerCase().contains("git")) {
            GitClient client = Git.getInstance().getClient(repoRoot);
            ToolProgressMonitor monitor = new ToolProgressMonitor();
            try {
                GitBranch active = getActiveBranch(client, monitor);
                String activeBranch = active != null ? active.getName() : "HEAD";
                String tracking = (active != null && active.getTrackedBranch() != null)
                        ? active.getTrackedBranch().getName()
                        : null;

                sb.append("## Live Version Control: Git [Branch: `").append(activeBranch).append("`");
                if (tracking != null) {
                    sb.append(" (tracks `").append(tracking).append("`)");
                }
                sb.append("]\n");
                sb.append("> [!NOTE]\n");
                sb.append("> This is the live repository status generated JIT for this turn. Do not call `VCS.gitStatus` to re-query.\n\n");

                Map<String, GitRemoteConfig> remotes = client.getRemotes(monitor);
                if (!remotes.isEmpty()) {
                    sb.append("  - **Remotes**:\n");
                    for (GitRemoteConfig rc : remotes.values()) {
                        String uri = rc.getUris().isEmpty() ? "-" : rc.getUris().get(0);
                        sb.append("    * `").append(rc.getRemoteName()).append("`: ").append(uri).append("\n");
                    }
                }

                Map<File, GitStatus> statusMap = client.getStatus(new File[]{repoRoot}, monitor);
                int modifiedCount = 0;
                StringBuilder table = new StringBuilder();
                table.append("  | Status (Index / Working Tree) | File |\n");
                table.append("  | :--- | :--- |\n");

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
                    table.append("  | `").append(indexWc).append(" / ").append(headWc).append("` | `").append(relativePath).append("` |\n");
                    modifiedCount++;
                }

                if (modifiedCount == 0) {
                    sb.append("  - **Working Tree**: Clean (no uncommitted changes)\n");
                } else {
                    sb.append("  - **Working Tree**: ").append(modifiedCount).append(" modified/untracked files\n\n");
                    sb.append(table).append("\n");
                }
            } finally {
                client.release();
            }
        } else {
            // Subversion / Mercurial / Generic NetBeans VCS
            sb.append("## Live Version Control: ").append(vcsName).append("\n");
            sb.append("> [!NOTE]\n");
            sb.append("> This is the live repository status generated JIT for this turn.\n\n");
            sb.append("  - **Working Copy Root**: `").append(repoRoot.getAbsolutePath()).append("`\n");
        }

        // 2. Repo-Tech Agnostic: Recent Commits (works for Git, SVN, Mercurial via VCSHistoryProvider)
        List<HistoryEntry> recentCommits = getHistory(repoRoot.getAbsolutePath(), 5);
        if (!recentCommits.isEmpty()) {
            sb.append("  ### Recent Commits\n");
            String table = HistoryEntry.toMarkdownTable(repoRoot.getName(), recentCommits);
            if (table != null) {
                sb.append("  ").append(table.replace("\n", "\n  ")).append("\n");
            }
        }

        return sb.toString().trim();
    }


    /**
     * Resolves a GitUser from parameters, repository client defaults, or host system username.
     *
     * @param client The active GitClient (optional).
     * @param authorName Optional provided author name.
     * @param authorEmail Optional provided author email.
     * @return Valid GitUser instance.
     */
    private GitUser resolveGitUser(GitClient client, String authorName, String authorEmail) {
        if (authorName != null && !authorName.isBlank() && authorEmail != null && !authorEmail.isBlank()) {
            return new GitUser(authorName.trim(), authorEmail.trim());
        }
        if (client != null) {
            try {
                GitUser defaultUser = client.getUser();
                if (defaultUser != null && defaultUser.getName() != null && defaultUser.getEmailAddress() != null) {
                    return defaultUser;
                }
            } catch (Exception ex) {
                log.warn("Could not retrieve default Git user from GitClient: {}", ex.getMessage());
            }
        }
        return getGlobalGitUser();
    }

    /**
     * Resolves the global Git user from ~/.gitconfig or NetBeans system configuration.
     *
     * @return The configured GitUser.
     */
    private GitUser getGlobalGitUser() {
        File home = new File(System.getProperty("user.home"));
        GitClient client = null;
        try {
            client = Git.getInstance().getClient(home);
            GitUser user = client.getUser();
            if (user != null && user.getName() != null && !user.getName().isBlank()) {
                return user;
            }
        } catch (Exception ignored) {
        } finally {
            if (client != null) {
                client.release();
            }
        }
        String name = System.getProperty("user.name", "Anahata");
        return new GitUser(name, name + "@local");
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
        Git.getInstance().getFileStatusCache().refreshAllRoots(file);
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
