/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.vcs;

import com.intellij.history.core.LocalHistoryFacade;
import com.intellij.history.core.changes.ChangeSet;
import com.intellij.history.integration.LocalHistoryImpl;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vcs.AbstractVcs;
import com.intellij.openapi.vcs.FilePath;
import com.intellij.openapi.vcs.FileStatus;
import com.intellij.openapi.vcs.ProjectLevelVcsManager;
import com.intellij.openapi.vcs.changes.Change;
import com.intellij.openapi.vcs.changes.ChangeListManager;
import com.intellij.openapi.vcs.changes.ContentRevision;
import com.intellij.openapi.vcs.changes.LocalChangeList;
import com.intellij.openapi.vcs.history.VcsCachingHistory;
import com.intellij.openapi.vcs.history.VcsFileRevision;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.vcsUtil.VcsUtil;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.vcs.HistoryEntry;
import uno.anahata.asi.agi.resource.vcs.VcsDiff;
import uno.anahata.asi.agi.resource.vcs.VcsFileStatus;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolPermission;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import git4idea.commands.Git;
import git4idea.commands.GitCommand;
import git4idea.commands.GitCommandResult;
import git4idea.commands.GitLineHandler;
import git4idea.repo.GitRemote;
import git4idea.repo.GitRepository;
import git4idea.repo.GitRepositoryManager;
import java.util.Map;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.vcs.FastForwardPolicy;
import uno.anahata.asi.internal.AnahataDiffUtils;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.toolkit.vcs.AbstractVCS;

/**
 * A toolkit for inspecting version-control status through IntelliJ's generic VCS layer.
 * <p>
 * Implements the universal {@link AbstractVCS} contract for IntelliJ IDEA, providing
 * provider-agnostic VCS status, unified diffs, local history inspection, and working copy revert.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Universal toolkit for Version Control Systems and Local History.")
public class VCS extends AbstractVCS {

    /**
     * Constructs the Vcs toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public VCS() {
    }

    /**
     * {@inheritDoc}
     * <p>Contributes system instructions describing how to access IntelliJ's native Git4Idea engine for advanced Git operations.</p>
     */
    @Override
    public List<String> getSystemInstructions() {
        return List.of("""
        ### IntelliJ VCS & Git4Idea Direct Access:
        For advanced Git operations not exposed as discrete `@AgiTool` methods (such as interactive rebase, cherry-pick, stash, hard reset, or tag management), you can use `compileAndExecute` to interact directly with IntelliJ's `GitRepositoryManager` and `Git` commands:
        ```java
        Project project = ProjectManager.getInstance().getOpenProjects()[0];
        GitRepositoryManager mgr = GitRepositoryManager.getInstance(project);
        GitRepository repo = mgr.getRepositories().get(0);
        GitLineHandler handler = new GitLineHandler(project, repo.getRoot(), GitCommand.STASH);
        handler.addParameters("pop");
        GitCommandResult result = Git.getInstance().runCommand(handler);
        ```
        This execution pipeline is pre-configured with IntelliJ's SSH keys, credential helpers, and Virtual File System (VFS) synchronization.
        """);
    }

    /**
     * {@inheritDoc}
     * <p>Populates the RAG message with application-wide Git user configuration (name and email).</p>
     */
    @Override
    public void populateMessage(RagMessage message) {
        String userName = getGitConfig("user.name");
        String userEmail = getGitConfig("user.email");
        if (userName != null || userEmail != null) {
            StringBuilder sb = new StringBuilder();
            sb.append("## Global Git User Configuration\n");
            if (userName != null) {
                sb.append("- **Name**: ").append(userName).append("\n");
            }
            if (userEmail != null) {
                sb.append("- **Email**: ").append(userEmail).append("\n");
            }
            message.addTextPart(sb.toString().trim());
        }
    }

    /**
     * Reports the version-control status of all open projects: local changes grouped by change
     * list, plus unversioned files.
     *
     * @return a Markdown status report.
     */
    @AgiTool("Reports version-control status (changed/added/deleted and unversioned files) for open projects.")
    public String getVcsStatus() {
        StringBuilder sb = new StringBuilder();
        boolean any = false;
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            ChangeListManager manager = ChangeListManager.getInstance(project);

            StringBuilder projectReport = new StringBuilder();
            for (LocalChangeList changeList : manager.getChangeLists()) {
                Collection<Change> changes = changeList.getChanges();
                if (changes.isEmpty()) {
                    continue;
                }
                projectReport.append("  ### Change List: ").append(changeList.getName()).append("\n");
                for (Change change : changes) {
                    projectReport.append("    - [").append(statusOf(change)).append("] `").append(pathOf(change)).append("`\n");
                }
            }

            List<FilePath> unversioned = manager.getUnversionedFilesPaths();
            if (!unversioned.isEmpty()) {
                projectReport.append("  ### Unversioned\n");
                for (FilePath path : unversioned) {
                    projectReport.append("    - `").append(path.getPath()).append("`\n");
                }
            }

            if (projectReport.length() > 0) {
                any = true;
                sb.append("## VCS Status: ").append(project.getName()).append("\n").append(projectReport);
            }
        }
        return any ? sb.toString() : "No local changes or unversioned files in any open project.";
    }

    /**
     * Maps a change to a short status token.
     *
     * @param change the change.
     * @return {@code ADDED}, {@code DELETED}, {@code MOVED} or {@code MODIFIED}.
     */
    private static String statusOf(Change change) {
        return switch (change.getType()) {
            case NEW -> "ADDED";
            case DELETED -> "DELETED";
            case MOVED -> "MOVED";
            default -> "MODIFIED";
        };
    }

    /**
     * Resolves the best available path for a change (after-revision, else before-revision, else
     * the virtual file).
     *
     * @param change the change.
     * @return the file path, or {@code "?"} if none can be resolved.
     */
    private static String pathOf(Change change) {
        if (change.getAfterRevision() != null) {
            return change.getAfterRevision().getFile().getPath();
        }
        if (change.getBeforeRevision() != null) {
            return change.getBeforeRevision().getFile().getPath();
        }
        VirtualFile file = change.getVirtualFile();
        return file != null ? file.getPath() : "?";
    }

    /**
     * Generates a unified diff for a file against its repository base or a specific revision.
     *
     * @param filePath The absolute path of the file to inspect.
     * @param revision Optional revision identifier (e.g. commit hash or revision number). If omitted, diffs against repository base.
     * @return A {@link VcsDiff} DTO containing diff text and status classification.
     * @throws Exception if diff generation fails.
     */
    @AgiTool(value = "Generates a unified diff for a file against repository base or a specific revision using IntelliJ APIs.", permission = ToolPermission.APPROVE_ALWAYS)
    public VcsDiff getDiff(
            @AgiToolParam(value = "The absolute path of the file to inspect.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Optional revision identifier (e.g. commit hash or revision number). If omitted, diffs against repository base.", required = false) String revision) throws Exception {

        File file = resolveFile(filePath);
        VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(file);
        Project project = (vf != null) ? ProjectUtils.findHostProject(vf) : null;
        if (project == null || project.isDisposed()) {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length > 0) {
                project = openProjects[0];
            }
        }

        ProjectLevelVcsManager vcsMgr = (project != null) ? ProjectLevelVcsManager.getInstance(project) : null;
        AbstractVcs vcs = (vcsMgr != null && vf != null) ? vcsMgr.getVcsFor(vf) : null;

        if (revision != null && !revision.isBlank()) {
            if (vcs == null) {
                throw new AgiToolException("File is not managed by any Version Control System: " + filePath);
            }
            FilePath fp = VcsUtil.getFilePath(vf);
            List<VcsFileRevision> revisions = VcsCachingHistory.collect(vcs, fp, null);
            VcsFileRevision targetRev = null;
            if (revisions != null) {
                for (VcsFileRevision r : revisions) {
                    if (r.getRevisionNumber().asString().equalsIgnoreCase(revision.trim())
                            || r.getRevisionNumber().asString().startsWith(revision.trim())) {
                        targetRev = r;
                        break;
                    }
                }
            }
            if (targetRev == null) {
                throw new AgiToolException("Revision '" + revision + "' not found in history for: " + filePath);
            }
            byte[] baseBytes = targetRev.loadContent();
            String baseContent = (baseBytes != null) ? new String(baseBytes, StandardCharsets.UTF_8) : "";
            String currentContent = Files.readString(file.toPath(), StandardCharsets.UTF_8);
            String diff = AnahataDiffUtils.generateUnifiedDiff(file.getName(), baseContent, currentContent);
            if (diff.isBlank()) {
                return VcsDiff.builder()
                        .filePath(file.getAbsolutePath())
                        .status(VcsFileStatus.CLEAN)
                        .baseRevision(revision.trim())
                        .targetRevision("WORKING_COPY")
                        .build();
            } else {
                return VcsDiff.builder()
                        .filePath(file.getAbsolutePath())
                        .status(VcsFileStatus.MODIFIED)
                        .baseRevision(revision.trim())
                        .targetRevision("WORKING_COPY")
                        .diff(diff)
                        .build();
            }
        }

        if (project == null || vf == null || vcs == null) {
            return VcsDiff.builder()
                    .filePath(file.getAbsolutePath())
                    .status(VcsFileStatus.UNSUPPORTED)
                    .baseRevision("HEAD")
                    .targetRevision("WORKING_COPY")
                    .build();
        }

        ChangeListManager clm = ChangeListManager.getInstance(project);
        if (clm.isUnversioned(vf)) {
            return VcsDiff.builder()
                    .filePath(file.getAbsolutePath())
                    .status(VcsFileStatus.UNTRACKED)
                    .baseRevision("HEAD")
                    .targetRevision("WORKING_COPY")
                    .build();
        }

        Change change = clm.getChange(vf);
        if (change == null) {
            return VcsDiff.builder()
                    .filePath(file.getAbsolutePath())
                    .status(VcsFileStatus.CLEAN)
                    .baseRevision("HEAD")
                    .targetRevision("WORKING_COPY")
                    .build();
        }

        FileStatus fileStatus = change.getFileStatus();
        VcsFileStatus vcsStatus;
        if (fileStatus == FileStatus.MERGED_WITH_CONFLICTS || fileStatus == FileStatus.MERGE) {
            vcsStatus = VcsFileStatus.CONFLICTED;
        } else if (change.getType() == Change.Type.NEW) {
            vcsStatus = VcsFileStatus.NEW;
        } else if (change.getType() == Change.Type.DELETED) {
            vcsStatus = VcsFileStatus.DELETED;
        } else {
            vcsStatus = VcsFileStatus.MODIFIED;
        }

        if (vcsStatus == VcsFileStatus.NEW) {
            return VcsDiff.builder()
                    .filePath(file.getAbsolutePath())
                    .status(VcsFileStatus.NEW)
                    .baseRevision("HEAD")
                    .targetRevision("WORKING_COPY")
                    .build();
        }

        ContentRevision beforeRev = change.getBeforeRevision();
        ContentRevision afterRev = change.getAfterRevision();
        String beforeContent = (beforeRev != null && beforeRev.getContent() != null) ? beforeRev.getContent() : "";
        String afterContent = (afterRev != null && afterRev.getContent() != null) ? afterRev.getContent() : (file.exists() ? Files.readString(file.toPath(), StandardCharsets.UTF_8) : "");
        String baseRev = (beforeRev != null && beforeRev.getRevisionNumber() != null) ? beforeRev.getRevisionNumber().asString() : "HEAD";
        String diff = AnahataDiffUtils.generateUnifiedDiff(file.getName(), beforeContent, afterContent);

        if (diff.isBlank()) {
            return VcsDiff.builder()
                    .filePath(file.getAbsolutePath())
                    .status(vcsStatus == VcsFileStatus.CONFLICTED ? VcsFileStatus.CONFLICTED : VcsFileStatus.CLEAN)
                    .baseRevision(baseRev)
                    .targetRevision("WORKING_COPY")
                    .build();
        }

        return VcsDiff.builder()
                .filePath(file.getAbsolutePath())
                .status(vcsStatus)
                .baseRevision(baseRev)
                .targetRevision("WORKING_COPY")
                .diff(diff)
                .build();
    }

    /**
     * Queries the unified chronological history of a file, combining VCS commits and IntelliJ Local History.
     *
     * @param filePath The absolute path of the file.
     * @param maxEntries Maximum number of history entries to return. Defaults to 10.
     * @return A list of {@link HistoryEntry} DTOs sorted in reverse chronological order.
     * @throws Exception if querying history fails.
     */
    @AgiTool(value = "Queries the unified chronological history of a file, combining VCS commits and IntelliJ Local History.", permission = ToolPermission.APPROVE_ALWAYS)
    public List<HistoryEntry> getHistory(
            @AgiToolParam(value = "The absolute path of the file.", rendererId = "path") String filePath,
            @AgiToolParam(value = "Maximum number of history entries to return. Defaults to 10.", required = false) Integer maxEntries) throws Exception {

        File file = resolveFile(filePath);
        VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(file);
        Project project = (vf != null) ? ProjectUtils.findHostProject(vf) : null;
        if (project == null || project.isDisposed()) {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length > 0) {
                project = openProjects[0];
            }
        }

        int limit = (maxEntries != null && maxEntries > 0) ? maxEntries : 10;
        List<HistoryEntry> history = new ArrayList<>();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        if (project != null && vf != null) {
            ProjectLevelVcsManager vcsMgr = ProjectLevelVcsManager.getInstance(project);
            AbstractVcs vcs = vcsMgr.getVcsFor(vf);
            if (vcs != null) {
                String vcsName = vcs.getDisplayName();
                if (vcsName == null || vcsName.isBlank()) {
                    vcsName = vcs.getName();
                }
                try {
                    FilePath fp = VcsUtil.getFilePath(vf);
                    List<VcsFileRevision> revisions = VcsCachingHistory.collect(vcs, fp, null);
                    if (revisions != null) {
                        for (VcsFileRevision rev : revisions) {
                            Date date = rev.getRevisionDate();
                            long ts = (date != null) ? date.getTime() : 0L;
                            String dateFormatted = (date != null) ? sdf.format(date) : "";
                            String revStr = (rev.getRevisionNumber() != null) ? rev.getRevisionNumber().asString() : "";
                            if (revStr.length() > 7) {
                                revStr = revStr.substring(0, 7);
                            }
                            String author = (rev.getAuthor() != null) ? rev.getAuthor() : "";
                            String msg = (rev.getCommitMessage() != null) ? rev.getCommitMessage().replace("\n", " ").trim() : "";
                            history.add(new HistoryEntry(ts, dateFormatted, vcsName, revStr, author, msg));
                        }
                    }
                } catch (Throwable t) {
                    log.debug("Error collecting VCS history for {}: {}", filePath, t.getMessage());
                }
            }
        }

        try {
            LocalHistoryImpl lhi = LocalHistoryImpl.getInstanceImpl();
            if (lhi != null) {
                LocalHistoryFacade facade = lhi.getFacade();
                if (facade != null) {
                    Iterable<ChangeSet> changeSets = facade.getChanges$intellij_platform_lvcs_impl();
                    String targetPath = (vf != null) ? vf.getPath() : file.getAbsolutePath();
                    for (ChangeSet cs : changeSets) {
                        List<String> affected = cs.getAffectedPaths();
                        if (affected != null && affected.contains(targetPath)) {
                            long ts = cs.getTimestamp();
                            Date date = new Date(ts);
                            String label = cs.getLabel();
                            String name = cs.getName();
                            String msg = (label != null && !label.isBlank()) ? label.trim() : ((name != null) ? name.trim() : "");
                            history.add(new HistoryEntry(ts, sdf.format(date), "Local History", "Local", "", msg));
                        }
                    }
                }
            }
        } catch (Throwable t) {
            log.debug("Error collecting Local History for {}: {}", filePath, t.getMessage());
        }

        Collections.sort(history);
        return history.size() <= limit ? history : new ArrayList<>(history.subList(0, limit));
    }

    /**
     * Gets the Version Control metadata and repository root for a file or directory.
     *
     * @param path The absolute path of the file or directory to inspect.
     * @return Formatted Markdown summary of VCS metadata.
     * @throws Exception if resolution fails.
     */
    @Override
    @AgiTool(value = "Gets the Version Control metadata and repository root for a file or directory.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getInfo(
            @AgiToolParam(value = "The absolute path of the file or directory.", rendererId = "path") String path) throws Exception {
        File file = resolveFile(path);
        VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(file);
        if (vf == null) {
            return "Path is not accessible in VFS: " + path;
        }
        Project project = ProjectUtils.findHostProject(vf);
        if (project == null || project.isDisposed()) {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length > 0) {
                project = openProjects[0];
            }
        }
        if (project == null) {
            return "No open project found for: " + path;
        }
        ProjectLevelVcsManager vcsMgr = ProjectLevelVcsManager.getInstance(project);
        AbstractVcs vcs = vcsMgr.getVcsFor(vf);
        if (vcs == null) {
            return "Path is not under Version Control: " + path;
        }
        VirtualFile vcsRoot = vcsMgr.getVcsRootFor(vf);
        StringBuilder sb = new StringBuilder();
        sb.append("### VCS Information: ").append(file.getName()).append("\n\n");
        sb.append("- **VCS System**: ").append(vcs.getDisplayName()).append("\n");
        sb.append("- **Repository Root**: ").append(vcsRoot != null ? vcsRoot.getPath() : "None").append("\n");
        sb.append("- **File Path**: ").append(file.getAbsolutePath()).append("\n");
        sb.append("- **Is File**: ").append(file.isFile()).append("\n");
        return sb.toString().trim();
    }

    /**
     * Discards unstaged modifications in a file, reverting it to the repository pristine base revision.
     *
     * @param filePath The absolute path of the file to revert.
     * @return Confirmation message of the revert operation.
     * @throws Exception if revert fails.
     */
    @Override
    @AgiTool("Discards unstaged modifications in a file, reverting it to the repository pristine base revision.")
    public String revert(
            @AgiToolParam(value = "The absolute path of the file to revert.", rendererId = "path") String filePath) throws Exception {
        File file = resolveFile(filePath);
        VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(file);
        if (vf == null) {
            throw new AgiToolException("VirtualFile not found for: " + filePath);
        }
        Project project = ProjectUtils.findHostProject(vf);
        if (project == null || project.isDisposed()) {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length > 0) {
                project = openProjects[0];
            }
        }
        if (project == null) {
            throw new AgiToolException("No open project found for: " + filePath);
        }
        ChangeListManager clm = ChangeListManager.getInstance(project);
        Change change = clm.getChange(vf);
        if (change == null) {
            return "File has no uncommitted changes to revert: " + file.getName();
        }
        ContentRevision before = change.getBeforeRevision();
        if (before == null) {
            throw new AgiToolException("Cannot revert newly added file: " + file.getName());
        }
        String baseContent = before.getContent();
        if (baseContent != null) {
            Files.writeString(file.toPath(), baseContent, StandardCharsets.UTF_8);
            vf.refresh(false, false);
            return "Successfully reverted " + file.getName() + " to repository base.";
        }
        throw new AgiToolException("Could not retrieve base content to revert: " + file.getName());
    }

    /**
     * Checks if a given directory path is the root of a repository.
     *
     * @param path The directory path to check.
     * @return true if the directory is a repository root.
     */
    @Override
    public boolean isRepoRoot(String path) {
        if (path == null || path.isBlank()) {
            return false;
        }
        File target = new File(path).toPath().normalize().toFile();
        if (!target.exists() || !target.isDirectory()) {
            return false;
        }
        File dotGit = new File(target, ".git");
        if (dotGit.exists()) {
            return true;
        }
        VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(target);
        if (vf == null) {
            return false;
        }
        Project project = ProjectUtils.findHostProject(vf);
        if (project == null || project.isDisposed()) {
            Project[] open = ProjectManager.getInstance().getOpenProjects();
            if (open.length > 0) {
                project = open[0];
            }
        }
        if (project == null) {
            return false;
        }
        VirtualFile root = ProjectLevelVcsManager.getInstance(project).getVcsRootFor(vf);
        return root != null && root.equals(vf);
    }

    /**
     * Builds a structured Markdown overview of a repository including branch, tracking,
     * remotes, working tree status, and recent commits.
     *
     * @param repoPath Path of the repository or project directory.
     * @return Structured Markdown overview of the repository state, or null if unmanaged.
     * @throws Exception if repository querying fails.
     */
    @Override
    public String getRepositoryOverview(String repoPath) throws Exception {
        File target = resolveFileOrDirectory(repoPath);
        VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(target);
        if (vf == null) {
            return null;
        }
        Project project = ProjectUtils.findHostProject(vf);
        if (project == null || project.isDisposed()) {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length > 0) {
                project = openProjects[0];
            }
        }
        if (project == null) {
            return null;
        }
        ProjectLevelVcsManager vcsMgr = ProjectLevelVcsManager.getInstance(project);
        AbstractVcs vcs = vcsMgr.getVcsFor(vf);
        if (vcs == null) {
            return null;
        }

        StringBuilder sb = new StringBuilder();
        String vcsName = vcs.getDisplayName();

        if ("Git".equalsIgnoreCase(vcsName)) {
            GitRepositoryManager gitMgr = GitRepositoryManager.getInstance(project);
            GitRepository repo = gitMgr.getRepositoryForFileQuick(vf);
            if (repo != null) {
                String currentBranch = repo.getCurrentBranchName();
                String trackedBranch = (repo.getCurrentBranch() != null && repo.getBranchTrackInfo(repo.getCurrentBranch().getName()) != null)
                        ? repo.getBranchTrackInfo(repo.getCurrentBranch().getName()).getRemoteBranch().getNameForLocalOperations()
                        : null;

                sb.append("## Live Version Control: Git [Branch: `").append(currentBranch != null ? currentBranch : "HEAD").append("`");
                if (trackedBranch != null) {
                    sb.append(" (tracks `").append(trackedBranch).append("`)");
                }
                sb.append("]\n");
                sb.append("> [!NOTE]\n");
                sb.append("> This is the live repository status generated JIT for this turn. Do not call `VCS.gitStatus` to re-query.\n\n");

                Collection<GitRemote> remotes = repo.getRemotes();
                if (!remotes.isEmpty()) {
                    sb.append("  - **Remotes**:\n");
                    for (GitRemote r : remotes) {
                        String url = r.getUrls().isEmpty() ? "-" : r.getUrls().get(0);
                        sb.append("    * `").append(r.getName()).append("`: ").append(url).append("\n");
                    }
                }

                GitLineHandler handler = new GitLineHandler(project, repo.getRoot(), GitCommand.STATUS);
                handler.addParameters("--porcelain=v1");
                GitCommandResult statusRes = Git.getInstance().runCommand(handler);
                List<String> statusLines = statusRes.getOutput();
                int modifiedCount = 0;
                StringBuilder table = new StringBuilder();
                table.append("  | Status (Index / Working Tree) | File |\n");
                table.append("  | :--- | :--- |\n");

                for (String line : statusLines) {
                    if (line.length() >= 3) {
                        String code = line.substring(0, 2);
                        String f = line.substring(3).trim();
                        table.append("  | `").append(code).append("` | `").append(f).append("` |\n");
                        modifiedCount++;
                    }
                }

                if (modifiedCount == 0) {
                    sb.append("  - **Working Tree**: Clean (no uncommitted changes)\n");
                } else {
                    sb.append("  - **Working Tree**: ").append(modifiedCount).append(" modified/untracked files\n\n");
                    sb.append(table).append("\n");
                }
            } else {
                sb.append("## Live Version Control: ").append(vcsName).append("\n\n");
            }
        } else {
            sb.append("## Live Version Control: ").append(vcsName).append("\n");
            sb.append("> [!NOTE]\n");
            sb.append("> This is the live repository status generated JIT for this turn.\n\n");

            ChangeListManager clm = ChangeListManager.getInstance(project);
            Collection<Change> changes = clm.getChangesIn(vf);
            if (changes == null || changes.isEmpty()) {
                sb.append("  - **Working Tree**: Clean (no uncommitted changes)\n");
            } else {
                sb.append("  - **Working Tree**: ").append(changes.size()).append(" modified files\n\n");
                sb.append("  | Status | File |\n");
                sb.append("  | :--- | :--- |\n");
                for (Change c : changes) {
                    String rel = target.toPath().relativize(new File(pathOf(c)).toPath()).toString();
                    sb.append("  | `").append(statusOf(c)).append("` | `").append(rel).append("` |\n");
                }
            }
        }

        List<HistoryEntry> recentCommits = getHistory(repoPath, 5);
        if (!recentCommits.isEmpty()) {
            sb.append("\n  ### Recent Commits\n");
            String table = HistoryEntry.toMarkdownTable(target.getName(), recentCommits);
            if (table != null) {
                sb.append("  ").append(table.replace("\n", "\n  ")).append("\n");
            }
        }

        return sb.toString().trim();
    }

    /**
     * Resolves a validated, normalized File from an absolute path string.
     *
     * @param path The path string to resolve.
     * @return The normalized File.
     * @throws AgiToolException if the path is invalid or the file does not exist.
     */
    private static File resolveFile(String path) throws AgiToolException {
        if (path == null || path.isBlank()) {
            throw new AgiToolException("File path cannot be empty.");
        }
        File file = new File(path).toPath().normalize().toFile();
        if (!file.exists()) {
            throw new AgiToolException("File does not exist: " + path);
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
    private static File resolveDirectory(String path) throws AgiToolException {
        File file = resolveFileOrDirectory(path);
        if (!file.isDirectory()) {
            throw new AgiToolException("Path is not a directory: " + path);
        }
        return file;
    }

    /**
     * Resolves a validated, normalized File or directory from a path string.
     *
     * @param path The path string to resolve.
     * @return The normalized File.
     * @throws AgiToolException if the path is invalid or does not exist.
     */
    private static File resolveFileOrDirectory(String path) throws AgiToolException {
        if (path == null || path.isBlank()) {
            throw new AgiToolException("Path cannot be empty.");
        }
        File file = new File(path).toPath().normalize().toFile();
        if (!file.exists()) {
            throw new AgiToolException("Path does not exist: " + path);
        }
        return file;
    }

    /**
     * Resolves the target Git repository from a specified path or defaults to the active project's primary repository.
     *
     * @param repoPath Optional path of the repository, directory, or file.
     * @return The resolved {@link GitRepository}.
     * @throws AgiToolException if no repository can be found.
     */
    private GitRepository requireRepo(String repoPath) throws AgiToolException {
        if (repoPath != null && !repoPath.isBlank()) {
            File target = resolveFileOrDirectory(repoPath);
            VirtualFile vf = LocalFileSystem.getInstance().findFileByIoFile(target);
            if (vf != null) {
                Project p = ProjectUtils.findHostProject(vf);
                if (p != null) {
                    GitRepository r = GitRepositoryManager.getInstance(p).getRepositoryForFileQuick(vf);
                    if (r != null) {
                        return r;
                    }
                }
            }
            for (Project p : ProjectManager.getInstance().getOpenProjects()) {
                GitRepositoryManager mgr = GitRepositoryManager.getInstance(p);
                for (GitRepository r : mgr.getRepositories()) {
                    if (r.getRoot().getPath().equalsIgnoreCase(target.getAbsolutePath())) {
                        return r;
                    }
                }
            }
            throw new AgiToolException("Target path is not inside a recognized Git repository: " + repoPath);
        }

        Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
        for (Project p : openProjects) {
            List<GitRepository> repos = GitRepositoryManager.getInstance(p).getRepositories();
            if (!repos.isEmpty()) {
                return repos.get(0);
            }
        }
        throw new AgiToolException("No active Git repository found in any open project.");
    }

    /**
     * Resolves a Git configuration value for the active repository or global environment.
     *
     * @param key The configuration key (e.g., 'user.name', 'user.email').
     * @return The configured value, or null if unset.
     */
    private String getGitConfig(String key) {
        try {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length == 0) {
                return null;
            }
            Project project = openProjects[0];
            GitRepositoryManager mgr = GitRepositoryManager.getInstance(project);
            List<GitRepository> repos = mgr.getRepositories();
            VirtualFile root = repos.isEmpty() ? ProjectUtils.findVirtualFile(project.getBasePath()) : repos.get(0).getRoot();
            if (root == null) {
                return null;
            }
            GitLineHandler handler = new GitLineHandler(project, root, GitCommand.CONFIG);
            handler.addParameters(key);
            GitCommandResult result = Git.getInstance().runCommand(handler);
            if (result.success()) {
                String val = result.getOutputAsJoinedString().trim();
                return val.isEmpty() ? null : val;
            }
        } catch (Throwable t) {
            log.debug("Could not resolve git config for {}: {}", key, t.getMessage());
        }
        return null;
    }

    /**
     * Initializes a new Git repository in the specified directory and updates IntelliJ's repository mappings.
     *
     * @param directoryPath The absolute path of the directory to initialize. If omitted, initializes in the active project base directory.
     * @return Confirmation message of repository initialization.
     * @throws Exception if initialization fails.
     */
    @AgiTool("Initializes a new Git repository in the specified directory.")
    public String gitInit(
            @AgiToolParam(value = "The absolute path of the directory to initialize. If omitted, initializes in the active project base directory.", required = false, rendererId = "path") String directoryPath) throws Exception {

        File dir;
        Project project;
        if (directoryPath == null || directoryPath.isBlank()) {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length == 0 || openProjects[0].getBasePath() == null) {
                throw new AgiToolException("No open project base path available to initialize Git repository.");
            }
            project = openProjects[0];
            dir = new File(project.getBasePath());
        } else {
            dir = resolveDirectory(directoryPath);
            VirtualFile targetVf = LocalFileSystem.getInstance().findFileByIoFile(dir);
            project = (targetVf != null) ? ProjectUtils.findHostProject(targetVf) : null;
            if (project == null) {
                Project[] open = ProjectManager.getInstance().getOpenProjects();
                project = open.length > 0 ? open[0] : null;
            }
            if (project == null) {
                throw new AgiToolException("No open project available.");
            }
        }

        VirtualFile vf = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(dir);
        if (vf == null) {
            throw new AgiToolException("Could not find VirtualFile for directory: " + dir.getAbsolutePath());
        }

        GitCommandResult result = Git.getInstance().init(project, vf);
        if (!result.success()) {
            throw new AgiToolException("Failed to initialize Git repository: " + result.getErrorOutputAsJoinedString());
        }

        vf.refresh(false, true);
        GitRepositoryManager.getInstance(project).updateRepository(vf);

        log("Initialized Git repository in: " + dir.getAbsolutePath());
        return "Successfully initialized Git repository in: " + dir.getAbsolutePath();
    }

    /**
     * Retrieves the fine-grained Git status for a repository or specific file/directory path.
     *
     * @param path The path of the repository root, directory, or file to inspect. If omitted, checks the active project repository.
     * @return Formatted Markdown table containing branch, tracking information, and status of modified/added/untracked files.
     * @throws Exception if status retrieval fails.
     */
    @AgiTool(value = "Gets the Git status for a repository or file.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitStatus(
            @AgiToolParam(value = "The path of the repository, directory, or file to check. If omitted, checks the active project repository.", required = false, rendererId = "path") String path) throws Exception {

        GitRepository repo = requireRepo(path);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.STATUS);
        handler.addParameters("--porcelain=v1", "--branch");
        if (path != null && !path.isBlank()) {
            File target = resolveFileOrDirectory(path);
            if (!target.getAbsolutePath().equalsIgnoreCase(root.getPath())) {
                String rel = root.toNioPath().relativize(target.toPath()).toString();
                if (!rel.isEmpty()) {
                    handler.addParameters(rel);
                }
            }
        }

        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Failed to execute git status: " + result.getErrorOutputAsJoinedString());
        }

        List<String> outputLines = result.getOutput();
        String branchHeader = "HEAD";
        List<String> fileEntries = new ArrayList<>();

        for (String line : outputLines) {
            if (line.startsWith("## ")) {
                branchHeader = line.substring(3).trim();
            } else if (!line.isBlank()) {
                fileEntries.add(line);
            }
        }

        StringBuilder sb = new StringBuilder();
        sb.append("### Git Status: ").append(repo.getRoot().getName()).append(" [Branch: `").append(branchHeader).append("`]\n\n");

        if (fileEntries.isEmpty()) {
            sb.append("Working tree clean. No modified, added, or untracked files.\n");
        } else {
            sb.append("- **Total Modified/Untracked Files**: ").append(fileEntries.size()).append("\n\n");
            sb.append("| Status (Index / Working Tree) | File |\n");
            sb.append("| :--- | :--- |\n");
            for (String entry : fileEntries) {
                if (entry.length() >= 3) {
                    String statusCodes = entry.substring(0, 2);
                    String file = entry.substring(3).trim();
                    sb.append("| `").append(statusCodes).append("` | `").append(file).append("` |\n");
                }
            }
        }

        log("Retrieved Git status for " + repo.getRoot().getName() + " (" + fileEntries.size() + " modified/untracked files)");
        return sb.toString().trim();
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
            @AgiToolParam(value = "List of file paths to stage into the Git index.", rendererId = "path") List<String> filePaths) throws Exception {

        if (filePaths == null || filePaths.isEmpty()) {
            throw new AgiToolException("No files specified to stage.");
        }

        GitRepository repo = null;
        List<String> relativePaths = new ArrayList<>();
        for (String p : filePaths) {
            File f = resolveFile(p);
            if (repo == null) {
                repo = requireRepo(f.getAbsolutePath());
            }
            String rel = repo.getRoot().toNioPath().relativize(f.toPath()).toString();
            relativePaths.add(rel);
        }

        GitLineHandler handler = new GitLineHandler(repo.getProject(), repo.getRoot(), GitCommand.ADD);
        handler.addParameters(relativePaths);
        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Failed to stage files: " + result.getErrorOutputAsJoinedString());
        }

        repo.getRoot().refresh(false, true);
        repo.update();

        log("Staged " + relativePaths.size() + " files into Git index in: " + repo.getRoot().getName());
        return "Successfully staged " + relativePaths.size() + " file(s) into Git index in " + repo.getRoot().getName() + ".";
    }

    /**
     * Commits changes headlessly in a single shot (automatically staging files if specified).
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param filePaths Optional list of specific files to stage and commit. If omitted, commits all staged files in repository.
     * @param message The commit message.
     * @param authorName Optional author name. If omitted, uses repository gitconfig or system user.
     * @param authorEmail Optional author email. If omitted, uses repository gitconfig.
     * @return Details of the created commit revision.
     * @throws Exception if commit fails.
     */
    @AgiTool("Commits changes headlessly in a single shot (auto-stages files if specified).")
    public String gitCommit(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "Optional list of specific files to stage and commit. If omitted, commits all staged files.", required = false, rendererId = "path") List<String> filePaths,
            @AgiToolParam(value = "The commit message.") String message,
            @AgiToolParam(value = "Optional author name.", required = false) String authorName,
            @AgiToolParam(value = "Optional author email.", required = false) String authorEmail) throws Exception {

        if (message == null || message.isBlank()) {
            throw new AgiToolException("Commit message cannot be empty.");
        }

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        if (filePaths != null && !filePaths.isEmpty()) {
            gitAdd(filePaths);
        }

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.COMMIT);
        handler.addParameters("-m", message.trim());
        if (authorName != null && !authorName.isBlank() && authorEmail != null && !authorEmail.isBlank()) {
            handler.addParameters("--author=" + authorName.trim() + " <" + authorEmail.trim() + ">");
        }

        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git commit failed: " + result.getErrorOutputAsJoinedString());
        }

        root.refresh(false, true);
        repo.update();

        String rev = repo.getCurrentRevision();
        String revShort = (rev != null && rev.length() >= 7) ? rev.substring(0, 7) : (rev != null ? rev : "unknown");

        StringBuilder sb = new StringBuilder();
        sb.append("### Git Commit Successful\n\n");
        sb.append("- **Repository**: ").append(root.getName()).append("\n");
        sb.append("- **Revision**: `").append(revShort).append("`\n");
        sb.append("- **Message**: ").append(message.trim()).append("\n");
        if (authorName != null) {
            sb.append("- **Author**: ").append(authorName).append(" <").append(authorEmail != null ? authorEmail : "").append(">\n");
        }

        log("Committed revision " + revShort + " in " + root.getName());
        return sb.toString().trim();
    }

    /**
     * Opens the native IntelliJ Commit tool window or dialog on the UI with pre-filled commit message.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param filePaths Optional list of files to stage before opening the commit UI.
     * @param message The initial commit message to pre-fill.
     * @param authorName Optional author name.
     * @param authorEmail Optional author email.
     * @return Confirmation message of tool window dispatch.
     * @throws Exception if opening commit UI fails.
     */
    @AgiTool("Opens the native IntelliJ Commit tool window on the UI with pre-filled message and selected files.")
    public String gitOpenCommitDialog(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "Optional list of files to stage before opening the commit UI.", required = false, rendererId = "path") List<String> filePaths,
            @AgiToolParam(value = "The initial commit message to pre-fill.") String message,
            @AgiToolParam(value = "Optional author name.", required = false) String authorName,
            @AgiToolParam(value = "Optional author email.", required = false) String authorEmail) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();

        if (filePaths != null && !filePaths.isEmpty()) {
            gitAdd(filePaths);
        }

        SwingUtils.runInEDT(() -> {
            ToolWindow commitTw = ToolWindowManager.getInstance(project).getToolWindow("Commit");
            if (commitTw != null) {
                commitTw.activate(null, true);
            }
        });

        log("Activated IntelliJ Commit tool window for: " + repo.getRoot().getName());
        return "Successfully opened IntelliJ Commit tool window for repository `" + repo.getRoot().getName() + "`"
                + (message != null && !message.isBlank() ? " with suggested message: \"" + message.trim() + "\"" : "") + ".";
    }

    /**
     * Pushes committed revisions to a remote Git repository.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param remote The remote name (e.g. 'origin', 'helder'). If omitted, defaults to 'origin'.
     * @param branch Optional branch name to push. If omitted, pushes current active branch.
     * @param force Whether to force-push (allow non-fast-forward updates). Defaults to false.
     * @return Confirmation message of push result.
     * @throws Exception if push fails.
     */
    @AgiTool("Pushes committed revisions to a remote Git repository.")
    public String gitPush(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The remote name (e.g. 'origin', 'helder'). If omitted, defaults to 'origin'.", required = false) String remote,
            @AgiToolParam(value = "Optional branch name to push. If omitted, pushes current active branch.", required = false) String branch,
            @AgiToolParam(value = "Whether to force-push (allow non-fast-forward updates). Defaults to false.", required = false) Boolean force) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        String remoteName = (remote != null && !remote.isBlank()) ? remote.trim() : "origin";
        String branchName = (branch != null && !branch.isBlank()) ? branch.trim() : repo.getCurrentBranchName();
        if (branchName == null || branchName.isBlank()) {
            branchName = "HEAD";
        }

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.PUSH);
        if (force != null && force) {
            handler.addParameters("--force");
        }
        handler.addParameters(remoteName, branchName);

        log("Pushing branch '" + branchName + "' to remote '" + remoteName + "' in " + root.getName());
        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git push failed: " + result.getErrorOutputAsJoinedString());
        }

        repo.update();

        StringBuilder sb = new StringBuilder();
        sb.append("### Git Push Successful\n\n");
        sb.append("- **Repository**: ").append(root.getName()).append("\n");
        sb.append("- **Branch**: `").append(branchName).append("`\n");
        sb.append("- **Remote**: `").append(remoteName).append("`\n");
        if (force != null && force) {
            sb.append("- **Force Push**: true\n");
        }
        String out = result.getOutputAsJoinedString().trim();
        if (!out.isEmpty()) {
            sb.append("\n```\n").append(out).append("\n```\n");
        }

        return sb.toString().trim();
    }

    /**
     * Fetches remote branches, tags, and commits from a remote Git repository without modifying local working files.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param remote The remote name (e.g. 'origin', 'helder'). Defaults to 'origin'.
     * @return Markdown summary of fetched updates.
     * @throws Exception if fetch fails.
     */
    @AgiTool("Fetches updates from a remote Git repository into local tracking branches without modifying working files.")
    public String gitFetch(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The remote name (e.g. 'origin', 'helder'). Defaults to 'origin'.", required = false) String remote) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        String remoteName = (remote != null && !remote.isBlank()) ? remote.trim() : "origin";

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.FETCH);
        handler.addParameters(remoteName);

        log("Fetching from remote '" + remoteName + "' in " + root.getName());
        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git fetch failed: " + result.getErrorOutputAsJoinedString());
        }

        repo.update();

        StringBuilder sb = new StringBuilder();
        sb.append("### Git Fetch: ").append(remoteName).append(" (").append(root.getName()).append(")\n\n");
        String out = result.getOutputAsJoinedString().trim();
        if (out.isEmpty()) {
            sb.append("Already up to date. No remote updates found.\n");
        } else {
            sb.append("```\n").append(out).append("\n```\n");
        }

        return sb.toString().trim();
    }

    /**
     * Lists all local and optionally remote branches for a Git repository.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param includeRemote Whether to include remote-tracking branches. Defaults to true.
     * @return Markdown table of all branches, their active state, and commit hash.
     * @throws Exception if listing branches fails.
     */
    @AgiTool(value = "Lists local and remote branches in a Git repository.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitBranches(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "Whether to include remote tracking branches. Defaults to true.", required = false) Boolean includeRemote) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.BRANCH);
        handler.addParameters("-vv");
        if (includeRemote == null || includeRemote) {
            handler.addParameters("--all");
        }

        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Failed to list branches: " + result.getErrorOutputAsJoinedString());
        }

        List<String> lines = result.getOutput();
        StringBuilder sb = new StringBuilder();
        sb.append("### Branches: ").append(root.getName()).append("\n\n");
        sb.append("| Branch | Active | Type | Commit / Details |\n");
        sb.append("| :--- | :--- | :--- | :--- |\n");

        for (String line : lines) {
            if (line.isBlank()) {
                continue;
            }
            boolean isActive = line.startsWith("*");
            String trimmed = line.replace("*", " ").trim();
            String[] tokens = trimmed.split("\\s+", 3);
            String branchName = tokens.length > 0 ? tokens[0] : "";
            String commitShort = tokens.length > 1 ? tokens[1] : "";
            String details = tokens.length > 2 ? tokens[2] : "";

            boolean isRemote = branchName.startsWith("remotes/");
            sb.append("| `").append(branchName).append("` | ")
              .append(isActive ? "**YES**" : "no").append(" | ")
              .append(isRemote ? "Remote" : "Local").append(" | `")
              .append(commitShort).append("` ").append(details.replace("|", "\\|")).append(" |\n");
        }

        return sb.toString().trim();
    }

    /**
     * Switches the working tree to a target branch or revision, optionally creating the branch if it does not exist.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param branchOrRevision The branch name or commit revision to checkout.
     * @param createIfMissing If true, creates a new branch from current HEAD if the branch does not already exist.
     * @return Confirmation message of the checkout operation.
     * @throws Exception if checkout fails.
     */
    @AgiTool("Checks out a Git branch or revision, optionally creating the branch if missing.")
    public String gitCheckout(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The branch name or revision hash to checkout.") String branchOrRevision,
            @AgiToolParam(value = "Whether to create the branch if it does not exist. Defaults to false.", required = false) Boolean createIfMissing) throws Exception {

        if (branchOrRevision == null || branchOrRevision.isBlank()) {
            throw new AgiToolException("Target branch or revision cannot be empty.");
        }

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();
        String target = branchOrRevision.trim();

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.CHECKOUT);
        if (createIfMissing != null && createIfMissing) {
            GitLineHandler checkHandler = new GitLineHandler(project, root, GitCommand.BRANCH);
            checkHandler.addParameters("--list", target);
            GitCommandResult checkRes = Git.getInstance().runCommand(checkHandler);
            if (checkRes.getOutput().isEmpty()) {
                handler.addParameters("-b");
            }
        }
        handler.addParameters(target);

        log("Checking out: " + target + " in " + root.getName());
        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git checkout failed: " + result.getErrorOutputAsJoinedString());
        }

        root.refresh(false, true);
        repo.update();

        log("Successfully checked out " + target + " in " + root.getName());
        return "Successfully checked out branch/revision: `" + target + "` in " + root.getName();
    }

    /**
     * Creates a new Git branch at a specific revision or current HEAD.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param branchName Name of the new branch to create.
     * @param startRevision Optional starting revision hash or branch name. Defaults to 'HEAD'.
     * @return Confirmation message of branch creation.
     * @throws Exception if branch creation fails.
     */
    @AgiTool("Creates a new Git branch at a specified revision or current HEAD.")
    public String gitCreateBranch(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The name of the new branch to create.") String branchName,
            @AgiToolParam(value = "Optional starting revision hash or branch name. Defaults to 'HEAD'.", required = false) String startRevision) throws Exception {

        if (branchName == null || branchName.isBlank()) {
            throw new AgiToolException("Branch name cannot be empty.");
        }

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        String name = branchName.trim();
        String start = (startRevision != null && !startRevision.isBlank()) ? startRevision.trim() : "HEAD";

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.BRANCH);
        handler.addParameters(name, start);

        log("Creating branch '" + name + "' at " + start + " in " + root.getName());
        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Failed to create branch: " + result.getErrorOutputAsJoinedString());
        }

        repo.update();
        log("Successfully created branch " + name + " in " + root.getName());
        return "Successfully created branch `" + name + "` at revision `" + start + "` in " + root.getName();
    }

    /**
     * Deletes a local Git branch.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param branchName Name of the branch to delete.
     * @param force Whether to force deletion even if unmerged. Defaults to false.
     * @return Confirmation message of branch deletion.
     * @throws Exception if branch deletion fails.
     */
    @AgiTool("Deletes a local Git branch.")
    public String gitDeleteBranch(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The name of the branch to delete.") String branchName,
            @AgiToolParam(value = "Whether to force delete unmerged commits. Defaults to false.", required = false) Boolean force) throws Exception {

        if (branchName == null || branchName.isBlank()) {
            throw new AgiToolException("Branch name cannot be empty.");
        }

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();
        String name = branchName.trim();

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.BRANCH);
        handler.addParameters((force != null && force) ? "-D" : "-d", name);

        log("Deleting branch '" + name + "' in " + root.getName());
        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Failed to delete branch: " + result.getErrorOutputAsJoinedString());
        }

        repo.update();
        log("Successfully deleted branch " + name + " in " + root.getName());
        return "Successfully deleted branch `" + name + "` from " + root.getName();
    }

    /**
     * Merges a branch or revision into the current active branch.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param branchOrRevision The branch name or revision hash to merge into the active branch.
     * @param fastForwardPolicy Fast-forward merge policy. Defaults to FAST_FORWARD.
     * @return Markdown summary of the merge outcome.
     * @throws Exception if merge fails.
     */
    @AgiTool("Merges a branch or revision into the current active branch.")
    public String gitMerge(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The branch name or revision hash to merge into the active branch.") String branchOrRevision,
            @AgiToolParam(value = "Fast-forward merge policy. Defaults to FAST_FORWARD.", required = false) FastForwardPolicy fastForwardPolicy) throws Exception {

        if (branchOrRevision == null || branchOrRevision.isBlank()) {
            throw new AgiToolException("Branch name or revision cannot be empty.");
        }

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        String target = branchOrRevision.trim();
        String active = repo.getCurrentBranchName();

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.MERGE);
        FastForwardPolicy policy = fastForwardPolicy != null ? fastForwardPolicy : FastForwardPolicy.FAST_FORWARD;
        switch (policy) {
            case FAST_FORWARD_ONLY -> handler.addParameters("--ff-only");
            case NO_FAST_FORWARD -> handler.addParameters("--no-ff");
            default -> handler.addParameters("--ff");
        }
        handler.addParameters(target);

        log("Merging '" + target + "' into " + active + " in " + root.getName());
        GitCommandResult result = Git.getInstance().runCommand(handler);

        root.refresh(false, true);
        repo.update();

        StringBuilder sb = new StringBuilder();
        sb.append("### Git Merge Result: ").append(root.getName()).append("\n\n");
        sb.append("- **Active Branch**: `").append(active != null ? active : "HEAD").append("`\n");
        sb.append("- **Merged Revision/Branch**: `").append(target).append("`\n");
        sb.append("- **Status**: ").append(result.success() ? "SUCCESS" : "CONFLICTS_OR_ERROR").append("\n\n");
        sb.append("```\n").append(result.getOutputAsJoinedString().trim()).append("\n```\n");

        if (!result.success()) {
            sb.append("\n⚠️ Conflicts encountered during merge. Resolve conflicts in working tree files, stage with `gitAdd`, and finalize with `gitCommit`.\n");
        }

        return sb.toString().trim();
    }

    /**
     * Retrieves the commit log for a repository, specific branch, or revision.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param branchOrRevision Optional branch name (e.g. 'main', 'helder/main') or revision hash. If omitted, uses active branch HEAD.
     * @param maxEntries Maximum number of commits to retrieve. Defaults to 10.
     * @param filePath Optional file or directory path to limit the commit log to.
     * @param author Optional author name or email filter.
     * @param noMerges Whether to exclude merge commits. Defaults to false.
     * @param grep Optional regex or keyword filter for commit messages.
     * @return Markdown table of commit revisions, authors, dates, and messages.
     * @throws Exception if retrieving log fails.
     */
    @AgiTool(value = "Retrieves the commit history log for a Git repository, branch, or revision.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitLog(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "Optional branch name (e.g. 'main', 'helder/main') or commit hash. If omitted, uses active HEAD.", required = false) String branchOrRevision,
            @AgiToolParam(value = "Maximum number of commits to retrieve. Defaults to 10.", required = false) Integer maxEntries,
            @AgiToolParam(value = "Optional file or directory path to limit the commit log to.", required = false, rendererId = "path") String filePath,
            @AgiToolParam(value = "Optional author name or email filter.", required = false) String author,
            @AgiToolParam(value = "Whether to exclude merge commits. Defaults to false.", required = false) Boolean noMerges,
            @AgiToolParam(value = "Optional regex or keyword filter for commit messages.", required = false) String grep) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        int limit = (maxEntries != null && maxEntries > 0) ? maxEntries : 10;
        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.LOG);
        handler.addParameters("-n", String.valueOf(limit));
        handler.addParameters("--pretty=format:%h|%an|%ad|%s", "--date=short");

        if (noMerges != null && noMerges) {
            handler.addParameters("--no-merges");
        }
        if (author != null && !author.isBlank()) {
            handler.addParameters("--author=" + author.trim());
        }
        if (grep != null && !grep.isBlank()) {
            handler.addParameters("--grep=" + grep.trim());
        }
        if (branchOrRevision != null && !branchOrRevision.isBlank()) {
            handler.addParameters(branchOrRevision.trim());
        }

        if (filePath != null && !filePath.isBlank()) {
            File f = resolveFile(filePath);
            String rel = root.toNioPath().relativize(f.toPath()).toString();
            handler.addParameters("--", rel);
        }

        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git log failed: " + result.getErrorOutputAsJoinedString());
        }

        List<String> lines = result.getOutput();
        if (lines.isEmpty()) {
            return "No commits found for " + (branchOrRevision != null ? branchOrRevision : "HEAD") + " in " + root.getName();
        }

        StringBuilder sb = new StringBuilder();
        String targetLabel = (branchOrRevision != null && !branchOrRevision.isBlank()) ? branchOrRevision : "HEAD";
        sb.append("### Git Log: ").append(root.getName()).append(" [").append(targetLabel).append("]\n\n");
        sb.append("| Date | Revision | Author | Message |\n");
        sb.append("| :--- | :--- | :--- | :--- |\n");

        for (String line : lines) {
            String[] parts = line.split("\\|", 4);
            if (parts.length >= 4) {
                sb.append("| ").append(parts[2]).append(" | `")
                  .append(parts[0]).append("` | ")
                  .append(parts[1]).append(" | ")
                  .append(parts[3].replace("|", "\\|")).append(" |\n");
            }
        }

        return sb.toString().trim();
    }

    /**
     * Generates a Git diff between two branches or revisions, optionally filtered to a specific file or folder.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param baseRevision The base branch or revision hash (e.g. 'main', 'HEAD~1').
     * @param targetRevision The target branch or revision hash (e.g. 'helder/main', 'HEAD').
     * @param filePath Optional file or folder path to limit the diff to.
     * @param summaryOnly If true, returns only the list of modified/added/deleted file paths instead of the full patch text. Defaults to false.
     * @return Unified diff output as text or summary.
     * @throws Exception if diff generation fails.
     */
    @AgiTool(value = "Generates a Git diff between two branches or revisions, optionally filtered to a specific file.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitDiff(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The base branch or revision hash (e.g. 'main', 'HEAD~1').") String baseRevision,
            @AgiToolParam(value = "The target branch or revision hash (e.g. 'helder/main', 'HEAD').") String targetRevision,
            @AgiToolParam(value = "Optional specific file or folder path to limit the diff to.", required = false, rendererId = "path") String filePath,
            @AgiToolParam(value = "If true, returns only the list of modified/added/deleted file paths instead of the full patch text. Defaults to false.", required = false) Boolean summaryOnly) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.DIFF);
        if (summaryOnly != null && summaryOnly) {
            handler.addParameters("--name-status");
        }
        handler.addParameters(baseRevision.trim(), targetRevision.trim());

        if (filePath != null && !filePath.isBlank()) {
            File f = resolveFile(filePath);
            String rel = root.toNioPath().relativize(f.toPath()).toString();
            handler.addParameters("--", rel);
        }

        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git diff failed: " + result.getErrorOutputAsJoinedString());
        }

        String output = result.getOutputAsJoinedString().trim();
        if (output.isBlank()) {
            return "No differences found between " + baseRevision + " and " + targetRevision + (filePath != null ? " for " + filePath : "");
        }

        if (summaryOnly != null && summaryOnly) {
            StringBuilder sb = new StringBuilder();
            sb.append("### Git Diff Summary: ").append(root.getName()).append(" [").append(baseRevision).append("...").append(targetRevision).append("]\n\n");
            sb.append("| Status | File |\n");
            sb.append("| :--- | :--- |\n");
            for (String line : result.getOutput()) {
                String[] parts = line.split("\\s+", 2);
                if (parts.length == 2) {
                    sb.append("| `[").append(parts[0]).append("]` | `").append(parts[1]).append("` |\n");
                }
            }
            return sb.toString().trim();
        }

        return output;
    }

    /**
     * Reads and returns the content of a file from a specific Git branch or revision without modifying working files.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param branchOrRevision The branch name or revision hash (e.g. 'main', 'HEAD~2').
     * @param filePath The relative or absolute file path to read.
     * @return The raw text content of the file at that revision.
     * @throws Exception if reading the file fails.
     */
    @AgiTool(value = "Reads and returns the content of a file from a specific Git branch or revision without switching branches.", permission = ToolPermission.APPROVE_ALWAYS)
    public String gitShow(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The branch name or revision hash (e.g. 'main', 'HEAD~2').") String branchOrRevision,
            @AgiToolParam(value = "The relative or absolute file path to read.", rendererId = "path") String filePath) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        File f = resolveFileOrDirectory(filePath);
        String rel = root.toNioPath().relativize(f.toPath()).toString().replace('\\', '/');

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.SHOW);
        handler.addParameters(branchOrRevision.trim() + ":" + rel);

        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git show failed: " + result.getErrorOutputAsJoinedString());
        }

        return result.getOutputAsJoinedString();
    }

    /**
     * Pulls updates from a remote repository and merges them into the current active branch.
     *
     * @param repoPath Path of the repository or project directory. If omitted, uses active project repository.
     * @param remote The remote name (e.g. 'origin', 'helder'). If omitted, defaults to 'origin'.
     * @return Markdown summary of pull updates and merge outcome.
     * @throws Exception if pull or merge fails.
     */
    @AgiTool("Pulls changes from a remote Git repository into the current active branch (fetch and merge).")
    public String gitPull(
            @AgiToolParam(value = "Path of the repository or project directory. If omitted, uses active project repository.", required = false, rendererId = "path") String repoPath,
            @AgiToolParam(value = "The remote name (e.g. 'origin', 'helder'). Defaults to 'origin'.", required = false) String remote) throws Exception {

        GitRepository repo = requireRepo(repoPath);
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        String remoteName = (remote != null && !remote.isBlank()) ? remote.trim() : "origin";
        String currentBranch = repo.getCurrentBranchName();

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.PULL);
        handler.addParameters(remoteName);
        if (currentBranch != null && !currentBranch.isBlank()) {
            handler.addParameters(currentBranch);
        }

        log("Pulling from remote '" + remoteName + "' into " + currentBranch + " in " + root.getName());
        GitCommandResult result = Git.getInstance().runCommand(handler);

        root.refresh(false, true);
        repo.update();

        if (!result.success()) {
            throw new AgiToolException("Git pull failed: " + result.getErrorOutputAsJoinedString());
        }

        StringBuilder sb = new StringBuilder();
        sb.append("### Git Pull Result: ").append(root.getName()).append("\n\n");
        sb.append("- **Active Branch**: `").append(currentBranch != null ? currentBranch : "HEAD").append("`\n");
        sb.append("- **Remote**: `").append(remoteName).append("`\n\n");
        sb.append("```\n").append(result.getOutputAsJoinedString().trim()).append("\n```\n");

        return sb.toString().trim();
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
        GitRepository repo = requireRepo(file.getAbsolutePath());
        Project project = repo.getProject();
        VirtualFile root = repo.getRoot();

        String rel = root.toNioPath().relativize(file.toPath()).toString().replace('\\', '/');

        GitLineHandler handler = new GitLineHandler(project, root, GitCommand.BLAME);
        if (startLine != null && startLine > 0 && endLine != null && endLine >= startLine) {
            handler.addParameters("-L", startLine + "," + endLine);
        } else if (startLine != null && startLine > 0) {
            handler.addParameters("-L", startLine + ",");
        }
        if (revision != null && !revision.isBlank()) {
            handler.addParameters(revision.trim());
        }
        handler.addParameters("--", rel);

        GitCommandResult result = Git.getInstance().runCommand(handler);
        if (!result.success()) {
            throw new AgiToolException("Git blame failed: " + result.getErrorOutputAsJoinedString());
        }

        List<String> lines = result.getOutput();
        if (lines.isEmpty()) {
            return "File is empty or blame produced no output: " + file.getName();
        }

        StringBuilder sb = new StringBuilder();
        sb.append("### Git Blame: ").append(file.getName()).append("\n\n");
        sb.append("```\n");
        for (String l : lines) {
            sb.append(l).append("\n");
        }
        sb.append("```\n");

        return sb.toString().trim();
    }
}
