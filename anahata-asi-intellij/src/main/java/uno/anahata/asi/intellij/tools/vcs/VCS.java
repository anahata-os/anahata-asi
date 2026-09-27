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
import uno.anahata.asi.internal.AnahataDiffUtils;
import uno.anahata.asi.intellij.internal.ProjectUtils;

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

            java.util.List<FilePath> unversioned = manager.getUnversionedFilesPaths();
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
        File target = resolveFile(repoPath);
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
        sb.append("## Live Version Control: ").append(vcs.getDisplayName()).append("\n");
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
}
