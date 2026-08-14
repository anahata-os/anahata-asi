/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.vcs;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vcs.FilePath;
import com.intellij.openapi.vcs.changes.Change;
import com.intellij.openapi.vcs.changes.ChangeListManager;
import com.intellij.openapi.vcs.changes.LocalChangeList;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

import java.util.Collection;

/**
 * A toolkit for inspecting version-control status through IntelliJ's generic VCS layer.
 * <p>
 * A beyond-parity capability with no NetBeans equivalent. It uses the provider-agnostic
 * {@link ChangeListManager} — which works for Git and any other configured VCS — to report
 * changed, added, deleted and unversioned files, grouped by change list. VCS-provider-specific
 * operations (Git branch/commit/log/blame) require the {@code git4idea} plugin API, which is
 * not published as a resolvable artifact, so they are intentionally out of scope.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for inspecting version-control status (changed and unversioned files).")
public class Vcs extends AnahataToolkit {

    /**
     * Constructs the Vcs toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Vcs() {
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
}
