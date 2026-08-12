/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide.actions;

import com.intellij.ide.projectView.ProjectView;
import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.ToggleAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.intellij.internal.AgiContext;

/**
 * A single session's toggle within the "AGI Context" popup: checked when the selected
 * files are all in that session's context, and adds/removes them on toggle.
 *
 * @author anahata
 */
public class ToggleAgiContextAction extends ToggleAction {

    /**
     * The session this toggle targets.
     */
    private final transient Agi agi;

    /**
     * Constructs a toggle for the given session, labelled with its display name.
     *
     * @param agi the target session.
     */
    public ToggleAgiContextAction(Agi agi) {
        super(agi.getDisplayName());
        this.agi = agi;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Selected when the current file selection is fully in this session's context.
     * </p>
     */
    @Override
    public boolean isSelected(@NotNull AnActionEvent e) {
        VirtualFile[] files = e.getData(CommonDataKeys.VIRTUAL_FILE_ARRAY);
        return files != null && AgiContext.allInContext(agi, files);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Adds the selection to the session's context when enabling, removes it when disabling.
     * </p>
     */
    @Override
    public void setSelected(@NotNull AnActionEvent e, boolean state) {
        VirtualFile[] files = e.getData(CommonDataKeys.VIRTUAL_FILE_ARRAY);
        if (files == null) {
            return;
        }
        if (state) {
            AgiContext.add(agi, files);
        } else {
            AgiContext.remove(agi, files);
        }
        // Repaint the Project view so the in-context badges update immediately.
        Project project = e.getProject();
        if (project != null) {
            ProjectView.getInstance(project).refresh();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Membership is a lightweight resource-manager lookup, safe to compute off the EDT.
     * </p>
     */
    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }
}
