/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide.actions;

import com.intellij.openapi.actionSystem.ActionGroup;
import com.intellij.openapi.actionSystem.ActionUpdateThread;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.intellij.internal.AgiContext;

import java.util.List;

/**
 * Dynamic "AGI Context" popup group for the Project-view context menu, listing one toggle
 * per active AGI session that adds/removes the selected files from that session's context.
 * <p>
 * This is the IntelliJ analogue of the NetBeans {@code AnahataContextActionPresenter}. The
 * children are rebuilt on each menu open from the live session registry, so newly created
 * sessions appear automatically. When no sessions are active, a single disabled hint is
 * shown.
 * </p>
 *
 * @author anahata
 */
public class AgiContextActionGroup extends ActionGroup {

    /**
     * Constructs the action group (instantiated by the platform via its public no-arg constructor).
     */
    public AgiContextActionGroup() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Builds one {@link ToggleAgiContextAction} per active session, or a disabled hint when
     * there are none.
     * </p>
     */
    @Override
    public AnAction @NotNull [] getChildren(@Nullable AnActionEvent e) {
        List<Agi> sessions = AgiContext.activeSessions();
        if (sessions.isEmpty()) {
            AnAction hint = new AnAction("No Active AGI Sessions") {
                @Override
                public void actionPerformed(@NotNull AnActionEvent event) {
                    // Informational only.
                }
            };
            hint.getTemplatePresentation().setEnabled(false);
            return new AnAction[]{hint};
        }
        AnAction[] children = new AnAction[sessions.size()];
        for (int i = 0; i < sessions.size(); i++) {
            children[i] = new ToggleAgiContextAction(sessions.get(i));
        }
        return children;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Children are computed off the EDT: only the lightweight session registry and resource
     * managers are consulted, no PSI.
     * </p>
     */
    @Override
    public @NotNull ActionUpdateThread getActionUpdateThread() {
        return ActionUpdateThread.BGT;
    }
}
