/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.status;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.StatusBarWidget;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.util.Alarm;
import com.intellij.util.Consumer;
import org.jetbrains.annotations.NotNull;
import uno.anahata.asi.intellij.internal.AgiContext;

import java.awt.Component;
import java.awt.event.MouseEvent;

/**
 * Status-bar widget showing the number of active Anahata sessions, with a click that focuses
 * the Anahata tool window.
 * <p>
 * The count is refreshed on a short recurring Swing-thread timer via {@link Alarm}. Session
 * data is read through {@link AgiContext} (the live container/session registry).
 * </p>
 *
 * @author anahata
 */
public class AnahataStatusBarWidget implements StatusBarWidget, StatusBarWidget.TextPresentation {

    /**
     * The project this widget belongs to (used to focus the tool window).
     */
    private final Project project;

    /**
     * Recurring refresh timer, disposed with the widget.
     */
    private final Alarm alarm;

    /**
     * The hosting status bar, set on install.
     */
    private StatusBar statusBar;

    /**
     * Constructs the widget for a project.
     *
     * @param project the owning project.
     */
    public AnahataStatusBarWidget(Project project) {
        this.project = project;
        this.alarm = new Alarm(Alarm.ThreadToUse.SWING_THREAD, this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public @NotNull String ID() {
        return AnahataStatusBarWidgetFactory.ID;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public WidgetPresentation getPresentation() {
        return this;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Records the status bar and starts the refresh loop.
     * </p>
     */
    @Override
    public void install(@NotNull StatusBar statusBar) {
        this.statusBar = statusBar;
        scheduleRefresh();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dispose() {
        alarm.cancelAllRequests();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Shows the active-session count.
     * </p>
     */
    @Override
    public String getText() {
        return "Anahata: " + AgiContext.activeSessions().size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public float getAlignment() {
        return Component.CENTER_ALIGNMENT;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTooltipText() {
        int count = AgiContext.activeSessions().size();
        return count == 0 ? "No active Anahata sessions — click to open" : count + " active Anahata session(s) — click to open";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Focuses the Anahata tool window on click.
     * </p>
     */
    @Override
    public Consumer<MouseEvent> getClickConsumer() {
        return event -> {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("Anahata ASI");
            if (toolWindow != null) {
                toolWindow.activate(null);
            }
        };
    }

    /**
     * Schedules the next status-bar refresh (~2.5s), re-arming itself.
     */
    private void scheduleRefresh() {
        alarm.addRequest(() -> {
            if (statusBar != null) {
                statusBar.updateWidget(ID());
            }
            scheduleRefresh();
        }, 2500);
    }
}
