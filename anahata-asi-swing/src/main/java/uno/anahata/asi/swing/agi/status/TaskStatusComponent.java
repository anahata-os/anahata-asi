/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.status;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.SwingTaskManager;

/**
 * A high-density status bar component for monitoring background activities.
 * <p>
 * This component displays the most recent active {@link SwingTask} with an
 * indeterminate progress bar and a summary count of other active tasks. It
 * provides a hand-cursor interaction to reveal the full
 * {@link SwingTaskMonitor} popup.
 * <p>
 * <b>Global Mode:</b> If constructed with a {@code null} Agi, this component
 * monitors only container-level infrastructure tasks.
 * </p>
 *
 * @author anahata
 */
public class TaskStatusComponent extends JPanel {

    /**
     * The Agi session to monitor, if any.
     */
    private final Agi agi;
    /**
     * The container to monitor.
     */
    private final AbstractAsiContainer container;
    /**
     * The visual indeterminate progress bar display.
     */
    private final JProgressBar progressBar;
    /**
     * Button allowing rapid cancelation of the newest background activity.
     */
    private final JButton quickKillButton;
    /**
     * Binds activeTasks list updates to our EDT redraw loop.
     */
    private final EdtPropertyChangeListener taskListener;

    /**
     * Constructs a monitor for a specific Agi session. Shows tasks for this agi
     * .
     *
     * @param agi the agi whose task will be rendering
     */
    public TaskStatusComponent(Agi agi) {
        this(agi, agi.getConfig().getAsiContainer());
    }

    /**
     * Constructs a monitor for a specific asi container. Shows all tasks
     * belonging to this container.
     *
     * @param container the asi container this component is for.
     */
    public TaskStatusComponent(AbstractAsiContainer container) {
        this(null, container);
    }

    /**
     * Private constructor for either one or the other.
     * 
     * @param agi the one 
     * @param container the other
     */
    private TaskStatusComponent(Agi agi, AbstractAsiContainer container) {
        super(new MigLayout("ins 0, fillx, gap 5", "[grow, fill]5[pref!]", "[]"));
        this.agi = agi;
        this.container = container;
        setOpaque(false);
        setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));

        progressBar = new JProgressBar();
        progressBar.setIndeterminate(true);
        progressBar.setStringPainted(true);        
        progressBar.setPreferredSize(new Dimension(210, 22));
        progressBar.setMinimumSize(new Dimension(32, 22));
        //progressBar.setFont(progressBar.getFont().deriveFont(Font.BOLD, 10f));
        progressBar.setForeground(new Color(0, 102, 204));

        quickKillButton = new JButton(new CancelIcon(14));
        quickKillButton.setBorder(BorderFactory.createEmptyBorder());
        quickKillButton.setContentAreaFilled(false);
        quickKillButton.setToolTipText("Stop most recent task");

        add(progressBar, "growx");
        add(quickKillButton);
        
        setPreferredSize(new Dimension(240, 26));
        setMinimumSize(new Dimension(240, 26));
        setMaximumSize(new Dimension(240, 26));
        
        setVisible(false);

        // Ensure clicks on sub-components also trigger the popup
        MouseAdapter popupTrigger = new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                SwingTaskMonitor.showPopup(TaskStatusComponent.this, agi, container);
            }
        };
        addMouseListener(popupTrigger);
        progressBar.addMouseListener(popupTrigger);

        this.taskListener = new EdtPropertyChangeListener(this, SwingTaskManager.getInstance(), "activeTasks", evt -> refresh());
        refresh();
    }

    /**
     * Queries active tasks from the registry and syncs layout visibility and summaries.
     */
    private void refresh() {
        List<SwingTask<?>> allTasks = SwingTaskManager.getInstance().getActiveTasks();
        List<SwingTask<?>> activeTasks = allTasks.stream()
                .filter(t -> {
                    if (agi != null) {
                        // Session monitor: STRICTLY show only this session's tasks
                        return t.getAgi() == agi;
                    } else {
                        // Container monitor: STRICTLY show only global tasks for this stadium
                        return t.getAgi() == null && t.getContainer() == container;
                    }
                })
                .toList();

        if (!activeTasks.isEmpty()) {
            SwingTask<?> latest = activeTasks.get(activeTasks.size() - 1);
            String summary = latest.getTaskName();
            if (activeTasks.size() > 1) {
                summary += " (+" + (activeTasks.size() - 1) + " more)";
            }
            progressBar.setString(summary);

            // Wire quick-kill button to latest task
            for (ActionListener al : quickKillButton.getActionListeners()) {
                quickKillButton.removeActionListener(al);
            }
            quickKillButton.addActionListener(e -> latest.cancel(true));

            setVisible(true);
        } else {
            setVisible(false);
        }
        revalidate();
        repaint();
    }
}
