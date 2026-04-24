/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.status;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JPopupMenu;
import javax.swing.SwingConstants;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.PulseIcon;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;
import uno.anahata.asi.swing.internal.SwingTaskManager;

/**
 * A self-contained monitor component for tracking active background tasks.
 * <p>
 * This component provides a high-salience list of currently running 
 * {@link SwingTask} instances, featuring animated pulse icons and real-time 
 * status updates. It can be embedded in sidebars or floating panels to 
 * maintain UI observability without bloating the primary status dashboard.
 * </p>
 * 
 * @author anahata
 */
public class SwingTaskMonitor extends JPanel {
 
      /**
       * Displays the detailed task monitor for a specific Agi session.
       */
      public static void showPopup(Component invoker, Agi agi) {
          showPopup(invoker, agi, agi.getConfig().getAsiContainer());
      }

      /**
       * Displays the detailed task monitor for a specific container dashboard.
       */
      public static void showPopup(Component invoker, AbstractAsiContainer container) {
          showPopup(invoker, null, container);
      }

      /**
       * Displays the detailed task monitor using specific session and container context.
       * 
       * @param invoker The component to position the popup relative to.
       * @param agi The agi session to filter for (may be null).
       * @param container The container to filter for (mandatory).
       */
      public static void showPopup(Component invoker, Agi agi, AbstractAsiContainer container) {
          JPopupMenu popup = new JPopupMenu();
          SwingTaskMonitor monitor = new SwingTaskMonitor(agi, container);
          monitor.setPreferredSize(new Dimension(400, 250));
          popup.add(monitor);
          popup.show(invoker, 0, -monitor.getPreferredSize().height);
      }

      private final Agi agi;
      private final AbstractAsiContainer container;
      private final JPanel taskListPanel;
      private final EdtPropertyChangeListener taskListener;

      public SwingTaskMonitor(Agi agi) {
          this(agi, agi.getConfig().getAsiContainer());
      }

      public SwingTaskMonitor(AbstractAsiContainer container) {
          this(null, container);
      }

      private SwingTaskMonitor(Agi agi, AbstractAsiContainer container) {
          setLayout(new BorderLayout());
          this.agi = agi;
          this.container = container;
        
        taskListPanel = new JPanel(new MigLayout("fillx, wrap 1, insets 2", "[grow]", "[]2[]"));
        
        JScrollPane scrollPane = new JScrollPane(taskListPanel);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
        add(scrollPane, BorderLayout.CENTER);
        
        this.taskListener = new EdtPropertyChangeListener(this, SwingTaskManager.getInstance(), "activeTasks", evt -> refresh());
        
        refresh();
    }

    /**
     * Synchronizes the UI with the current set of active tasks.
     */
    public void refresh() {
        taskListPanel.removeAll();
        List<SwingTask<?>> allTasks = SwingTaskManager.getInstance().getActiveTasks();
        List<SwingTask<?>> tasks = allTasks.stream()
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
        
        if (tasks.isEmpty()) {
            JLabel idleLabel = new JLabel("No active tasks.", SwingConstants.CENTER);
            idleLabel.setForeground(Color.GRAY);
            taskListPanel.add(idleLabel, "growx, gaptop 10, gapbottom 10");
        } else {
            for (SwingTask<?> task : tasks) {
                taskListPanel.add(createTaskEntry(task), "growx");
            }
        }
        
        revalidate();
        repaint();
    }

    private JPanel createTaskEntry(SwingTask<?> task) {
        JPanel panel = new JPanel(new MigLayout("fillx, ins 5", "[grow, fill]5[pref!]", "[]"));
        panel.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createLineBorder(Color.LIGHT_GRAY),
            BorderFactory.createEmptyBorder(5, 5, 5, 5)
        ));
        
        JProgressBar pb = new JProgressBar();
        pb.setIndeterminate(true);
        pb.setStringPainted(true);
        pb.setString(task.getTaskName());
        pb.setFont(pb.getFont().deriveFont(java.awt.Font.BOLD, 11f));
        panel.add(pb, "growx");

        JButton killButton = new JButton(new CancelIcon(16));
        killButton.setToolTipText("Terminate background task");
        killButton.addActionListener(e -> task.cancel(true));
        panel.add(killButton);
        
        return panel;
    }
}
