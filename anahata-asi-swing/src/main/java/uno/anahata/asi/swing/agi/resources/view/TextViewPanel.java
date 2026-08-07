/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.view;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JToggleButton;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.agi.resource.view.TextViewportSettings;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A specialized metadata panel for the {@link TextView}.
 * <p>
 * This panel provides integrated controls for the text viewport, including
 * tailing, grep patterns, and line numbers. It reactively updates the model and
 * triggers reloads when settings change.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class TextViewPanel extends AbstractViewPanel<TextView> {

    /**
     * Toggle for enabling/disabling the tailing behavior (following the end of
     * the file).
     */
    private final JCheckBox tailCheck;
    /**
     * Spinner to configure the number of lines to tail.
     */
    private final JSpinner tailLinesSpinner;
    /**
     * Spinner for start character offset.
     */
    private final JSpinner fromSpinner;
    /**
     * Spinner for page size.
     */
    private final JSpinner sizeSpinner;
    /**
     * Field for entering a regex pattern to filter lines (grep).
     */
    private final JTextField grepField;
    /**
     * Toggle for showing/hiding line numbers in the viewport.
     */
    private final JCheckBox lineNumbersCheck;
    /**
     * Spinner to configure the maximum column width before wrapping/truncation.
     */
    private final JSpinner colWidthSpinner;
    /**
     * Label displaying real-time token metrics.
     */
    private final JLabel tokenLabel;
    /**
     * Pending background task calculating tokens, allowing preemption on rapid updates.
     */
    private SwingTask<Integer> pendingTokenTask;
    /**
     * Button/Toggle to enable or disable full view mode.
     */
    private final JToggleButton fullViewButton;
    /**
     * Label displaying the viewport summary toString.
     */
    private final JLabel summaryLabel;

    /**
     * Guard flag to prevent feedback loops during UI synchronization.
     */
    private boolean syncing = false;

    /**
     * Constructs a new TextViewPanel with integrated viewport controls.
     *
     * @param agiPanel The parent session panel.
     */
    public TextViewPanel(AgiPanel agiPanel) {
        super(agiPanel);
        setLayout(new BorderLayout());
        tailLinesSpinner = new JSpinner(new SpinnerNumberModel(100, 1, 10000, 50));
        tailLinesSpinner.setPreferredSize(new Dimension(70, 22));
        tailLinesSpinner.addChangeListener(e -> {
            if (!syncing && view != null) {
                view.getViewport().getSettings().setTailLines(((Number) tailLinesSpinner.getValue()).intValue());
            }
        });
        tailLinesSpinner.setEnabled(false);

        fromSpinner = new JSpinner(new SpinnerNumberModel(0, 0, Integer.MAX_VALUE, 4096));
        fromSpinner.setPreferredSize(new Dimension(80, 22));
        fromSpinner.addChangeListener(e -> {
            if (!syncing && view != null) {
                view.getViewport().getSettings().setStartChar(((Number) fromSpinner.getValue()).intValue());
            }
        });

        sizeSpinner = new JSpinner(new SpinnerNumberModel(65536, 1, Integer.MAX_VALUE, 4096));
        sizeSpinner.setPreferredSize(new Dimension(100, 22));
        sizeSpinner.addChangeListener(e -> {
            if (!syncing && view != null) {
                view.getViewport().getSettings().setPageSizeInChars(((Number) sizeSpinner.getValue()).intValue());
            }
        });

        tailCheck = new JCheckBox("Tail (Follow end of resource)");
        tailCheck.setOpaque(false);
        tailCheck.addActionListener(e -> {
            if (!syncing && view != null) {
                view.getViewport().getSettings().setTail(tailCheck.isSelected());
            }
        });
        grepField = new JTextField();
        grepField.setPreferredSize(new Dimension(200, 22));
        grepField.getDocument().addDocumentListener(new AnyChangeDocumentListener(() -> {
            if (!syncing && view != null) {
                view.getViewport().getSettings().setGrepPattern(grepField.getText());
            }
        }));

        lineNumbersCheck = new JCheckBox("Inject Line Numbers");
        lineNumbersCheck.setOpaque(false);
        lineNumbersCheck.addActionListener(e -> {
            if (!syncing && view != null) {
                view.getViewport().getSettings().setIncludeLineNumbers(lineNumbersCheck.isSelected());
            }
        });

        colWidthSpinner = new JSpinner(new SpinnerNumberModel(1024, 10, Integer.MAX_VALUE, 100));
        colWidthSpinner.setPreferredSize(new Dimension(80, 22));
        colWidthSpinner.addChangeListener(e -> {
            if (!syncing && view != null) {
                view.getViewport().getSettings().setColumnWidth(((Number) colWidthSpinner.getValue()).intValue());
            }
        });

        tokenLabel = new JLabel("Tokens: N/A");
        summaryLabel = new JLabel("");
        summaryLabel.setFont(summaryLabel.getFont().deriveFont(Font.ITALIC));

        fullViewButton = new JToggleButton("Full View");
        fullViewButton.addActionListener(e -> {
            if (!syncing && view != null) {
                boolean selected = fullViewButton.isSelected();
                log.info("fullViewButton clicked: selected={}, view={}", selected, view.getOwner() != null ? view.getOwner().getName() : "null");
                view.getViewport().getSettings().setFullView(selected);
            }
        });

        // Layout Assembly: Vertical stack of rows
        JPanel controls = new JPanel();
        controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));
        controls.setOpaque(false);

        // Row 1: Metrics
        JPanel metricsRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
        metricsRow.setOpaque(false);
        metricsRow.add(tokenLabel);
        controls.add(metricsRow);

        // Row 2: Pagination & Expand
        JPanel paginationRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
        paginationRow.setOpaque(false);
        paginationRow.add(new JLabel("From (Char):"));
        paginationRow.add(fromSpinner);
        paginationRow.add(Box.createHorizontalStrut(10));
        paginationRow.add(new JLabel("Size (Chars):"));
        paginationRow.add(sizeSpinner);
        paginationRow.add(Box.createHorizontalStrut(10));
        paginationRow.add(fullViewButton);
        controls.add(paginationRow);

        // Row 3: Line Numbers & Col Width
        JPanel displayRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
        displayRow.setOpaque(false);
        displayRow.add(lineNumbersCheck);
        displayRow.add(Box.createHorizontalStrut(10));
        displayRow.add(new JLabel("Max Col Width:"));
        displayRow.add(colWidthSpinner);
        controls.add(displayRow);

        // Row 4: Tailing
        JPanel tailRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
        tailRow.setOpaque(false);
        tailRow.add(tailCheck);
        tailRow.add(Box.createHorizontalStrut(10));
        tailRow.add(new JLabel("Tailing Lines:"));
        tailRow.add(tailLinesSpinner);
        controls.add(tailRow);

        // Row 5: Grep filtering
        JPanel grepRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
        grepRow.setOpaque(false);
        grepRow.add(new JLabel("Grep pattern (Regex):"));
        grepRow.add(grepField);
        controls.add(grepRow);

        // Row 6: Summary (Bottom)
        JPanel footerRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 2));
        footerRow.setOpaque(false);
        footerRow.add(summaryLabel);
        controls.add(footerRow);

        add(controls, BorderLayout.NORTH);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Synchronizes the viewport controls with the
     * current TextView state and estimated token count.</p>
     */
    @Override
    public void refresh() {
        log.info("TextViewPanel refresh() view={}", view );
        if (view == null) {
            return;
        }

        this.syncing = true;
        try {
            TextViewportSettings settings = view.getViewport().getSettings();
            boolean fullView = settings.isFullView();
            fullViewButton.setSelected(fullView);
            fullViewButton.setText(fullView ? "Full View (Active)" : "Full View");

            boolean tailing = settings.isTail();
            tailCheck.setSelected(tailing);
            tailCheck.setEnabled(!fullView);
            tailLinesSpinner.setValue(settings.getTailLines());
            tailLinesSpinner.setEnabled(!fullView && tailing);

            fromSpinner.setValue(settings.getStartChar());
            fromSpinner.setEnabled(!fullView && !tailing);

            sizeSpinner.setValue(settings.getPageSizeInChars());
            sizeSpinner.setEnabled(!fullView && !tailing);

            grepField.setText(settings.getGrepPattern());
            grepField.setEnabled(!fullView);

            lineNumbersCheck.setSelected(settings.isIncludeLineNumbers());

            colWidthSpinner.setValue(settings.getColumnWidth());
            colWidthSpinner.setEnabled(!fullView);

            tokenLabel.setText("Estimated Tokens: Counting...");
            
            if (pendingTokenTask != null && !pendingTokenTask.isDone()) {
                log.info("Cancelling previous SwingTask " + pendingTokenTask);
                pendingTokenTask.cancel(true);
            }

            final TextView targetView = view;
            String resName = (view.getOwner() != null) ? view.getOwner().getName() : "Resource";
            
            final SwingTask<Integer>[] taskRef = new SwingTask[1];
            taskRef[0] = new SwingTask<Integer>(agiPanel, "Calculating Tokens: " + resName, () -> {
                return targetView.getTokenCount();
            }, count -> {
                if (this.pendingTokenTask == taskRef[0] && this.view == targetView) {
                    tokenLabel.setText("Estimated Tokens: " + count);
                }
            }, error -> {
                if (this.pendingTokenTask == taskRef[0] && this.view == targetView) {
                    tokenLabel.setText("Estimated Tokens: Error");
                }
            }, false);

            this.pendingTokenTask = taskRef[0];
            log.info("Starting new SwingTask " + pendingTokenTask);
            taskRef[0].start();
            
            summaryLabel.setText(view.getViewport().toString());
        } finally {
            this.syncing = false;
        }
    }

}
