/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.view;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.agi.resource.view.TextViewportSettings;
import uno.anahata.asi.swing.internal.AnyChangeDocumentListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A specialized metadata panel for the {@link TextView}.
 * <p>
 * This panel provides integrated controls for the text viewport, including 
 * tailing, grep patterns, and line numbers. It reactively updates the 
 * model and triggers reloads when settings change.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class TextViewPanel extends AbstractViewPanel<TextView> {

    /** Toggle for enabling/disabling the tailing behavior (following the end of the file). */
    private final JCheckBox tailCheck;
    /** Spinner to configure the number of lines to tail. */
    private final JSpinner tailLinesSpinner;
    /** Spinner for start character offset. */
    private final JSpinner fromSpinner;
    /** Spinner for page size. */
    private final JSpinner sizeSpinner;
    /** Field for entering a regex pattern to filter lines (grep). */
    private final JTextField grepField;
    /** Toggle for showing/hiding line numbers in the viewport. */
    private final JCheckBox lineNumbersCheck;
    /** Label displaying real-time token metrics. */
    private final JLabel tokenLabel;

    /** Guard flag to prevent feedback loops during UI synchronization. */
    private boolean syncing = false;

    /**
     * Constructs a new TextViewPanel with integrated viewport controls.
     */
    public TextViewPanel() {
        tailLinesSpinner = new JSpinner(new SpinnerNumberModel(100, 1, 10000, 50));
        tailLinesSpinner.setPreferredSize(new Dimension(70, 22));
        tailLinesSpinner.addChangeListener(e -> updateViewportSettings());
        tailLinesSpinner.setEnabled(false);

        fromSpinner = new JSpinner(new SpinnerNumberModel(0L, 0L, Long.MAX_VALUE, 1024L));
        fromSpinner.setPreferredSize(new Dimension(100, 22));
        fromSpinner.addChangeListener(e -> updateViewportSettings());

        sizeSpinner = new JSpinner(new SpinnerNumberModel(65536, 1, 1024 * 1024, 4096));
        sizeSpinner.setPreferredSize(new Dimension(80, 22));
        sizeSpinner.addChangeListener(e -> updateViewportSettings());

        tailCheck = new JCheckBox("Tail");
        tailCheck.addActionListener(e -> {
            boolean tailing = tailCheck.isSelected();
            tailLinesSpinner.setEnabled(tailing);
            fromSpinner.setEnabled(!tailing);
            sizeSpinner.setEnabled(!tailing);
            updateViewportSettings();
        });
        grepField = new JTextField();
        grepField.setPreferredSize(new Dimension(120, 22));
        grepField.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::updateViewportSettings));

        lineNumbersCheck = new JCheckBox("Line Numbers");
        lineNumbersCheck.addActionListener(e -> updateViewportSettings());

        tokenLabel = new JLabel("Tokens: N/A");

        // Layout Assembly: Triple-row configuration for professional viewport management
        JPanel controls = new JPanel(new java.awt.GridBagLayout());
        controls.setOpaque(false);
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new java.awt.Insets(2, 0, 2, 8);

        // Row 1: Pagination (Mutual exclusive with Tail)
        gbc.gridy = 0; 
        gbc.gridx = 0; controls.add(new JLabel("From:"), gbc);
        gbc.gridx = 1; controls.add(fromSpinner, gbc);
        gbc.gridx = 2; controls.add(new JLabel("Size:"), gbc);
        gbc.gridx = 3; controls.add(sizeSpinner, gbc);

        // Row 2: Live Tailing
        gbc.gridy = 1; 
        gbc.gridx = 0; controls.add(tailCheck, gbc);
        gbc.gridx = 1; controls.add(new JLabel("Lines:"), gbc);
        gbc.gridx = 2; controls.add(tailLinesSpinner, gbc);

        // Row 3: Filtering & Display
        gbc.gridy = 2; 
        gbc.gridx = 0; controls.add(new JLabel("Grep pattern:"), gbc);
        gbc.gridx = 1; gbc.gridwidth = 2; gbc.fill = GridBagConstraints.HORIZONTAL;
        controls.add(grepField, gbc);
        gbc.gridx = 3; gbc.gridwidth = 1; gbc.fill = GridBagConstraints.NONE;
        controls.add(lineNumbersCheck, gbc);

        addProperty("Viewport:", controls);
        addProperty("Metrics:", tokenLabel);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Synchronizes the viewport controls with 
     * the current TextView state and estimated token count.</p>
     */
    @Override
    public void refresh() {
        if (view == null) {
            return;
        }

        this.syncing = true;
        try {
            TextViewportSettings settings = view.getViewport().getSettings();
            boolean tailing = settings.isTail();
            tailCheck.setSelected(tailing);
            tailLinesSpinner.setValue(settings.getTailLines());
            tailLinesSpinner.setEnabled(tailing);
            fromSpinner.setValue(settings.getStartChar());
            fromSpinner.setEnabled(!tailing);
            sizeSpinner.setValue(settings.getPageSizeInChars());
            sizeSpinner.setEnabled(!tailing);
            grepField.setText(settings.getGrepPattern());
            lineNumbersCheck.setSelected(settings.isIncludeLineNumbers());
            
            int tokens = view.getTokenCount();
            tokenLabel.setText("Estimated Tokens: " + tokens);
        } finally {
            this.syncing = false;
        }
    }

    /**
     * Authoritatively updates the underlying viewport settings and 
     * triggers a background reload of the resource.
     */
    private void updateViewportSettings() {
        if (syncing || view == null) {
            return;
        }

        TextViewportSettings settings = view.getViewport().getSettings();
        settings.setTail(tailCheck.isSelected());
        settings.setTailLines((Integer) tailLinesSpinner.getValue());
        settings.setStartChar((Long) fromSpinner.getValue());
        settings.setPageSizeInChars((Integer) sizeSpinner.getValue());
        settings.setGrepPattern(grepField.getText());
        settings.setIncludeLineNumbers(lineNumbersCheck.isSelected());

        view.markDirty();
        
        // Trigger background reload for immediate feedback in tabs
        new SwingTask<>(this, "Refresh Viewport", () -> {
            view.getOwner().reloadIfNeeded();
            return null;
        }).execute();
    }
}
