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
import uno.anahata.asi.resource.v2.view.TextView;
import uno.anahata.asi.resource.v2.view.TextViewportSettings;
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

    private final JCheckBox tailCheck;
    private final JSpinner tailLinesSpinner;
    private final JTextField grepField;
    private final JCheckBox lineNumbersCheck;
    private final JLabel tokenLabel;

    /** Guard flag to prevent feedback loops during UI synchronization. */
    private boolean syncing = false;

    /**
     * Constructs a new TextViewPanel with integrated viewport controls.
     */
    public TextViewPanel() {
        tailCheck = new JCheckBox("Tail");
        tailCheck.addActionListener(e -> updateViewportSettings());

        tailLinesSpinner = new JSpinner(new SpinnerNumberModel(100, 1, 10000, 50));
        tailLinesSpinner.setPreferredSize(new Dimension(70, 22));
        tailLinesSpinner.addChangeListener(e -> updateViewportSettings());

        grepField = new JTextField();
        grepField.setPreferredSize(new Dimension(150, 22));
        grepField.getDocument().addDocumentListener(new AnyChangeDocumentListener(this::updateViewportSettings));

        lineNumbersCheck = new JCheckBox("Line Numbers");
        lineNumbersCheck.addActionListener(e -> updateViewportSettings());

        tokenLabel = new JLabel("Tokens: N/A");

        // Layout Assembly
        JPanel controls = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 0));
        controls.setOpaque(false);
        controls.add(tailCheck);
        controls.add(new JLabel("Lines:"));
        controls.add(tailLinesSpinner);
        controls.add(new JLabel("Grep:"));
        controls.add(grepField);
        controls.add(lineNumbersCheck);

        addProperty("Controls:", controls);
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
            tailCheck.setSelected(settings.isTail());
            tailLinesSpinner.setValue(settings.getTailLines());
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
