/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.swing.chat.ContextPanel;

/**
 * A panel that displays the details and management controls for an {@link AbstractToolkit}.
 *
 * @author anahata
 */
public class ToolkitPanel extends JPanel {

    private final ContextPanel parentPanel;
    
    private final JLabel nameLabel;
    private final JLabel descLabel;
    private final JCheckBox enabledCheckbox;
    private final JSpinner retentionSpinner;

    public ToolkitPanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setBorder(BorderFactory.createTitledBorder("Toolkit Model Details"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 8, 4, 8);

        nameLabel = new JLabel();
        nameLabel.setFont(nameLabel.getFont().deriveFont(java.awt.Font.BOLD, 14f));
        mainPanel.add(nameLabel, gbc);
        gbc.gridy++;

        descLabel = new JLabel();
        mainPanel.add(descLabel, gbc);
        gbc.gridy++;

        enabledCheckbox = new JCheckBox("Toolkit Enabled");
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(enabledCheckbox, gbc);
        gbc.gridy++;

        JPanel retentionPanel = new JPanel(new BorderLayout(5, 0));
        retentionPanel.add(new JLabel("Default Retention (turns):"), BorderLayout.WEST);
        retentionSpinner = new JSpinner(new SpinnerNumberModel(-1, -1, 100, 1));
        retentionPanel.add(retentionSpinner, BorderLayout.CENTER);
        mainPanel.add(retentionPanel, gbc);

        add(mainPanel, BorderLayout.NORTH);
        add(new JPanel(), BorderLayout.CENTER); // Spacer
    }

    public void setToolkit(AbstractToolkit<?> tk) {
        nameLabel.setText("Toolkit: " + tk.getName());
        descLabel.setText("<html>" + tk.getDescription().replace("\n", "<br>") + "</html>");
        
        for (java.awt.event.ActionListener al : enabledCheckbox.getActionListeners()) {
            enabledCheckbox.removeActionListener(al);
        }
        enabledCheckbox.setSelected(tk.isEnabled());
        enabledCheckbox.addActionListener(e -> {
            tk.setEnabled(enabledCheckbox.isSelected());
            parentPanel.refresh();
        });

        retentionSpinner.setValue(tk.getDefaultRetention());
        retentionSpinner.addChangeListener(e -> {
            // Note: AbstractToolkit needs a setter for defaultRetention if we want to edit it here.
            // For now, we just display it.
        });

        revalidate();
        repaint();
    }
}
