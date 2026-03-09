/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.swing.toolkits.ToolkitUiRegistry;

/**
 * A panel that displays the details and management controls for an {@link AbstractToolkit}.
 * <p>
 * It supports extensible UI components via the {@link ToolkitUiRegistry}.
 * </p>
 *
 * @author anahata
 */
public class ToolkitPanel extends JPanel {

    private final ContextPanel parentPanel;
    
    private final JLabel nameLabel;
    private final JLabel descLabel;
    private final JCheckBox enabledCheckbox;
    private final JSpinner maxDepthSpinner;
    /** Wrapper container for specialized toolkit UI components. */
    private final JPanel rendererContainer;

    /**
     * Constructs a new ToolkitPanel.
     * @param parentPanel The parent context panel.
     */
    public ToolkitPanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        
        setMinimumSize(new Dimension(0, 0));

        JPanel detailsPanel = new JPanel(new GridBagLayout());
        detailsPanel.setBorder(BorderFactory.createTitledBorder("Toolkit Model Details"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 8, 4, 8);

        nameLabel = new JLabel();
        nameLabel.setFont(nameLabel.getFont().deriveFont(java.awt.Font.BOLD, 14f));
        detailsPanel.add(nameLabel, gbc);
        gbc.gridy++;

        descLabel = new JLabel();
        detailsPanel.add(descLabel, gbc);
        gbc.gridy++;

        enabledCheckbox = new JCheckBox("Toolkit Enabled");
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        detailsPanel.add(enabledCheckbox, gbc);
        gbc.gridy++;

        JPanel maxDepthPanel = new JPanel(new BorderLayout(5, 0));
        maxDepthPanel.add(new JLabel("Default Max Depth:"), BorderLayout.WEST);
        maxDepthSpinner = new JSpinner(new SpinnerNumberModel(-1, -1, 100, 1));
        maxDepthPanel.add(maxDepthSpinner, BorderLayout.CENTER);
        detailsPanel.add(maxDepthPanel, gbc);

        rendererContainer = new JPanel(new BorderLayout());
        rendererContainer.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createTitledBorder("Toolkit Specialized UI"),
            BorderFactory.createEmptyBorder(8, 8, 8, 8)
        ));
        
        // CUSTOM RENDERER POSITION: Below details as requested
        add(detailsPanel, BorderLayout.NORTH);
        add(rendererContainer, BorderLayout.CENTER);
    }

    /**
     * Updates the panel with the given toolkit's information.
     * @param tk The toolkit to display.
     */
    public void setToolkit(AbstractToolkit<?> tk) {
        nameLabel.setText("Toolkit: " + tk.getName());
        descLabel.setText("<html>" + tk.getDescription().replace("\n", "<br>") + "</html>");
        
        for (java.awt.event.ActionListener al : enabledCheckbox.getActionListeners()) {
            enabledCheckbox.removeActionListener(al);
        }
        enabledCheckbox.setSelected(tk.isEnabled());
        enabledCheckbox.addActionListener(e -> {
            tk.setEnabled(enabledCheckbox.isSelected());
            parentPanel.refresh(false);
        });

        maxDepthSpinner.setValue(tk.getDefaultMaxDepth());

        // Custom Toolkit UI Injection
        rendererContainer.removeAll();
        ToolkitUiRegistry.getInstance().createRenderer(tk, parentPanel.getAgiPanel()).ifPresent(comp -> {
            rendererContainer.add(comp, BorderLayout.CENTER);
        });

        revalidate();
        repaint();
    }
}
