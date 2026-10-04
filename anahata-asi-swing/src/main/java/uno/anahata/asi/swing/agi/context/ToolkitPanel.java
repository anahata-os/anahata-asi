/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Optional;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeListener;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.toolkit.render.ToolkitUiRegistry;

/**
 * A panel that displays the details and management controls for an
 * {@link AbstractToolkit}.
 * <p>
 * It supports extensible UI components via the {@link ToolkitUiRegistry}.
 * </p>
 *
 * @author anahata
 */
public class ToolkitPanel extends ScrollablePanel {

    /**
     * The parent container panel.
     */
    private final ContextPanel parentPanel;

    /**
     * Label showing the toolkit's name.
     */
    private final JLabel nameLabel;
    /**
     * Label displaying the toolkit's description.
     */
    private final JLabel descLabel;
    /**
     * Checkbox toggling active state of this toolkit.
     */
    private final JCheckBox enabledCheckbox;
    /**
     * Spinner defining maximum context pruning depth.
     */
    private final JSpinner maxDepthSpinner;
    /**
     * Label showing effective max depth when inheriting (-1).
     */
    private final JLabel maxDepthInheritLabel;
    /**
     * The toolkit currently being inspected.
     */
    private AbstractToolkit<?> currentToolkit;
    /**
     * Reentrancy guard preventing circular event feedback during programmatic UI synchronization.
     */
    private boolean adjusting = false;
    /**
     * Wrapper container for specialized toolkit UI components.
     */
    private final JPanel rendererContainer;

    /**
     * Constructs a new ToolkitPanel.
     *
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
        enabledCheckbox.addActionListener(e -> {
            if (adjusting || currentToolkit == null) {
                return;
            }
            currentToolkit.setEnabled(enabledCheckbox.isSelected());
            parentPanel.refresh(false);
        });
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        detailsPanel.add(enabledCheckbox, gbc);
        gbc.gridy++;

        JPanel maxDepthPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        maxDepthPanel.setOpaque(false);
        maxDepthPanel.add(new JLabel("Default Max Depth:"));
        maxDepthSpinner = new JSpinner(new SpinnerNumberModel(-1, -1, 100, 1));
        maxDepthSpinner.addChangeListener(e -> {
            if (adjusting || currentToolkit == null) {
                return;
            }
            int newDepth = (Integer) maxDepthSpinner.getValue();
            currentToolkit.setDefaultMaxDepth(newDepth);
            updateMaxDepthLabel(currentToolkit);
        });
        maxDepthPanel.add(maxDepthSpinner);
        maxDepthInheritLabel = new JLabel();
        maxDepthInheritLabel.setFont(maxDepthInheritLabel.getFont().deriveFont(Font.ITALIC));
        maxDepthPanel.add(maxDepthInheritLabel);
        detailsPanel.add(maxDepthPanel, gbc);

        rendererContainer = new JPanel(new BorderLayout());
        rendererContainer.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Toolkit Specialized UI"),
                BorderFactory.createEmptyBorder(8, 8, 8, 8)
        ));
        rendererContainer.setVisible(false);

        // CUSTOM RENDERER POSITION: Below details as requested
        add(detailsPanel, BorderLayout.NORTH);
        add(rendererContainer, BorderLayout.CENTER);
    }

    /**
     * Updates the panel with the given toolkit's information.
     *
     * @param tk The toolkit to display.
     */
    public void setToolkit(AbstractToolkit<?> tk) {
        this.currentToolkit = tk;
        this.adjusting = true;
        try {
            nameLabel.setText("Toolkit: " + tk.getName());
            descLabel.setText("<html>" + tk.getDescription().replace("\n", "<br>") + "</html>");

            enabledCheckbox.setSelected(tk.isEnabled());
            maxDepthSpinner.setValue(tk.getDefaultMaxDepth());
            updateMaxDepthLabel(tk);
        } finally {
            this.adjusting = false;
        }

        // Custom Toolkit UI Injection
        rendererContainer.removeAll();
        Optional<JPanel> rendererOpt = ToolkitUiRegistry.getInstance().createRenderer(tk, parentPanel.getAgiPanel());
        if (rendererOpt.isPresent()) {
            rendererContainer.add(rendererOpt.get(), BorderLayout.CENTER);
            rendererContainer.setVisible(true);
        } else {
            rendererContainer.setVisible(false);
        }

        revalidate();
        repaint();
    }

    /**
     * Updates the inheritance label based on the current toolkit max depth.
     *
     * @param tk The active toolkit.
     */
    private void updateMaxDepthLabel(AbstractToolkit<?> tk) {
        int depth = tk.getDefaultMaxDepth();
        if (depth == -1) {
            int effective = parentPanel.getAgiPanel().getAgi().getConfig().getDefaultToolMaxDepth();
            maxDepthInheritLabel.setText("(inherit from agi config: " + effective + ")");
        } else {
            maxDepthInheritLabel.setText("(explicit: " + depth + ")");
        }
    }
}
