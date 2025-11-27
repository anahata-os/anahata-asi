/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.tool;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.AbstractToolkit;
import uno.anahata.ai.model.tool.ToolPermission;
import uno.anahata.ai.swing.chat.ToolsPanel;

/**
 * A panel that displays the details and controls for a selected AbstractToolkit or AbstractTool.
 *
 * @author pablo
 */
public class ToolDetailPanel extends JPanel {

    private final ToolsPanel parentPanel;

    /**
     * Constructs a new ToolDetailPanel.
     * @param parentPanel The parent ToolsPanel, used to trigger UI refreshes.
     */
    public ToolDetailPanel(ToolsPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
    }

    /**
     * Updates the panel to display the details for the given tree node.
     *
     * @param node The selected node, which can be an AbstractToolkit, an AbstractTool, or null.
     */
    public void setNode(Object node) {
        removeAll();

        if (node instanceof AbstractToolkit) {
            add(createToolkitDetailPanel((AbstractToolkit) node), BorderLayout.NORTH);
        } else if (node instanceof AbstractTool) {
            add(createToolDetailPanel((AbstractTool) node), BorderLayout.NORTH);
        }

        revalidate();
        repaint();
    }

    /**
     * Creates and returns a JPanel containing the details and controls for a specific toolkit.
     *
     * @param toolkit The toolkit to display.
     * @return A configured JPanel.
     */
    private JPanel createToolkitDetailPanel(AbstractToolkit toolkit) {
        JPanel panel = new JPanel(new GridBagLayout());
        panel.setBorder(BorderFactory.createTitledBorder("<html><b>Toolkit: " + toolkit.getName() + "</b></html>"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 8, 4, 8);

        // Description
        String descriptionText = "<html>" + toolkit.getDescription().replace("\n", "<br>") + "</html>";
        panel.add(new JLabel(descriptionText), gbc);
        gbc.gridy++;

        // Enabled Checkbox
        JCheckBox enabledCheckbox = new JCheckBox("Enabled", toolkit.isEnabled());
        enabledCheckbox.addActionListener(e -> {
            toolkit.setEnabled(enabledCheckbox.isSelected());
            parentPanel.refresh();
        });
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        panel.add(enabledCheckbox, gbc);

        return panel;
    }

    /**
     * Creates and returns a JPanel containing the details and controls for a specific tool.
     *
     * @param tool The tool to display.
     * @return A configured JPanel.
     */
    private JPanel createToolDetailPanel(AbstractTool tool) {
        JPanel panel = new JPanel(new GridBagLayout());
        panel.setBorder(BorderFactory.createTitledBorder("<html><b>Tool: " + tool.getName() + "</b></html>"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 8, 4, 8);

        // Description
        String descriptionText = "<html>" + tool.getDescription().replace("\n", "<br>") + "</html>";
        panel.add(new JLabel(descriptionText), gbc);
        gbc.gridy++;

        // --- Button Group ---
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        panel.add(createPermissionButtonGroup(tool), gbc);

        return panel;
    }

    /**
     * Creates and returns a JPanel containing the permission toggle buttons for a tool.
     *
     * @param tool The tool whose permissions are to be managed.
     * @return A configured JPanel with the button group.
     */
    private JPanel createPermissionButtonGroup(AbstractTool tool) {
        JPanel buttonPanel = new JPanel();
        ButtonGroup group = new ButtonGroup();

        JToggleButton promptButton = new JToggleButton("Prompt");
        JToggleButton alwaysButton = new JToggleButton("Always Allow");
        JToggleButton neverButton = new JToggleButton("Never Allow");

        group.add(promptButton);
        group.add(alwaysButton);
        group.add(neverButton);

        buttonPanel.add(promptButton);
        buttonPanel.add(alwaysButton);
        buttonPanel.add(neverButton);

        // Set initial selection based on the tool's current permission
        switch (tool.getPermission()) {
            case APPROVE_ALWAYS:
                alwaysButton.setSelected(true);
                break;
            case DENY_NEVER:
                neverButton.setSelected(true);
                break;
            default: // APPROVE or DENY (treat as prompt)
                promptButton.setSelected(true);
                break;
        }

        // Add listeners to update the tool's permission
        promptButton.addActionListener(e -> {
            tool.setPermission(ToolPermission.APPROVE);
            parentPanel.refresh();
        });
        alwaysButton.addActionListener(e -> {
            tool.setPermission(ToolPermission.APPROVE_ALWAYS);
            parentPanel.refresh();
        });
        neverButton.addActionListener(e -> {
            tool.setPermission(ToolPermission.DENY_NEVER);
            parentPanel.refresh();
        });

        return buttonPanel;
    }
}
