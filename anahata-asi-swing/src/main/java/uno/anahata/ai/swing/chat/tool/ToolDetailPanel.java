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
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.ToolPermission;
import uno.anahata.ai.swing.chat.ToolsPanel;

/**
 * A panel that displays the details and controls for a specific {@link AbstractTool}.
 *
 * @author anahata
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
     * Updates the panel to display the details for the given tool.
     *
     * @param tool The selected tool.
     */
    public void setTool(AbstractTool tool) {
        removeAll();

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

        add(panel, BorderLayout.NORTH);

        revalidate();
        repaint();
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
            default: // PROMPT or DENY (treat as prompt)
                promptButton.setSelected(true);
                break;
        }

        // Add listeners to update the tool's permission
        promptButton.addActionListener(e -> {
            tool.setPermission(ToolPermission.PROMPT);
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
