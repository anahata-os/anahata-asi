/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolParameter;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.components.AdjustingTabPane;

/**
 * A panel that displays the details and controls for a specific {@link AbstractTool}.
 * <p>
 * It provides a dynamic tabbed view for inspecting tool parameters, the return 
 * type schema, and the native declaration string.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ToolPanel extends ScrollablePanel {

    /** The parent context panel. */
    private final ContextPanel parentPanel;
    /** The specialized tabbed pane for schemas. */
    private final AdjustingTabPane tabbedPane;
    
    /** Label for the tool name. */
    private final JLabel nameLabel;
    /** Label for the tool description. */
    private final JLabel descLabel;
    /** Panel for permission buttons in the header. */
    private final JPanel permissionPanel;

    /**
     * Constructs a new ToolPanel.
     * @param parentPanel The parent context panel.
     */
    public ToolPanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        
        // Ensure the panel can be resized small enough to not squeeze the tree
        setMinimumSize(new Dimension(0, 0));

        // 1. Header Panel (Tool Details)
        JPanel headerPanel = new JPanel(new GridBagLayout());
        headerPanel.setBorder(BorderFactory.createTitledBorder("Tool Details"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 8, 2, 8);

        nameLabel = new JLabel();
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 14f));
        headerPanel.add(nameLabel, gbc);
        
        // Permissions Group below the name
        gbc.gridy++;
        gbc.insets = new Insets(2, 8, 2, 8);
        permissionPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        permissionPanel.setOpaque(false);
        headerPanel.add(permissionPanel, gbc);

        gbc.gridy++;
        gbc.insets = new Insets(2, 8, 4, 8);
        descLabel = new JLabel();
        headerPanel.add(descLabel, gbc);

        add(headerPanel, BorderLayout.NORTH);

        // 2. Tabs Container (Center)
        tabbedPane = new AdjustingTabPane(150);
        add(tabbedPane, BorderLayout.CENTER);
    }

    /**
     * Updates the panel to display the details for the given tool.
     * @param tool The selected tool.
     */
    public void setTool(AbstractTool<?, ?> tool) {
        nameLabel.setText(tool.getName());
        descLabel.setText("<html>" + tool.getDescription().replace("\n", "<br>") + "</html>");

        // Update Permissions
        permissionPanel.removeAll();
        permissionPanel.add(createPermissionButtonGroup(tool));

        // Rebuild Tabs
        tabbedPane.removeAll();
        
        // 1. Parameter Tabs
        List<? extends AbstractToolParameter> parameters = tool.getParameters();
        for (AbstractToolParameter<?> param : parameters) {
            String title = (param.isRequired() ? "* " : "") + param.getName();
            tabbedPane.addTab(title, createSchemaViewer(param.getName(), param.getJsonSchema()));
        }

        // 2. Response Schema Tab (Disabled if method is void)
        String responseSchema = tool.getResponseJsonSchema();
        if (responseSchema == null || responseSchema.isBlank()) {
            tabbedPane.addTab("Response Schema", new JPanel());
            tabbedPane.setEnabledAt(tabbedPane.getTabCount() - 1, false);
        } else {
            tabbedPane.addTab("Response Schema", createSchemaViewer("response", responseSchema));
        }

        // 3. Native Declaration Tab
        RequestConfig config = parentPanel.getAgi().getRequestConfig();
        String nativeJson = parentPanel.getAgi().getSelectedModel().getToolDeclarationJson(tool, config);
        tabbedPane.addTab("Native Declaration", createSchemaViewer("native", nativeJson));

        tabbedPane.refresh();
        revalidate();
        repaint();
    }

    /**
     * Creates a high-fidelity viewer for a JSON schema.
     * @param name The name for the ephemeral resource.
     * @param json The JSON string to render.
     * @return A JComponent (viewer) wrapped in a padded panel.
     */
    private JComponent createSchemaViewer(String name, String json) {
        if (json == null) {
            return new JLabel(" Error: No schema data provided.");
        }
        // Direct ResourceUI Rendering (High-fidelity and clutter-free)
        String prettyJson = JacksonUtils.prettyPrintJsonString(json);
        StringHandle handle = new StringHandle(name + ".json", prettyJson);
        Resource ephemeral = new Resource(handle);
        try { 
            ephemeral.reloadIfNeeded(); 
        } catch (Exception e) { 
            log.error("Failed to reload ephemeral resource for {}", name, e); 
        }
        
        ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
        JComponent viewer = strategy.createContent(ephemeral, parentPanel.getAgiPanel());
        if (viewer instanceof AbstractTextResourceViewer atv) {
            atv.setToolbarVisible(false);
            atv.setVerticalScrollEnabled(false);
            atv.setPreviewAsEditor(true);
        }
        
        // Add a small border for padding within the tab
        JPanel wrapper = new JPanel(new BorderLayout());
        wrapper.setOpaque(false);
        wrapper.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        wrapper.add(viewer, BorderLayout.CENTER);
        
        return wrapper;
    }

    /**
     * Creates and returns a JPanel containing the permission toggle buttons for a tool.
     * @param tool The tool whose permissions are to be managed.
     * @return A configured JPanel with the button group.
     */
    private JPanel createPermissionButtonGroup(AbstractTool<?, ?> tool) {
        JPanel groupPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        groupPanel.setOpaque(false);
        
        JLabel label = new JLabel("Permission: ");
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        groupPanel.add(label);

        ButtonGroup group = new ButtonGroup();
        JToggleButton promptButton = new JToggleButton("Prompt");
        JToggleButton alwaysButton = new JToggleButton("Always Allow");
        JToggleButton neverButton = new JToggleButton("Never Allow");

        // Small styling
        Font btnFont = promptButton.getFont().deriveFont(11f);
        promptButton.setFont(btnFont);
        alwaysButton.setFont(btnFont);
        neverButton.setFont(btnFont);

        group.add(promptButton);
        group.add(alwaysButton);
        group.add(neverButton);

        groupPanel.add(promptButton);
        groupPanel.add(alwaysButton);
        groupPanel.add(neverButton);

        switch (tool.getPermission()) {
            case APPROVE_ALWAYS -> alwaysButton.setSelected(true);
            case DENY_NEVER -> neverButton.setSelected(true);
            default -> promptButton.setSelected(true);
        }

        promptButton.addActionListener(e -> { tool.setPermission(ToolPermission.PROMPT); parentPanel.refresh(false); });
        alwaysButton.addActionListener(e -> { tool.setPermission(ToolPermission.APPROVE_ALWAYS); parentPanel.refresh(false); });
        neverButton.addActionListener(e -> { tool.setPermission(ToolPermission.DENY_NEVER); parentPanel.refresh(false); });

        return groupPanel;
    }
}
