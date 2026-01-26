/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JToggleButton;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.model.core.RequestConfig;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolParameter;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.swing.chat.ContextPanel;
import uno.anahata.asi.swing.chat.render.AbstractCodeBlockSegmentRenderer;

/**
 * A panel that displays the details and controls for a specific {@link AbstractTool},
 * including its JSON schemas for arguments and responses, and the native 
 * provider-specific declaration.
 *
 * @author anahata
 */
public class ToolPanel extends JPanel {

    /** The parent ToolsPanel. */
    private final ContextPanel parentPanel;
    /** The tabbed pane for schemas. */
    private final JTabbedPane tabbedPane;
    
    /** Label for the tool name. */
    private final JLabel nameLabel;
    /** Label for the tool description. */
    private final JLabel descLabel;
    
    /** Renderer for the arguments schema. */
    private final AbstractCodeBlockSegmentRenderer argsSchemaRenderer;
    /** Renderer for the response schema. */
    private final AbstractCodeBlockSegmentRenderer responseSchemaRenderer;
    /** Renderer for the native provider declaration. */
    private final AbstractCodeBlockSegmentRenderer nativeDeclarationRenderer;

    /**
     * Constructs a new ToolDetailPanel.
     * @param parentPanel The parent ToolsPanel, used to trigger UI refreshes.
     */
    public ToolPanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        // Header Panel
        JPanel headerPanel = new JPanel(new GridBagLayout());
        headerPanel.setBorder(BorderFactory.createTitledBorder("Tool Details"));
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(4, 8, 4, 8);

        nameLabel = new JLabel();
        nameLabel.setFont(nameLabel.getFont().deriveFont(java.awt.Font.BOLD, 14f));
        headerPanel.add(nameLabel, gbc);
        gbc.gridy++;

        descLabel = new JLabel();
        headerPanel.add(descLabel, gbc);
        gbc.gridy++;

        add(headerPanel, BorderLayout.NORTH);

        // Tabs for Schemas
        tabbedPane = new JTabbedPane();
        
        argsSchemaRenderer = createJsonRenderer();
        responseSchemaRenderer = createJsonRenderer();
        nativeDeclarationRenderer = createJsonRenderer();
        
        tabbedPane.addTab("Arguments Schema", argsSchemaRenderer.getComponent());
        tabbedPane.addTab("Response Schema", responseSchemaRenderer.getComponent());
        tabbedPane.addTab("Native Declaration", nativeDeclarationRenderer.getComponent());
        
        add(tabbedPane, BorderLayout.CENTER);

        // Footer for Permissions
        JPanel footerPanel = new JPanel(new BorderLayout());
        footerPanel.setBorder(BorderFactory.createEmptyBorder(8, 0, 0, 0));
        add(footerPanel, BorderLayout.SOUTH);
    }

    /**
     * Creates a code block renderer configured for JSON.
     * @return The configured renderer.
     */
    private AbstractCodeBlockSegmentRenderer createJsonRenderer() {
        AbstractCodeBlockSegmentRenderer renderer = AbstractCodeBlockSegmentRenderer.create(parentPanel.getChatPanel(), "", "json");
        renderer.setEditable(false);
        return renderer;
    }

    /**
     * Updates the panel to display the details for the given tool.
     *
     * @param tool The selected tool.
     */
    public void setTool(AbstractTool tool) {
        nameLabel.setText("Tool: " + tool.getName());
        descLabel.setText("<html>" + tool.getDescription().replace("\n", "<br>") + "</html>");

        // Update Schemas
        String argsJson = "{}";
        if (!tool.getParameters().isEmpty()) {
            Object firstParam = tool.getParameters().get(0);
            if (firstParam instanceof AbstractToolParameter atp) {
                argsJson = atp.getJsonSchema();
            }
        }
        argsSchemaRenderer.updateContent(JacksonUtils.prettyPrintJsonString(argsJson));
        argsSchemaRenderer.render();
        
        responseSchemaRenderer.updateContent(JacksonUtils.prettyPrintJsonString(tool.getResponseJsonSchema()));
        responseSchemaRenderer.render();

        // Update Native Declaration
        RequestConfig config = parentPanel.getChat().getRequestConfig();
        String nativeJson = parentPanel.getChat().getSelectedModel().getToolDeclarationJson(tool, config);
        nativeDeclarationRenderer.updateContent(JacksonUtils.prettyPrintJsonString(nativeJson));
        nativeDeclarationRenderer.render();

        // Update Permissions (Footer)
        JPanel footer = (JPanel) getComponent(2);
        footer.removeAll();
        footer.add(createPermissionButtonGroup(tool), BorderLayout.WEST);

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

        switch (tool.getPermission()) {
            case APPROVE_ALWAYS: alwaysButton.setSelected(true); break;
            case DENY_NEVER: neverButton.setSelected(true); break;
            default: promptButton.setSelected(true); break;
        }

        promptButton.addActionListener(e -> { tool.setPermission(ToolPermission.PROMPT); parentPanel.refresh(); });
        alwaysButton.addActionListener(e -> { tool.setPermission(ToolPermission.APPROVE_ALWAYS); parentPanel.refresh(); });
        neverButton.addActionListener(e -> { tool.setPermission(ToolPermission.DENY_NEVER); parentPanel.refresh(); });

        return buttonPanel;
    }
}
