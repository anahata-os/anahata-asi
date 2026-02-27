/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.ArrayList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToggleButton;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.model.core.RequestConfig;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolParameter;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.swing.agi.ContextPanel;
import uno.anahata.asi.swing.agi.render.AbstractCodeBlockSegmentRenderer;

/**
 * A panel that displays the details and controls for a specific {@link AbstractTool}.
 * <p>
 * It provides a tabbed view for inspecting the tool's parameters (one section per 
 * parameter), the return type schema, and the native declaration string.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ToolPanel extends JPanel {

    /** The parent context panel. */
    private final ContextPanel parentPanel;
    /** The tabbed pane for schemas. */
    private final JTabbedPane tabbedPane;
    
    /** Label for the tool name. */
    private final JLabel nameLabel;
    /** Label for the tool description. */
    private final JLabel descLabel;
    
    /** Panel containing the list of parameter sections. */
    private final JPanel paramsListPanel;
    /** Renderer for the response schema. */
    private final AbstractCodeBlockSegmentRenderer responseSchemaRenderer;
    /** Renderer for the native provider declaration. */
    private final AbstractCodeBlockSegmentRenderer nativeDeclarationRenderer;
    
    /** Cache of parameter renderers to avoid redundant creation. */
    private final List<AbstractCodeBlockSegmentRenderer> paramRenderers = new ArrayList<>();

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
        
        paramsListPanel = new JPanel();
        paramsListPanel.setLayout(new BoxLayout(paramsListPanel, BoxLayout.Y_AXIS));
        paramsListPanel.setOpaque(false);
        
        responseSchemaRenderer = createJsonRenderer();
        nativeDeclarationRenderer = createJsonRenderer();
        
        tabbedPane.addTab("Parameters", new JScrollPane(paramsListPanel));
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
        AbstractCodeBlockSegmentRenderer renderer = AbstractCodeBlockSegmentRenderer.create(parentPanel.getAgiPanel(), "", "json");
        renderer.setEditable(false);
        return renderer;
    }

    /**
     * Updates the panel to display the details for the given tool.
     * @param tool The selected tool.
     */
    public void setTool(AbstractTool<?, ?> tool) {
        nameLabel.setText("Tool: " + tool.getName());
        descLabel.setText("<html>" + tool.getDescription().replace("\n", "<br>") + "</html>");

        // 1. Update Parameters Tab
        paramsListPanel.removeAll();
        paramRenderers.clear();
        
        List<? extends AbstractToolParameter> parameters = tool.getParameters();
        if (parameters.isEmpty()) {
            paramsListPanel.add(new JLabel("This tool has no parameters.", JLabel.CENTER));
        } else {
            for (AbstractToolParameter<?> param : parameters) {
                paramsListPanel.add(createParameterSection(param));
                paramsListPanel.add(Box.createVerticalStrut(10));
            }
        }
        paramsListPanel.add(Box.createVerticalGlue());

        // 2. Update Response Schema
        responseSchemaRenderer.updateContent(JacksonUtils.prettyPrintJsonString(tool.getResponseJsonSchema()));
        responseSchemaRenderer.render();

        // 3. Update Native Declaration
        RequestConfig config = parentPanel.getAgi().getRequestConfig();
        String nativeJson = parentPanel.getAgi().getSelectedModel().getToolDeclarationJson(tool, config);
        nativeDeclarationRenderer.updateContent(JacksonUtils.prettyPrintJsonString(nativeJson));
        nativeDeclarationRenderer.render();

        // 4. Update Permissions (Footer)
        JPanel footer = (JPanel) getComponent(2);
        footer.removeAll();
        footer.add(createPermissionButtonGroup(tool), BorderLayout.WEST);

        revalidate();
        repaint();
    }

    /**
     * Creates a UI section for a single tool parameter.
     * @param param The parameter to render.
     * @return A JPanel containing the parameter's name and schema.
     */
    private JPanel createParameterSection(AbstractToolParameter<?> param) {
        JPanel section = new JPanel(new BorderLayout(5, 5));
        section.setOpaque(false);
        section.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)
        ));

        JLabel nameLabel = new JLabel(param.getName() + (param.isRequired() ? " (Required)" : " (Optional)"));
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD));
        section.add(nameLabel, BorderLayout.NORTH);

        AbstractCodeBlockSegmentRenderer renderer = createJsonRenderer();
        renderer.updateContent(JacksonUtils.prettyPrintJsonString(param.getJsonSchema()));
        renderer.render();
        section.add(renderer.getComponent(), BorderLayout.CENTER);
        
        paramRenderers.add(renderer);
        return section;
    }

    /**
     * Creates and returns a JPanel containing the permission toggle buttons for a tool.
     * @param tool The tool whose permissions are to be managed.
     * @return A configured JPanel with the button group.
     */
    private JPanel createPermissionButtonGroup(AbstractTool<?, ?> tool) {
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

        promptButton.addActionListener(e -> { tool.setPermission(ToolPermission.PROMPT); parentPanel.refresh(false); });
        alwaysButton.addActionListener(e -> { tool.setPermission(ToolPermission.APPROVE_ALWAYS); parentPanel.refresh(false); });
        neverButton.addActionListener(e -> { tool.setPermission(ToolPermission.DENY_NEVER); parentPanel.refresh(false); });

        return buttonPanel;
    }
}
