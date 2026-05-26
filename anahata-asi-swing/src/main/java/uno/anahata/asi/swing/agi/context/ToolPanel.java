/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FlowLayout;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.message.part.tool.ToolPermissionRenderer;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
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
 * A panel that displays the details and controls for a specific
 * {@link AbstractTool}.
 * <p>
 * It provides a dynamic tabbed view for inspecting tool parameters, the return
 * type schema, and the native declaration string.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ToolPanel extends ScrollablePanel {

    /**
     * The parent context panel.
     */
    private final ContextPanel parentPanel;
    /**
     * The specialized tabbed pane for schemas.
     */
    private final AdjustingTabPane tabbedPane;

    /**
     * Label for the tool name.
     */
    private final JLabel nameLabel;
    /**
     * Label for the tool description.
     */
    private final JLabel descLabel;
    /**
     * Panel for permission buttons in the header.
     */
    private final JPanel permissionPanel;

    /**
     * The control for tool permissions.
     */
    private JComboBox<ToolPermission> permissionCombo;
    /**
     * The control for default max depth.
     */
    private JSpinner maxDepthSpinner;
    /**
     * The active tool listener.
     */
    private EdtPropertyChangeListener permissionListener;

    /**
     * Constructs a new ToolPanel.
     *
     * @param parentPanel The parent context panel.
     */
    public ToolPanel(ContextPanel parentPanel) {
        this.parentPanel = parentPanel;
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        // Ensure the panel can be resized small enough to not squeeze the tree
        setMinimumSize(new Dimension(0, 0));

        // 1. Header Panel (Tool Details)
        JPanel headerPanel = new JPanel(new MigLayout("fillx, insets 4 8 4 8", "[grow]", "[]2[]5[]"));
        headerPanel.setBorder(BorderFactory.createTitledBorder("Tool Details"));

        nameLabel = new JLabel();
        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 14f));
        headerPanel.add(nameLabel, "wrap");

        permissionPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        permissionPanel.setOpaque(false);

        permissionCombo = new JComboBox<>(ToolPermission.values());
        permissionCombo.setRenderer(new ToolPermissionRenderer());
        permissionCombo.addActionListener(e -> {
            AbstractTool<?, ?> tool = (AbstractTool<?, ?>) permissionCombo.getClientProperty("tool");
            if (tool != null) {
                ToolPermission tp = (ToolPermission) permissionCombo.getSelectedItem();
                tool.setPermission(tp);
                permissionCombo.setForeground(SwingAgiConfig.getColor(tp));
            }
        });

        permissionPanel.add(new JLabel("Permission: "));
        permissionPanel.add(permissionCombo);

        headerPanel.add(permissionPanel, "wrap");

        JPanel maxDepthPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        maxDepthPanel.setOpaque(false);
        maxDepthPanel.add(new JLabel("Max Depth: "));
        maxDepthSpinner = new JSpinner(new SpinnerNumberModel(-1, -1, 100, 1));
        maxDepthSpinner.addChangeListener(e -> {
            AbstractTool<?, ?> tool = (AbstractTool<?, ?>) permissionCombo.getClientProperty("tool");
            if (tool != null) {
                tool.setMaxDepth((Integer) maxDepthSpinner.getValue());
            }
        });
        maxDepthPanel.add(maxDepthSpinner);

        headerPanel.add(maxDepthPanel, "wrap");

        descLabel = new JLabel();
        headerPanel.add(descLabel, "growx");

        add(headerPanel, BorderLayout.NORTH);

        // 2. Tabs Container (Center)
        tabbedPane = new AdjustingTabPane(150);
        add(tabbedPane, BorderLayout.CENTER);
    }

    /**
     * Updates the panel to display the details for the given tool.
     *
     * @param tool The selected tool.
     */
    public void setTool(AbstractTool<?, ?> tool) {
        nameLabel.setText(tool.getName());
        descLabel.setText("<html>" + tool.getDescription().replace("\n", "<br>") + "</html>");

        // Update Permissions
        if (permissionListener != null) {
            permissionListener.unbind();
        }
        permissionCombo.putClientProperty("tool", tool);
        ToolPermission tp = tool.getPermission();
        permissionCombo.setSelectedItem(tp);
        permissionCombo.setForeground(SwingAgiConfig.getColor(tp));

        permissionListener = new EdtPropertyChangeListener(this, tool, "permission", evt -> {
            ToolPermission newTp = (ToolPermission) evt.getNewValue();
            permissionCombo.setSelectedItem(newTp);
            permissionCombo.setForeground(SwingAgiConfig.getColor(newTp));
        });

        maxDepthSpinner.setValue(tool.getMaxDepth());

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
     *
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

}
