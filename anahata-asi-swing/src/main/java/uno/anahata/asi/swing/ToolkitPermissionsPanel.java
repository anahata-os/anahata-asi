/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.tree.DefaultTreeCellRenderer;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.jdesktop.swingx.JXTreeTable;
import org.jdesktop.swingx.decorator.ColorHighlighter;
import org.jdesktop.swingx.decorator.ComponentAdapter;
import org.jdesktop.swingx.decorator.Highlighter;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import uno.anahata.asi.AsiContainerPreferences;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.tool.spi.java.JavaObjectToolkit;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.message.part.tool.ToolPermissionRenderer;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.ToolIcon;

/**
 * A dedicated panel for managing tool permissions at the container level.
 * <p>
 * This panel provides a hierarchical "Rules of Engagement" view where toolkits 
 * and their respective tools are displayed in a treetable. Users can define 
 * global safety levels (Always, Prompt, Never) that act as the default for all 
 * sessions within this container.
 * </p>
 * <p>
 * <b>Inheritance Awareness:</b> The panel uses {@link JavaObjectToolkit#getAllAnnotatedMethods} 
 * to ensure that tools inherited from superclasses (e.g., in {@code SwingJava}) 
 * are correctly identified and configurable.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class ToolkitPermissionsPanel extends JPanel {

    /** The parent ASI container providing the preferences context. */
    private final AbstractSwingAsiContainer container;
    /** The preferences object where global tool rules are persisted. */
    private final AsiContainerPreferences prefs;

    /**
     * Constructs a new permissions panel.
     * 
     * @param container The ASI container instance.
     */
    public ToolkitPermissionsPanel(@NonNull AbstractSwingAsiContainer container) {
        this.container = container;
        this.prefs = container.getPreferences();
        setLayout(new BorderLayout());
        initComponents();
    }

    /**
     * Initializes the UI components, including the treetable, renderers, and editors.
     */
    private void initComponents() {
        PermissionsTreeTableModel model = new PermissionsTreeTableModel(prefs);
        JXTreeTable treeTable = new JXTreeTable(model);
        treeTable.setRowHeight(28);
        treeTable.setRootVisible(false);
        treeTable.setShowsRootHandles(true);
        treeTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF); // Prevent stretching

        // Set surgical column widths
        treeTable.getColumnModel().getColumn(0).setPreferredWidth(350); // Toolkit/Tool
        treeTable.getColumnModel().getColumn(1).setPreferredWidth(120); // Global Permission
        treeTable.getColumnModel().getColumn(1).setMinWidth(120);
        treeTable.getColumnModel().getColumn(1).setMaxWidth(120);
        
        // 1. Tool / Toolkit Icon Renderer
        treeTable.setTreeCellRenderer(new DefaultTreeCellRenderer() {
            @Override
            public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {
                super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);
                if (value instanceof PermissionsTreeTableModel.ToolkitNode tk) {
                    setIcon(IconUtils.getIcon("java.png", 16));
                    setText(tk.name);
                } else if (value instanceof PermissionsTreeTableModel.ToolNode tool) {
                    setIcon(new ToolIcon(16));
                    setText(tool.name);
                }
                return this;
            }
        });

        // 2. Permission Column Renderer
        treeTable.getColumnModel().getColumn(1).setCellRenderer(new DefaultTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
                super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
                setText(value instanceof ToolPermission tp ? tp.getDisplayValue() : "");
                return this;
            }
        });

        // 3. Simple Foreground-Only Highlighter (The SwingX Way to ensure visibility)
        Highlighter permHighlighter = new ColorHighlighter(
            (renderer, adapter) -> adapter.getValue() instanceof ToolPermission,
            null, null // Don't touch background
        ) {
            @Override
            protected Component doHighlight(Component component, ComponentAdapter adapter) {
                if (adapter.getValue() instanceof ToolPermission tp) {
                    component.setForeground(SwingAgiConfig.getColor(tp));
                }
                return component;
            }
        };

        treeTable.setHighlighters(permHighlighter);

        // 4. Compact Permission Column Editor
        JComboBox<ToolPermission> permissionCombo = new JComboBox<>(ToolPermission.values());
        permissionCombo.setRenderer(new ToolPermissionRenderer());
        
        treeTable.getColumnModel().getColumn(1).setCellEditor(new DefaultCellEditor(permissionCombo));

        // Expand all toolkits by default
        for (int i = 0; i < treeTable.getRowCount(); i++) {
            treeTable.expandRow(i);
        }

        add(new JScrollPane(treeTable), BorderLayout.CENTER);
        add(new JLabel("<html><div style='padding: 10px;'><b>Global Tool Rules of Engagement:</b> Set the default safety level for every tool grouped by toolkit. Changes apply to all sessions.</div></html>"), BorderLayout.NORTH);
    }

    /**
     * TreeTableModel implementation for the hierarchical tool permissions view.
     * <p>
     * This model maps the deep structure of discovered toolkits and their methods 
     * to a two-column table view.
     * </p>
     */
    private static class PermissionsTreeTableModel extends AbstractTreeTableModel {
        /** The list of root nodes representing discovered toolkits. */
        private final List<ToolkitNode> toolkitNodes = new ArrayList<>();
        /** Reference to preferences for persistence of permission changes. */
        private final AsiContainerPreferences preferences;

        /**
         * Constructs the model by scanning the AGI template for tool classes.
         * 
         * @param prefs The container preferences.
         */
        public PermissionsTreeTableModel(AsiContainerPreferences prefs) {
            super(new Object()); // dummy root
            this.preferences = prefs;
            AgiConfig template = prefs.getAgiTemplate();
            Map<String, ToolPermission> currentPermissions = prefs.getToolPermissions();

            for (Class<?> toolkitClass : template.getToolClasses()) {
                ToolkitNode tkNode = new ToolkitNode(toolkitClass.getSimpleName());
                toolkitNodes.add(tkNode);
                for (Method m : JavaObjectToolkit.getAllAnnotatedMethods(toolkitClass)) {
                    AgiTool toolAnnotation = m.getAnnotation(AgiTool.class);
                    if (toolAnnotation != null) {
                        String toolName = tkNode.name + "." + m.getName();
                        ToolPermission p = currentPermissions.getOrDefault(toolName, toolAnnotation.permission());
                        tkNode.tools.add(new ToolNode(m.getName(), p, tkNode));
                    }
                }
            }
        }

        /** {@inheritDoc} */
        @Override
        public int getColumnCount() {
            return 2;
        }

        /** {@inheritDoc} */
        @Override
        public String getColumnName(int column) {
            return column == 0 ? "Toolkit / Tool" : "Global Permission";
        }

        /** {@inheritDoc} */
        @Override
        public Object getValueAt(Object node, int column) {
            if (node instanceof ToolkitNode tk) {
                return column == 0 ? tk.name : null;
            } else if (node instanceof ToolNode tool) {
                return column == 0 ? tool.name : tool.permission;
            }
            return null;
        }

        /** 
         * {@inheritDoc} 
         * <p>Only the permission column (index 1) for Tool nodes is editable.</p> 
         */
        @Override
        public boolean isCellEditable(Object node, int column) {
            return node instanceof ToolNode && column == 1;
        }

        /** 
         * {@inheritDoc} 
         * <p>Persists the new permission level directly into the container preferences.</p> 
         */
        @Override
        public void setValueAt(Object value, Object node, int column) {
            if (node instanceof ToolNode tool && column == 1 && value instanceof ToolPermission p) {
                tool.permission = p;
                preferences.getToolPermissions().put(tool.parent.name + "." + tool.name, p);
            }
        }

        /** {@inheritDoc} */
        @Override
        public Object getChild(Object parent, int index) {
            if (parent == getRoot()) {
                return toolkitNodes.get(index);
            }
            if (parent instanceof ToolkitNode tk) {
                return tk.tools.get(index);
            }
            return null;
        }

        /** {@inheritDoc} */
        @Override
        public int getChildCount(Object parent) {
            if (parent == getRoot()) {
                return toolkitNodes.size();
            }
            if (parent instanceof ToolkitNode tk) {
                return tk.tools.size();
            }
            return 0;
        }

        /** {@inheritDoc} */
        @Override
        public int getIndexOfChild(Object parent, Object child) {
            if (parent == getRoot()) {
                return toolkitNodes.indexOf(child);
            }
            if (parent instanceof ToolkitNode tk) {
                return tk.tools.indexOf(child);
            }
            return -1;
        }

        /**
         * Represents a Toolkit grouping in the tree.
         */
        private static class ToolkitNode {
            /** The simple name of the toolkit class. */
            String name;
            /** The list of tools contained within this toolkit. */
            List<ToolNode> tools = new ArrayList<>();
            /** Constructs a toolkit node. */
            ToolkitNode(String name) { this.name = name; }
        }

        /**
         * Represents an individual Tool in the tree.
         */
        private static class ToolNode {
            /** The name of the tool method. */
            String name;
            /** The current global permission level. */
            ToolPermission permission;
            /** The parent toolkit node. */
            ToolkitNode parent;
            /** Constructs a tool node. */
            ToolNode(String name, ToolPermission p, ToolkitNode parent) {
                this.name = name; this.permission = p; this.parent = parent;
            }
        }
    }
}
