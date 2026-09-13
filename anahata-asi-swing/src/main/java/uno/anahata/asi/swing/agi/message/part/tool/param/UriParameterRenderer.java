/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.EditIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.icons.SaveIcon;
import uno.anahata.asi.swing.internal.SwingUtils;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.UIManager;

/**
 * A specialized parameter renderer for an individual URI string.
 * <p>
 * Supports both a compact pill/chip representation and an in-place high-fidelity
 * text editor for modifying or removing the URI.
 * When placed inside a collection, it is composed by a list container
 * such as {@link WrapListParameterRenderer} or {@link VBoxListParameterRenderer}.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class UriParameterRenderer extends AbstractParameterRenderer<Object> {

    /** CardLayout for toggling between the compact pill and in-place editor. */
    private final CardLayout cardLayout = new CardLayout();

    /** The root component hosting the card layout. */
    private final JPanel container = new JPanel(cardLayout) {
        @Override
        public Dimension getPreferredSize() {
            if (editing) {
                Dimension ps = editorPanel.getPreferredSize();
                Container parent = getParent();
                int w = (parent != null && parent.getWidth() > 0) ? parent.getWidth() - 15 : 600;
                return new Dimension(w, ps.height);
            }
            return pillPanel.getPreferredSize();
        }

        @Override
        public void updateUI() {
            super.updateUI();
            if (pillPanel != null) {
                applyColors();
            }
        }
    };

    /** The compact chip panel. */
    private final JPanel pillPanel = new JPanel(new BorderLayout(8, 0));

    /** The editor wrapper panel. */
    private final JPanel editorPanel = new JPanel(new BorderLayout());

    /** Label displaying the file name of the URI. */
    private final JLabel nameLabel = new JLabel();

    /** Whether the chip is currently in edit mode. */
    private boolean editing = false;

    /** Active high-fidelity viewer when in edit mode. */
    private AbstractTextResourceViewer editorViewer;

    /**
     * Constructs a new UriParameterRenderer with pill styling.
     */
    public UriParameterRenderer() {
        container.setOpaque(false);
        pillPanel.setOpaque(true);

        applyColors();

        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD));
        nameLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        nameLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                setEditing(true);
            }
        });

        JButton copyBtn = new JButton(new CopyIcon(14));
        copyBtn.putClientProperty("JButton.buttonType", "toolBarButton");
        copyBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        copyBtn.setToolTipText("Copy URI");
        copyBtn.setMargin(new Insets(1, 4, 1, 4));
        copyBtn.addActionListener(e -> SwingUtils.copyToClipboard(value != null ? value.toString() : ""));

        pillPanel.add(copyBtn, BorderLayout.WEST);

        JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 0));
        centerPanel.setOpaque(false);
        centerPanel.add(nameLabel);

        JButton openBtn = new JButton(new ExternalIcon(14));
        openBtn.putClientProperty("JButton.buttonType", "toolBarButton");
        openBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        openBtn.setToolTipText("Open");
        openBtn.setMargin(new Insets(1, 4, 1, 4));
        openBtn.addActionListener(e -> {
            if (value != null && ResourceUiRegistry.getInstance().getResourceUI() != null) {
                ResourceUiRegistry.getInstance().getResourceUI().openUri(value.toString());
            }
        });
        centerPanel.add(openBtn);

        JButton editBtn = new JButton(new EditIcon(14));
        editBtn.putClientProperty("JButton.buttonType", "toolBarButton");
        editBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        editBtn.setMargin(new Insets(1, 4, 1, 4));
        editBtn.setToolTipText("Edit in-place");
        editBtn.addActionListener(e -> setEditing(true));
        centerPanel.add(editBtn);

        pillPanel.add(centerPanel, BorderLayout.CENTER);

        JButton deleteBtn = new JButton(new DeleteIcon(14));
        deleteBtn.putClientProperty("JButton.buttonType", "toolBarButton");
        deleteBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        deleteBtn.setToolTipText("Remove");
        deleteBtn.setMargin(new Insets(1, 8, 1, 4));
        deleteBtn.addActionListener(e -> deleteSelf());

        pillPanel.add(deleteBtn, BorderLayout.EAST);

        editorPanel.setOpaque(false);

        container.add(pillPanel, "pill");
        container.add(editorPanel, "editor");
    }

    /**
     * Applies theme-aware colors from UIManager to support light and dark Look and Feels.
     */
    private void applyColors() {
        Color bg = UIManager.getColor("Button.background");
        if (bg == null) {
            bg = new Color(230, 235, 245);
        }

        Color border = UIManager.getColor("Component.borderColor");
        if (border == null) {
            border = UIManager.getColor("Separator.foreground");
        }
        if (border == null) {
            border = Color.GRAY;
        }

        Color fg = UIManager.getColor("Label.foreground");

        pillPanel.setBackground(bg);
        pillPanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(border, 1, true),
                BorderFactory.createEmptyBorder(2, 6, 2, 6)
        ));
        if (fg != null) {
            nameLabel.setForeground(fg);
        }
        editorPanel.setBorder(BorderFactory.createLineBorder(border, 1, true));
    }

    /**
     * Toggles the editing state and rebuilds the active view.
     *
     * @param editing true to show the editor, false for the compact pill.
     */
    public void setEditing(boolean editing) {
        this.editing = editing;
        if (editing) {
            setupEditor();
            cardLayout.show(container, "editor");
        } else {
            cardLayout.show(container, "pill");
            editorPanel.removeAll();
        }

        container.revalidate();
        container.repaint();
        Container p = container.getParent();
        if (p != null) {
            p.revalidate();
            p.repaint();
        }
    }

    /**
     * Mounts the high-fidelity NetBeans/RSyntax editor for modifying the URI string.
     */
    private void setupEditor() {
        editorPanel.removeAll();

        JPanel header = new JPanel(new BorderLayout(5, 0));
        header.setOpaque(true);
        Color headerBg = UIManager.getColor("TableHeader.background");
        if (headerBg == null) {
            headerBg = UIManager.getColor("Panel.background");
        }
        header.setBackground(headerBg);
        header.setBorder(BorderFactory.createEmptyBorder(2, 6, 2, 6));

        JLabel title = new JLabel(getEditorTitlePrefix() + getDisplayName());
        title.setFont(title.getFont().deriveFont(Font.BOLD, 11f));
        Color fg = UIManager.getColor("Label.foreground");
        if (fg != null) {
            title.setForeground(fg);
        }
        header.add(title, BorderLayout.WEST);

        JPanel headerActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 0));
        headerActions.setOpaque(false);

        JButton cancelBtn = new JButton("Cancel", new CancelIcon(14));
        cancelBtn.putClientProperty("JButton.buttonType", "toolBarButton");
        cancelBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        cancelBtn.setFont(cancelBtn.getFont().deriveFont(11f));
        cancelBtn.setMargin(new Insets(1, 4, 1, 4));
        cancelBtn.addActionListener(e -> setEditing(false));
        headerActions.add(cancelBtn);

        JButton saveBtn = new JButton("Save", new SaveIcon(14));
        saveBtn.putClientProperty("JButton.buttonType", "toolBarButton");
        saveBtn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        saveBtn.setFont(saveBtn.getFont().deriveFont(Font.BOLD, 11f));
        saveBtn.setMargin(new Insets(1, 4, 1, 4));
        saveBtn.addActionListener(e -> {
            if (editorViewer != null) {
                String newContent = editorViewer.getEditorContent();
                if (newContent != null && !newContent.isBlank()) {
                    String trimmed = newContent.trim();
                    updateContent(trimmed);
                    valueChanged(trimmed);
                }
            }
            setEditing(false);
        });
        headerActions.add(saveBtn);

        header.add(headerActions, BorderLayout.EAST);
        editorPanel.add(header, BorderLayout.NORTH);

        String uriText = (value != null) ? value.toString() : "";
        StringHandle handle = new StringHandle("uri.txt", uriText);
        Resource ephemeral = new Resource(handle);
        try {
            ephemeral.reloadIfNeeded();
        } catch (Exception ignored) {
        }

        ResourceUI ui = ResourceUiRegistry.getInstance().getResourceUI();
        if (ui != null) {
            JComponent comp = (agiPanel != null)
                    ? ui.createContent(ephemeral, agiPanel)
                    : ui.createContent(ephemeral, (parentRenderer != null && call != null && call.getAgi() != null && call.getAgi().getConfig() != null)
                            ? call.getAgi().getConfig().getAsiContainer() : null);

            if (comp instanceof AbstractTextResourceViewer atv) {
                this.editorViewer = atv;
                atv.setVerticalScrollEnabled(false);
                atv.setPreviewAsEditor(true);
                atv.setToolbarVisible(false);
                atv.setReadOnly(false);
                atv.setEditing(true);

                atv.setSaveAction(newText -> {
                    if (newText != null && !newText.isBlank()) {
                        String trimmed = newText.trim();
                        updateContent(trimmed);
                        valueChanged(trimmed);
                    }
                    setEditing(false);
                });
                editorPanel.add(atv, BorderLayout.CENTER);
            }
        }
    }

    /**
     * Returns the prefix for the editor title header.
     * Subclasses (like PathParameterRenderer) can customize this.
     *
     * @return The title prefix string.
     */
    protected String getEditorTitlePrefix() {
        return "Editing: ";
    }

    /**
     * Resolves the simple file name from the URI.
     *
     * @return The simple name.
     */
    private String getDisplayName() {
        if (value == null) {
            return "null";
        }
        String str = value.toString();
        int lastSlash = str.lastIndexOf('/');
        if (lastSlash != -1 && lastSlash < str.length() - 1) {
            return str.substring(lastSlash + 1);
        }
        return str;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateContent(Object value) {
        this.value = value;
        nameLabel.setText(getDisplayName());
        nameLabel.setToolTipText(value != null ? value.toString() : null);
    }

    /**
     * {@inheritDoc}
     * <p>Renders the single chip for the bound URI value.</p>
     */
    @Override
    public boolean render() {
        updateContent(value);
        return true;
    }
}
