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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.UIManager;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.icons.ActionIconKey;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.SaveIcon;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Common abstract base class for chip/pill parameter renderers (e.g. URIs,
 * Paths, Resource UUIDs).
 * <p>
 * Manages the dual-mode CardLayout lifecycle:
 * </p>
 * <ul>
 * <li><b>Compact Pill View:</b> High-density pill featuring Copy, Name Label
 * (click-to-edit), Open in IDE, Edit, and Delete actions.</li>
 * <li><b>In-Place Editor View:</b> Dynamically expands across the container
 * width hosting the native {@link AbstractTextResourceViewer} with syntax
 * highlighting, line numbers, Cancel, and Save actions.</li>
 * </ul>
 *
 *
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public abstract class AbstractChipParameterRenderer extends AbstractParameterRenderer<Object> {

    /**
     * CardLayout for toggling between the compact pill and in-place editor.
     */
    protected final CardLayout cardLayout = new CardLayout();

    /**
     * The root component hosting the card layout.
     */
    protected final JPanel container = new JPanel(cardLayout) {
        @Override
        public Dimension getPreferredSize() {
            if (editing) {
                Dimension ps = editorPanel.getPreferredSize();
                Container parent = getParent();
                int w = (parent != null && parent.getWidth() > 0) ? parent.getWidth() - 15 : 600;
                return new Dimension(w, ps.height);
            }
            return pillRow.getPreferredSize();
        }

        @Override
        public void updateUI() {
            super.updateUI();
            if (pillPanel != null) {
                applyColors();
            }
        }
    };

    /**
     * The compact chip panel.
     */
    protected final JPanel pillPanel = new JPanel(new BorderLayout(8, 0));

    /**
     * The editor wrapper panel.
     */
    protected final JPanel editorPanel = new JPanel(new BorderLayout());

    /**
     * Label displaying the primary name of the chip.
     */
    protected final JLabel nameLabel = new JLabel();

    /**
     * Whether the chip is currently in edit mode.
     */
    protected boolean editing = false;

    /**
     * Active high-fidelity viewer when in edit mode.
     */
    protected AbstractTextResourceViewer editorViewer;

    /**
     * Button to copy content to clipboard.
     */
    protected JButton copyBtn;

    /**
     * Button to toggle in-place editing.
     */
    protected JButton editBtn;

    /**
     * Button to open the resource or URI in the host environment.
     */
    protected JButton openBtn;

    /**
     * Button to remove this chip from its enclosing parent container.
     */
    protected JButton deleteBtn;

    /**
     * Panel wrapping the pill in a flow layout so it does not stretch across the container when standalone.
     */
    protected final JPanel pillRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));

    /**
     * Constructs a new AbstractChipParameterRenderer with standard pill wiring.
     */
    public AbstractChipParameterRenderer() {
        container.setOpaque(false);
        pillPanel.setOpaque(true);

        nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD));
        nameLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        nameLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                setEditing(true);
            }
        });

        pillRow.setOpaque(false);
        pillRow.add(pillPanel);

        editorPanel.setOpaque(false);

        container.add(pillRow, "pill");
        container.add(editorPanel, "editor");

        applyColors();
    }

    /**
     * {@inheritDoc}
     * <p>Initializes the renderer and constructs pill action buttons directly via SwingAgiConfig.</p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, Object value) {
        super.init(agiPanel, call, paramName, value);
        SwingAgiConfig config = agiPanel.getAgiConfig();

        pillPanel.removeAll();

        copyBtn = config.createSquareButton(ActionIconKey.COPY, 14, "Copy to clipboard");
        copyBtn.addActionListener(e -> SwingUtils.copyToClipboard(getClipboardContent()));
        pillPanel.add(copyBtn, BorderLayout.WEST);

        JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 0));
        centerPanel.setOpaque(false);
        centerPanel.add(nameLabel);

        openBtn = config.createSquareButton(ActionIconKey.EXTERNAL, 14, "Open");
        openBtn.addActionListener(e -> onOpen());
        centerPanel.add(openBtn);

        editBtn = config.createSquareButton(ActionIconKey.EDIT, 14, "Edit in-place");
        editBtn.addActionListener(e -> setEditing(true));
        centerPanel.add(editBtn);

        pillPanel.add(centerPanel, BorderLayout.CENTER);

        deleteBtn = config.createSquareButton(ActionIconKey.DELETE, 14, "Remove");
        deleteBtn.addActionListener(e -> deleteSelf());
        deleteBtn.setVisible(parentRenderer != null);
        pillPanel.add(deleteBtn, BorderLayout.EAST);
    }

    /**
     * Resolves the concise display name for the chip label.
     *
     * @return The display name string.
     */
    protected abstract String getDisplayName();

    /**
     * Resolves the detailed tooltip text for the chip.
     *
     * @return The tooltip string.
     */
    protected abstract String getTooltipText();

    /**
     * Action performed when the user clicks the Open button.
     */
    protected abstract void onOpen();

    /**
     * Resolves the string to copy to the clipboard.
     *
     * @return The text content to copy.
     */
    protected String getClipboardContent() {
        return value != null ? value.toString() : "";
    }

    /**
     * Returns the prefix for the editor title header.
     *
     * @return The title prefix string.
     */
    protected String getEditorTitlePrefix() {
        return "Editing: ";
    }

    /**
     * Returns the virtual file name used for the ephemeral editor resource.
     *
     * @return The ephemeral file name.
     */
    protected String getEphemeralFileName() {
        return "value.txt";
    }

    /**
     * Resolves the background color for the compact pill panel.
     *
     * @return The background Color.
     */
    protected Color getPillBackground() {
        Color bg = UIManager.getColor("Button.background");
        return bg != null ? bg : new Color(230, 235, 245);
    }

    /**
     * Resolves the border color for the compact pill panel.
     *
     * @return The border Color.
     */
    protected Color getPillBorderColor() {
        Color border = UIManager.getColor("Component.borderColor");
        if (border == null) {
            border = UIManager.getColor("Separator.foreground");
        }
        return border != null ? border : Color.GRAY;
    }

    /**
     * Applies theme-aware colors from UIManager to support light and dark Look
     * and Feels.
     */
    protected void applyColors() {
        Color bg = getPillBackground();
        Color border = getPillBorderColor();
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
     * Toggles the editing state and swaps between the pill view and editor
     * view.
     *
     * @param editing true to enter in-place edit mode, false to return to pill
     * view.
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
     * Mounts the high-fidelity NetBeans/RSyntax editor for modifying the raw
     * value string.
     */
    protected void setupEditor() {
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

        SwingAgiConfig config = agiPanel.getAgiConfig();
        JButton cancelBtn = new JButton("Cancel", config.getActionIcon(ActionIconKey.CANCEL, 14));
        cancelBtn.setFont(cancelBtn.getFont().deriveFont(11f));
        cancelBtn.addActionListener(e -> setEditing(false));
        headerActions.add(cancelBtn);

        JButton saveBtn = new JButton("Save", config.getActionIcon(ActionIconKey.SAVE, 14));
        saveBtn.setFont(saveBtn.getFont().deriveFont(Font.BOLD, 11f));
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

        String rawText = (value != null) ? value.toString() : "";
        StringHandle handle = new StringHandle(getEphemeralFileName(), rawText);
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
        nameLabel.setToolTipText(getTooltipText());
    }

    /**
     * {@inheritDoc}
     * <p>Updates delete button visibility when parent renderer is assigned.</p>
     */
    @Override
    public void setParentRenderer(ParameterRenderer<?> parentRenderer) {
        super.setParentRenderer(parentRenderer);
        if (deleteBtn != null) {
            deleteBtn.setVisible(parentRenderer != null);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Renders the single chip for the bound value.</p>
     */
    @Override
    public boolean render() {
        updateContent(value);
        if (deleteBtn != null) {
            deleteBtn.setVisible(parentRenderer != null);
        }
        return true;
    }
}
