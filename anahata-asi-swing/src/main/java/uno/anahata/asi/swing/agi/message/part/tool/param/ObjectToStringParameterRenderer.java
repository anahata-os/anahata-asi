/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.message.part.tool.param;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.agi.resources.ResourceUI;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A specialized parameter renderer that converts any Java object to a string 
 * and displays it using a high-fidelity IDE viewer.
 * <p>
 * This implementation authoritatively leverages the {@link AbstractTextResourceViewer} 
 * directly, bypassing the markdown segment middle-man to provide a professional, 
 * host-aware editor for tool parameters.
 * </p>
 * <p>
 * <b>Representational Fidelity:</b> Uses the polymorphic {@link TextUtils#resolveContentString} 
 * to handle Strings, Enums, Collections, and Arrays with 100% technical purity.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class ObjectToStringParameterRenderer implements ParameterRenderer<Object> {
    
    /** The parent panel. */
    private AgiPanel agiPanel;
    /** The main container panel. */
    private final JPanel container = new JPanel(new BorderLayout());
    /** The high-fidelity viewer. */
    @Getter
    private AbstractTextResourceViewer viewer;

    /** The programming language for syntax highlighting. */
    @Getter @Setter
    private String language = "text";
    /** Whether the user is allowed to edit this parameter. */
    @Getter @Setter
    private boolean editable = false;

    /** No-arg constructor for factory instantiation. */
    public ObjectToStringParameterRenderer() {
        container.setOpaque(false);
        container.setBorder(BorderFactory.createLineBorder(new Color(200, 200, 200), 1, true));
    }

    /** {@inheritDoc} 
     * <p>Implementation details: Initializes the high-fidelity pipeline by resolving the 
     * object to a string, wrapping it in a virtual resource, and requesting a viewer.</p>
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, Object value) {
        this.agiPanel = agiPanel;
        
        // 1. Resolve Purity: Convert object to string using authoritative utility
        String content = TextUtils.resolveContentString(value);
        String mimeType = StringHandle.resolveMimeType(language);
        
        // 2. Initialize High-Fidelity Sense
        StringHandle handle = new StringHandle("param." + language, mimeType, content);
        Resource ephemeral = new Resource(handle);
        
        ResourceUI strategy = ResourceUiRegistry.getInstance().getResourceUI();
        if (strategy != null) {
            JComponent contentComp = strategy.createContent(ephemeral, agiPanel);
            if (contentComp instanceof AbstractTextResourceViewer atv) {
                this.viewer = atv;
                viewer.setToolbarVisible(false); // Clean UI
                viewer.setVerticalScrollEnabled(false); // Conversation passthrough
                viewer.setPreviewAsEditor(true); // Constant IDE fidelity
                viewer.setEditing(false); // Start read-only
                
                // WIRE PERSISTENCE: Save refinements back to the tool call
                viewer.setSaveAction(contentStr -> {
                    call.setModifiedArgument(paramName, contentStr);
                    viewer.setEditing(false);
                });
            }
        }

        // 3. Assemble UI
        container.removeAll();
        container.add(createActionStrip(content), BorderLayout.NORTH);
        if (viewer != null) {
            container.add(viewer, BorderLayout.CENTER);
        }
    }

    /** {@inheritDoc} */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /** {@inheritDoc} 
     * <p>Implementation details: Updates the underlying virtual resource handle 
     * with the new object's string representation.</p>
     */
    @Override
    public void updateContent(Object value) {
        if (viewer != null && viewer.getResource().getHandle() instanceof StringHandle sh) {
            try {
                String content = TextUtils.resolveContentString(value);
                sh.write(content);
                viewer.getResource().reloadIfNeeded();
            } catch (Exception e) {
                log.error("Failed to update parameter content", e);
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean render() {
        container.revalidate();
        container.repaint();
        return true;
    }

    /**
     * Creates a minimal, faint header with Copy and (if enabled) Edit actions.
     * @param initialValue The initial string to copy.
     * @return The header panel.
     */
    private JPanel createActionStrip(String initialValue) {
        JPanel header = new JPanel(new FlowLayout(FlowLayout.RIGHT, 5, 2));
        header.setOpaque(true);
        header.setBackground(new Color(240, 240, 240, 180));
        header.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, new Color(210, 210, 210)));

        JButton copyBtn = new JButton("Copy", new CopyIcon(12));
        copyBtn.setFont(new Font("SansSerif", Font.PLAIN, 11));
        copyBtn.setMargin(new java.awt.Insets(1, 5, 1, 5));
        copyBtn.setFocusPainted(false);
        copyBtn.addActionListener(e -> {
            String current = (viewer != null) ? viewer.getEditorContent() : initialValue;
            SwingUtils.copyToClipboard(current);
        });
        header.add(copyBtn);

        if (editable) {
            JButton editBtn = new JButton("Edit");
            editBtn.setFont(new Font("SansSerif", Font.PLAIN, 11));
            editBtn.setMargin(new java.awt.Insets(1, 5, 1, 5));
            editBtn.setFocusPainted(false);
            editBtn.addActionListener(e -> {
                if (viewer != null) {
                    viewer.setEditing(!viewer.isEditing());
                    editBtn.setText(viewer.isEditing() ? "Cancel" : "Edit");
                }
            });
            header.add(editBtn);
        }

        return header;
    }
}
