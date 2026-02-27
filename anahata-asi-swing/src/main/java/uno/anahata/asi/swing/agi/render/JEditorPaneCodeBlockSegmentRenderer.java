/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.text.EditorKit;
import javax.swing.text.View;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * JEditorPane implementation of a code block segment renderer, using NetBeans EditorKits.
 *
 * @author anahata
 */
@Slf4j
public class JEditorPaneCodeBlockSegmentRenderer extends AbstractCodeBlockSegmentRenderer {

    /** The EditorKit used for syntax highlighting. */
    private final EditorKit kit;

    /**
     * Constructs a new JEditorPaneCodeBlockSegmentRenderer.
     *
     * @param agiPanel The agi panel instance.
     * @param initialContent The initial code content.
     * @param language The programming language.
     * @param kit The EditorKit to use.
     */
    public JEditorPaneCodeBlockSegmentRenderer(AgiPanel agiPanel, String initialContent, String language, EditorKit kit) {
        super(agiPanel, initialContent, language);
        this.kit = kit;
    }

    /**
     * {@inheritDoc}
     * Creates a JEditorPane configured with the provided EditorKit and custom
     * preferred size calculation to handle height correctly.
     */
    @Override
    protected JComponent createInnerComponent() {
        JEditorPane codeEditor = new JEditorPane() {
            @Override
            public boolean getScrollableTracksViewportWidth() {
                // Disable line wrapping to prevent height inflation in constrained viewports
                return false; 
            }

            @Override
            public Dimension getPreferredSize() {
                Dimension d = super.getPreferredSize();
                if (!editing) {
                    try {
                        View v = getUI().getRootView(this);
                        if (v != null) {
                            // Use a large width to avoid wrapping during height calculation
                            v.setSize(3000, Integer.MAX_VALUE); 
                            
                            int length = getDocument().getLength();
                            if (length > 0) {
                                // modelToView is the most reliable way to find the actual bottom of the text.
                                Rectangle r = modelToView(length - 1);
                                if (r != null) {
                                    d.height = r.y + r.height + 5; 
                                    return d;
                                }
                            }
                            // Fallback to span if modelToView fails
                            d.height = (int) v.getPreferredSpan(View.Y_AXIS) + 5;
                        }
                    } catch (Exception e) {
                        log.error("Failed to calculate preferred size for JEditorPane", e);
                    }
                }
                return d;
            }
        };
        codeEditor.setEditable(false);
        codeEditor.setEditorKit(kit);
        codeEditor.setOpaque(false);
        codeEditor.setBackground(new Color(0, 0, 0, 0));
        codeEditor.setMargin(new java.awt.Insets(0, 0, 0, 0));
        codeEditor.getDocument().putProperty("mimeType", kit.getContentType());
        log.info("JEditorPane initialized with kit: {} for language: {}", kit.getContentType(), language);
        return codeEditor;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void updateComponentContent(String content) {
        try {
            ((JEditorPane) innerComponent).setText(content);
        } catch (Exception e) {
            log.error("Failed to update JEditorPane content", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getCurrentContentFromComponent() {
        return ((JEditorPane) innerComponent).getText();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        ((JEditorPane) innerComponent).setEditable(editable);
    }
}
