/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.text.EditorKit;
import javax.swing.text.View;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * JEditorPane implementation of a code block segment renderer, using NetBeans EditorKits.
 *
 * @author anahata
 */
@Slf4j
public class JEditorPaneCodeBlockSegmentRenderer extends AbstractCodeBlockSegmentRenderer {

    private final EditorKit kit;

    public JEditorPaneCodeBlockSegmentRenderer(ChatPanel chatPanel, String initialContent, String language, EditorKit kit) {
        super(chatPanel, initialContent, language);
        this.kit = kit;
    }

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
                if (!editable) {
                    try {
                        View v = getUI().getRootView(this);
                        if (v != null) {
                            // Use a large width to avoid wrapping during height calculation
                            v.setSize(3000, Integer.MAX_VALUE); 
                            int lastOffset = getDocument().getLength();
                            if (lastOffset > 0) {
                                // modelToView is the most reliable way to find the actual bottom of the text
                                Rectangle r = modelToView(lastOffset);
                                if (r != null) {
                                    d.height = r.y + r.height + 2; // Minimal padding
                                    return d;
                                }
                            }
                            // Fallback to span if modelToView fails
                            d.height = (int) v.getPreferredSpan(View.Y_AXIS);
                        }
                    } catch (Exception e) {
                        // Fallback to super.getPreferredSize()
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

    @Override
    protected void updateComponentContent(String content) {
        ((JEditorPane) innerComponent).setText(content);
    }

    @Override
    protected String getCurrentContentFromComponent() {
        return ((JEditorPane) innerComponent).getText();
    }

    @Override
    protected void setComponentEditable(boolean editable) {
        ((JEditorPane) innerComponent).setEditable(editable);
    }
}
