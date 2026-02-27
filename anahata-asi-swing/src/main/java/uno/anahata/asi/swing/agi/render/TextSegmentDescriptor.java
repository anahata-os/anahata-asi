/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import javax.swing.text.EditorKit;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.editorkit.EditorKitProvider;

/**
 * A record that describes a segment of text within a {@link TextPartPanel}.
 * It includes the segment's type, its content, and optionally the language for code blocks.
 *
 * @param type The {@link TextSegmentType} of the segment (TEXT or CODE).
 * @param content The raw text content of the segment.
 * @param language The programming language for code segments (null for text segments).
 *
 * @author anahata
 */
@Slf4j
public record TextSegmentDescriptor(TextSegmentType type, String content, String language) {

    /**
     * Creates and returns an appropriate {@link AbstractTextSegmentRenderer} for this descriptor.
     *
     * @param agiPanel The agi panel instance.
     * @param isThought True if the text represents a model thought, false otherwise.
     * @return A concrete instance of {@link AbstractTextSegmentRenderer}.
     */
    public AbstractTextSegmentRenderer createRenderer(AgiPanel agiPanel, boolean isThought) {
        return switch (type) {
            case TEXT -> new MarkupTextSegmentRenderer(agiPanel, content, isThought);
            case CODE -> createCodeBlockRenderer(agiPanel);
        };
    }

    private AbstractTextSegmentRenderer createCodeBlockRenderer(AgiPanel agiPanel) {
        EditorKitProvider editorKitProvider = agiPanel.getAgiConfig().getEditorKitProvider();
        if (editorKitProvider != null) {
            try {
                EditorKit kit = editorKitProvider.getEditorKitForLanguage(language);
                if (kit != null) {
                    return new JEditorPaneCodeBlockSegmentRenderer(agiPanel, content, language, kit);
                }
            } catch (Exception e) {
                log.error("Failed to obtain EditorKit for language: {}", language, e);
            }
        }
        return new RSyntaxTextAreaCodeBlockSegmentRenderer(agiPanel, content, language);
    }
}
