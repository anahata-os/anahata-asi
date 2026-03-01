/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render.editorkit;

import javax.swing.text.EditorKit;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.AbstractCodeBlockSegmentRenderer;

/**
 * An interface for providing an EditorKit for a given programming language.
 * This allows the generic UI to be configured with IDE-specific editor kits.
 *
 * @author anahata
 */
public interface EditorKitProvider {

    /**
     * Gets a Swing EditorKit suitable for rendering the specified language.
     *
     * @param language The programming language (e.g., "java", "html", "xml").
     * @return An EditorKit instance, or null if no specific kit is available.
     */
    EditorKit getEditorKitForLanguage(String language);

    /**
     * Maps a filename or extension to a language identifier suitable for 
     * syntax highlighting.
     * 
     * @param filename The name of the file or its path.
     * @return A language string (e.g., "java", "json"), or "text" as a fallback.
     */
    String getLanguageForFile(String filename);
    
    /**
     * Factory method to create an environment-appropriate code block renderer.
     * 
     * @param agiPanel The active agi panel.
     * @param content The initial content to render.
     * @param language The language identifier.
     * @return A concrete renderer instance.
     */
    AbstractCodeBlockSegmentRenderer createRenderer(AgiPanel agiPanel, String content, String language);
}
