/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render.editorkit;

import javax.swing.text.EditorKit;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.rtf.RTFEditorKit;

/**
 * A default implementation of EditorKitProvider that provides the standard
 * kits available in a vanilla Swing environment.
 *
 * @author anahata
 */
public class DefaultEditorKitProvider implements EditorKitProvider {

    
    @Override
    public EditorKit getEditorKitForLanguage(String language) {
        if ("html".equalsIgnoreCase(language)) {
            return new HTMLEditorKit();
        }
        if ("rtf".equalsIgnoreCase(language)) {
            return new RTFEditorKit();
        }
        // For any other language, return null to signal that no specific
        // editor kit is available, allowing the renderer to use a plain-text fallback.
        return null;
    }
}