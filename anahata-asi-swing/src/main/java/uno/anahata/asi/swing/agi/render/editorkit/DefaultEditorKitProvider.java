/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render.editorkit;

import java.io.File;
import javax.swing.text.EditorKit;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.rtf.RTFEditorKit;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.AbstractCodeBlockSegmentRenderer;
import uno.anahata.asi.swing.agi.render.RSyntaxTextAreaCodeBlockSegmentRenderer;

/**
 * A default implementation of {@link EditorKitProvider} that provides standard
 * Swing kits and uses Apache Tika for environment-agnostic language detection.
 * <p>
 * This implementation is used in standalone mode or as a fallback when no
 * IDE-specific provider is available.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class DefaultEditorKitProvider implements EditorKitProvider {

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Provides support for HTML and RTF using standard JDK kits.
     * </p>
     */
    @Override
    public EditorKit getEditorKitForLanguage(String language) {
        if ("html".equalsIgnoreCase(language)) {
            return new HTMLEditorKit();
        }
        if ("rtf".equalsIgnoreCase(language)) {
            return new RTFEditorKit();
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Leverages Apache Tika to detect MIME types from filenames and maps 
     * them to language identifiers suitable for {@link RSyntaxTextAreaCodeBlockSegmentRenderer}.
     * </p>
     */
    @Override
    public String getLanguageForFile(String filename) {
        try {
            // Tika returns MIME types, which RSyntaxTextArea handles well 
            // after our mapping logic.
            String mime = TikaUtils.detectMimeType(new File(filename));
            if (mime == null) {
                return "text";
            }
            // Strip the 'text/' or 'application/' prefix to get a language ID
            int slash = mime.lastIndexOf('/');
            String lang = (slash != -1) ? mime.substring(slash + 1) : mime;
            return lang.replace("x-", ""); // cleanup common mime prefixes
        } catch (Exception e) {
            log.debug("Tika detection failed for: {}", filename);
            return "text";
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Returns an {@link RSyntaxTextAreaCodeBlockSegmentRenderer} for 
     * cross-platform, high-quality syntax highlighting without IDE dependencies.
     * </p>
     */
    @Override
    public AbstractCodeBlockSegmentRenderer createRenderer(AgiPanel agiPanel, String content, String language) {
        return new RSyntaxTextAreaCodeBlockSegmentRenderer(agiPanel, content, language);
    }
}
