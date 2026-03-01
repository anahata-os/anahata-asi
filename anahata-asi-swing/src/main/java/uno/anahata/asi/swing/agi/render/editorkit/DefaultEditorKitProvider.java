/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render.editorkit;

import java.io.File;
import java.util.Map;
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

    private static final Map<String, String> EXT_MAP = Map.ofEntries(
        Map.entry("java", "java"),
        Map.entry("py", "python"),
        Map.entry("xml", "xml"),
        Map.entry("html", "html"),
        Map.entry("js", "javascript"),
        Map.entry("ts", "typescript"),
        Map.entry("json", "json"),
        Map.entry("sql", "sql"),
        Map.entry("md", "markdown"),
        Map.entry("yaml", "yaml"),
        Map.entry("yml", "yaml"),
        Map.entry("sh", "shell"),
        Map.entry("properties", "properties")
    );

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
     * Robust language detection that prioritizes file extensions (for proposed files)
     * and falls back to Apache Tika for existing files.
     * </p>
     */
    @Override
    public String getLanguageForFile(String filename) {
        // 1. Primary: Extension Map (Works for non-existent files)
        int dot = filename.lastIndexOf('.');
        if (dot != -1) {
            String ext = filename.substring(dot + 1).toLowerCase();
            String lang = EXT_MAP.get(ext);
            if (lang != null) {
                log.info("Language detected via extension [{}]: {}", ext, lang);
                return lang;
            }
        }

        // 2. Secondary: Tika (Only if file exists)
        File f = new File(filename);
        if (f.exists()) {
            try {
                String mime = TikaUtils.detectMimeType(f);
                if (mime != null) {
                    int slash = mime.lastIndexOf('/');
                    String lang = (slash != -1) ? mime.substring(slash + 1) : mime;
                    lang = lang.replace("x-", "");
                    log.info("Language detected via Tika [{}]: {}", mime, lang);
                    return lang;
                }
            } catch (Exception e) {
                log.debug("Tika detection failed for: {}", filename);
            }
        }

        log.info("Language detection failed for [{}], falling back to 'text'.", filename);
        return "text";
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
