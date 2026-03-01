/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.mine;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.EditorKit;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.AbstractCodeBlockSegmentRenderer;
import uno.anahata.asi.swing.agi.render.JEditorPaneCodeBlockSegmentRenderer;
import uno.anahata.asi.swing.agi.render.editorkit.EditorKitProvider;

/**
 * NetBeans-specific implementation of {@link EditorKitProvider}.
 * <p>
 * This implementation leverages the NetBeans {@code MimeLookup} and {@code FileUtil}
 * APIs to provide high-fidelity syntax highlighting and language detection
 * within the IDE.
 * </p>
 *
 * @author anahata
 */
public class NetBeansEditorKitProvider implements EditorKitProvider {
    private static final Logger logger = Logger.getLogger(NetBeansEditorKitProvider.class.getName());

    private final Map<String, String> languageToMimeTypeMap;

    /**
     * Constructs a new NetBeansEditorKitProvider and initializes the 
     * language-to-MIME-type mapping cache.
     */
    public NetBeansEditorKitProvider() {
        logger.log(Level.INFO, "Initializing NetBeansEditorKitProvider language cache...");
        this.languageToMimeTypeMap = new ConcurrentHashMap<>();

        // 1. Start with the hardcoded map as a baseline (the fallback/default).
        Map<String, String> hardcodedMap = Map.of(
            "java", "text/x-java",
            "xml", "text/xml",
            "html", "text/html",
            "css", "text/css",
            "javascript", "text/javascript",
            "json", "text/x-json",
            "sql", "text/x-sql",
            "properties", "text/x-properties",
            "bash", "text/plain" 
        );
        languageToMimeTypeMap.putAll(hardcodedMap);
        languageToMimeTypeMap.putAll(MimeUtils.getExtensionToMimeTypeMap());
        logger.log(Level.INFO, "Cache initialization complete. Final cache size: {0}", languageToMimeTypeMap.size());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Resolves the NetBeans MIME type for the given language ID and retrieves 
     * the corresponding {@link EditorKit} from the global {@code MimeLookup}.
     * </p>
     */
    @Override
    public EditorKit getEditorKitForLanguage(String language) {
        String langLower = (language == null) ? "" : language.toLowerCase().trim();
        String mimeType = languageToMimeTypeMap.get(langLower);

        if (mimeType != null) {
            EditorKit kit = MimeLookup.getLookup(mimeType).lookup(EditorKit.class);
            if (kit != null) {
                return kit;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Delegates to {@link FileUtil#getMIMEType} for authoritative detection 
     * and performs a reverse-lookup in the local cache to find a simplified 
     * language ID.
     * </p>
     */
    @Override
    public String getLanguageForFile(String filename) {
        // Use NetBeans FileUtil for authoritative MIME detection
        String mime = FileUtil.getMIMEType(filename);
        if (mime == null) {
            return "text";
        }
        
        // Try to find a reverse mapping in our cache first
        for (Map.Entry<String, String> entry : languageToMimeTypeMap.entrySet()) {
            if (entry.getValue().equalsIgnoreCase(mime)) {
                return entry.getKey();
            }
        }
        
        // Fallback: use the subtype part of the mime (e.g., x-java -> java)
        int slash = mime.lastIndexOf('/');
        String sub = (slash != -1) ? mime.substring(slash + 1) : mime;
        return sub.replace("x-", "");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details:
     * Instantiates a {@link JEditorPaneCodeBlockSegmentRenderer} to provide 
     * full NetBeans editor fidelity, including project-aware semantic 
     * highlighting and annotations.
     * </p>
     */
    @Override
    public AbstractCodeBlockSegmentRenderer createRenderer(AgiPanel agiPanel, String content, String language) {
        EditorKit kit = getEditorKitForLanguage(language);
        // Fallback to plain text kit if specific kit is missing
        if (kit == null) {
            kit = MimeLookup.getLookup("text/plain").lookup(EditorKit.class);
        }
        return new JEditorPaneCodeBlockSegmentRenderer(agiPanel, content, language, kit);
    }
}
