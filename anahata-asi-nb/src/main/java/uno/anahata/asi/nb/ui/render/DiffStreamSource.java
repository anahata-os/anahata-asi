/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import javax.swing.SwingUtilities;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.diff.Difference;
import org.netbeans.api.diff.StreamSource;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;

/**
 * A flexible {@link StreamSource} implementation for providing string-based content 
 * to the NetBeans Diff API. It supports optional editability and live document
 * synchronization to trigger the Merger UI (red crosses and arrows).
 * 
 * @author anahata
 */
@Slf4j
public class DiffStreamSource extends StreamSource {
    /**
     * The display name of the resource.
     */
    private final String name;
    /**
     * The title to be displayed in the diff viewer pane header.
     */
    private final String title;
    /**
     * The raw string content of the source.
     */
    private final String content;
    /**
     * The MIME type of the content, used to locate the correct {@link javax.swing.text.EditorKit}.
     */
    private final String mimeType;
    
    /** The live document associated with this source, used for synchronization. */
    @Getter @Setter
    private Document document;
    
    /** Whether this source should be considered editable by the diff visualizer. */
    @Setter
    private boolean editable = false;

    /**
     * Constructs a new stream source for the NetBeans Diff API.
     *
     * @param name The name of the source.
     * @param title The title for the UI pane.
     * @param content The initial text content.
     * @param mimeType The MIME type for syntax highlighting.
     */
    public DiffStreamSource(String name, String title, String content, String mimeType) {
        this.name = name;
        this.title = title;
        this.content = content;
        this.mimeType = mimeType;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTitle() {
        return title;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getMIMEType() {
        return mimeType;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Reader createReader() throws IOException {
        if (document != null) {
            try {
                return new StringReader(document.getText(0, document.getLength()));
            } catch (Exception e) {
                throw new IOException(e);
            }
        }
        return new StringReader(content);
    }

    /**
     * Implementation details: If {@link #isEditable()} returns true, this method 
     * provides a StringWriter that syncs back to the {@link #document} on close.
     * This ensures that "Merge" actions in the UI are reflected in the live document.
     */
    @Override
    public Writer createWriter(Difference[] conflicts) throws IOException {
        if (!isEditable()) {
            return null;
        }
        
        return new StringWriter() {
            @Override
            public void close() throws IOException {
                super.close();
                String newText = toString();
                if (document != null) {
                    SwingUtilities.invokeLater(() -> {
                        try {
                            document.remove(0, document.getLength());
                            document.insertString(0, newText, null);
                        } catch (BadLocationException ex) {
                            log.error("Failed to sync merge writer to document", ex);
                        }
                    });
                }
            }
        };
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEditable() {
        return editable;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Lookup getLookup() {
        if (document != null) {
            return Lookups.fixed(document);
        }
        return super.getLookup();
    }
}
