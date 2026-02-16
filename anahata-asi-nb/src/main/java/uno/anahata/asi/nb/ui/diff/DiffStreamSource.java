/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import javax.swing.text.Document;
import lombok.Getter;
import lombok.Setter;
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
public class DiffStreamSource extends StreamSource {
    private final String name;
    private final String title;
    private final String content;
    private final String mimeType;
    
    /** The live document associated with this source, used for synchronization. */
    @Getter @Setter
    private Document document;
    
    /** Whether this source should be considered editable by the diff visualizer. */
    @Getter @Setter
    private boolean editable = false;

    public DiffStreamSource(String name, String title, String content, String mimeType) {
        this.name = name;
        this.title = title;
        this.content = content;
        this.mimeType = mimeType;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
    public String getMIMEType() {
        return mimeType;
    }

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
     * provides a StringWriter to satisfy the visualizer's requirement for a 
     * writable target.
     */
    @Override
    public Writer createWriter(Difference[] conflicts) throws IOException {
        return isEditable() ? new StringWriter() : null;
    }

    @Override
    public boolean isEditable() {
        return editable;
    }

    /**
     * Provides a Lookup containing the Document if available. This is critical 
     * for the NetBeans Diff visualizer to perform live edits during a merge.
     */
    @Override
    public Lookup getLookup() {
        if (document != null) {
            return Lookups.fixed(document);
        }
        return super.getLookup();
    }
}
