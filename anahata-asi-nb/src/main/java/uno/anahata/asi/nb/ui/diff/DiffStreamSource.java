/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import lombok.Getter;
import lombok.Setter;
import org.netbeans.api.diff.Difference;
import org.netbeans.api.diff.StreamSource;

/**
 * A flexible {@link StreamSource} implementation for providing string-based content 
 * to the NetBeans Diff API. It supports optional editability to trigger the 
 * Merger UI (red crosses and arrows).
 * 
 * @author anahata
 */
public class DiffStreamSource extends StreamSource {
    private final String name;
    private final String title;
    private final String content;
    private final String mimeType;
    
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
        return new StringReader(content);
    }

    /**
     * Implementation details: If {@link #isEditable()} returns true, this method 
     * provides a StringWriter to satisfy the visualizer's requirement for a 
     * writable target, triggering the merge decorations.
     */
    @Override
    public Writer createWriter(Difference[] conflicts) throws IOException {
        return isEditable() ? new StringWriter() : null;
    }

    @Override
    public boolean isEditable() {
        return editable;
    }
}
