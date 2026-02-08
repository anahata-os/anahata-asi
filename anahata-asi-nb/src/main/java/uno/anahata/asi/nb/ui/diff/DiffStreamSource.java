/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import org.netbeans.api.diff.Difference;
import org.netbeans.api.diff.StreamSource;

/**
 * A simple {@link StreamSource} implementation for providing string-based content to the NetBeans Diff API.
 * 
 * @author anahata
 */
public class DiffStreamSource extends StreamSource {
    private final String name;
    private final String title;
    private final String content;
    private final String mimeType;

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

    @Override
    public Writer createWriter(Difference[] conflicts) throws IOException {
        return null; // Read-only for now, cherry-picking is handled via checkboxes
    }
}
