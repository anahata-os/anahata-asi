/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.handle;

import java.time.Instant;
import javax.swing.JLabel;
import javax.swing.JTextField;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TimeUtils;
import uno.anahata.asi.agi.resource.handle.UrlHandle;

/**
 * A specialized metadata panel for the {@link UrlHandle}.
 * <p>
 * This panel exposes remote connectivity attributes, including the source 
 * URL and indicators for the network metadata cache.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class UrlHandlePanel extends AbstractHandlePanel<UrlHandle> {

    private final JTextField urlField;
    private final JLabel cacheLabel = new JLabel();

    /**
     * Constructs a new UrlHandlePanel.
     */
    public UrlHandlePanel() {
        urlField = createReadOnlyField();
        addProperty("Remote URL:", urlField);
        addProperty("Cache Status:", cacheLabel);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Synchronizes labels with the remote URL metadata 
     * and displays the status of the internal connection cache.</p>
     */
    @Override
    public void refresh() {
        super.refresh();
        if (handle != null) {
            urlField.setText(handle.getUrlString());
            // Since getLastModified() triggers the HEAD request and populates the cache
            cacheLabel.setText(handle.getLastModified() > 0 ? "Metadata Captured (HEAD)" : "Waiting for First Access");
        }
    }
}
