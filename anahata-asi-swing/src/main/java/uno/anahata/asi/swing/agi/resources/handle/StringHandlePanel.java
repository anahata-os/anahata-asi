/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.handle;

import javax.swing.JLabel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.handle.StringHandle;

/**
 * A specialized metadata panel for the {@link StringHandle}.
 * <p>
 * This panel exposes virtual memory attributes of snippets, such as 
 * their memory URI and snippet-specific origins.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class StringHandlePanel extends AbstractHandlePanel<StringHandle> {

    private final JLabel uriLabel = new JLabel();

    /**
     * Constructs a new StringHandlePanel.
     */
    public StringHandlePanel() {
        addProperty("Memory URI:", uriLabel);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Synchronizes labels with the snippet's memory state.</p>
     */
    @Override
    public void refresh() {
        super.refresh();
        if (handle != null) {
            uriLabel.setText(handle.getUri().toString());
        }
    }
}
