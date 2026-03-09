/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.handle;

import javax.swing.JTextField;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.view.PathHandle;

/**
 * A specialized metadata panel for the {@link PathHandle}.
 * <p>
 * This panel exposes filesystem-specific attributes of local resources, 
 * primarily the absolute physical path on the host OS.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class PathHandlePanel extends AbstractHandlePanel<PathHandle> {

    private final JTextField pathField;

    /**
     * Constructs a new PathHandlePanel.
     */
    public PathHandlePanel() {
        pathField = createReadOnlyField();
        addProperty("Physical Path:", pathField);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Synchronizes labels with the filesystem metadata 
     * and ensures the absolute path is displayed.</p>
     */
    @Override
    public void refresh() {
        super.refresh();
        if (handle != null) {
            pathField.setText(handle.getPath());
        }
    }
}
