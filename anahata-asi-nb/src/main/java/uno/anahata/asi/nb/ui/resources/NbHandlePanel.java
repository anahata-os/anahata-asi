/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.resources;

import java.awt.Color;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTextField;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.ide.ui.resources.IdeHandlePanel;
import uno.anahata.asi.nb.resources.handle.NbHandle;

/**
 * A specialized metadata panel for the {@link NbHandle}.
 * <p>
 * This panel serves as a property sheet for NetBeans-specific file metadata,
 * visualizing IDE-level attributes such as file validity, absolute paths,
 * and storage origin (archive vs. local filesystem).
 * </p>
 */
@Slf4j
public class NbHandlePanel extends IdeHandlePanel<NbHandle> {

    /**
     * Checkbox indicating if the resource originates from a read-only archive (e.g., inside a JAR).
     */
    private final JCheckBox archiveBox = new JCheckBox("Archive Entry");
    /**
     * Read-only field displaying the full URI of the resource.
     */
    private final JTextField uriField = createReadOnlyField();

    /**
     * Constructs a new metadata panel and initializes the property layout.
     */
    public NbHandlePanel() {
        super();
        archiveBox.setEnabled(false);
        archiveBox.setOpaque(false);
        addProperty("URI:", uriField);
        addProperty("Storage:", archiveBox);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Extracts and styles IDE-specific validity and
     * storage attributes from the {@code NbHandle}.
     * </p>
     */
    @Override
    public void refresh() {
        super.refresh();
        if (handle == null) return;
        
        uriField.setText(handle.getUri().toString());
        pathField.setText(handle.getPath() != null ? handle.getPath() : "N/A");
        
        FileObject fo = handle.getFileObject();
        if (fo != null) {
            validityLabel.setText(fo.isValid() ? "VALID" : "INVALID");
            validityLabel.setForeground(fo.isValid() ? new Color(0, 150, 0) : Color.RED);
            try {
                archiveBox.setSelected(fo.getFileSystem().isReadOnly());
            } catch (Exception e) {
                archiveBox.setSelected(false);
            }
        } else {
            validityLabel.setText("OFFLINE (Unresolved)");
            validityLabel.setForeground(Color.RED);
        }
    }
}