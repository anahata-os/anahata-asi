/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.ui.resources;

import java.awt.Color;
import javax.swing.JLabel;
import javax.swing.JTextField;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.ide.resources.handle.IdeHandle;
import uno.anahata.asi.swing.agi.resources.handle.AbstractHandlePanel;

/**
 * Universal metadata panel for {@link IdeHandle} instances.
 * <p>
 * Displays foundational IDE handle connectivity, filesystem path,
 * and validity status for all host IDE environments.
 * </p>
 *
 * @param <H> The specialized IDE handle type.
 * @author anahata
 */
@Slf4j
public class IdeHandlePanel<H extends IdeHandle> extends AbstractHandlePanel<H> {

    /**
     * Label indicating whether the IDE considers the underlying handle valid.
     */
    protected final JLabel validityLabel = new JLabel();

    /**
     * Read-only field displaying the absolute filesystem path, if applicable.
     */
    protected final JTextField pathField = createReadOnlyField();

    /**
     * Constructs a new IdeHandlePanel and initializes default property fields.
     */
    public IdeHandlePanel() {
        addProperty("Path:", pathField);
        addProperty("IDE Validity:", validityLabel);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Populates the standard path and validity attributes from the {@link IdeHandle}.
     * </p>
     */
    @Override
    public void refresh() {
        super.refresh();
        if (handle == null) {
            return;
        }

        pathField.setText(handle.getPath() != null ? handle.getPath() : "N/A");
        if (handle.exists()) {
            validityLabel.setText("VALID");
            validityLabel.setForeground(new Color(0, 150, 0));
        } else {
            validityLabel.setText("OFFLINE (Unresolved)");
            validityLabel.setForeground(Color.RED);
        }
    }
}
