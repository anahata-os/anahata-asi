/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.handle;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.time.Instant;
import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TimeUtils;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A specialized metadata panel for the {@link ResourceHandle} layer.
 * <p>
 * This class authoritatively displays common attributes from the connectivity
 * interface, such as URI, MIME type, existence status, and capabilities. It
 * serves as a concrete base for specialized handles and a fallback for unknown
 * types.
 * </p>
 * <p>
 * <b>Capabilities:</b> Virtual and Writable flags are displayed as non-editable
 * checkboxes to maintain schema integrity.
 * </p>
 *
 * @param <H> The handle type this panel renders.
 * @author anahata
 */
@Slf4j
public class AbstractHandlePanel<H extends ResourceHandle> extends JPanel {

    /**
     * The current handle being displayed.
     */
    @Getter
    protected H handle;

    /**
     * The layout constraints for property labels/fields.
     */
    protected final GridBagConstraints gbc = new GridBagConstraints();

    /**
     * Label for the implementation class FQN of the handle.
     */
    protected final JLabel classLabel = new JLabel();
    /**
     * Text field for the authoritative resource URI, read-only.
     */
    protected final JTextField uriField = createReadOnlyField();
    /**
     * Label for the detected MIME type of the underlying data.
     */
    protected final JLabel mimeLabel = new JLabel();
    /**
     * Label for the real-time connectivity status (ONLINE/OFFLINE).
     */
    protected final JLabel statusLabel = new JLabel();
    /**
     * Label for the physical size of the resource.
     */
    protected final JLabel sizeLabel = new JLabel();
    /**
     * Label for the last modified timestamp of the resource.
     */
    protected final JLabel modifiedLabel = new JLabel();
    /**
     * Non-editable checkbox indicating if the resource is purely memory-backed
     * (Virtual).
     */
    protected final JCheckBox virtualBox = new JCheckBox("Virtual");
    /**
     * Non-editable checkbox indicating if the resource supports mutation
     * operations (Writable).
     */
    protected final JCheckBox writableBox = new JCheckBox("Writable");

    /**
     * Constructs a new handle panel with a standard GridBag layout and
     * initializes common metadata components.
     */
    public AbstractHandlePanel() {
        setLayout(new GridBagLayout());
        setBorder(BorderFactory.createTitledBorder(null, "Handle (Connectivity)", TitledBorder.LEFT, TitledBorder.TOP, getFont().deriveFont(Font.BOLD)));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 5, 2, 5);
        gbc.fill = GridBagConstraints.NONE;

        // Initialize common fields
        virtualBox.setEnabled(false);
        writableBox.setEnabled(false);

        addProperty("Class:", classLabel);
        classLabel.setForeground(Color.GRAY);
        addProperty("URI:", uriField);
        addProperty("MIME:", mimeLabel);
        addProperty("Status:", statusLabel);
        addProperty("Size:", sizeLabel);
        addProperty("Modified:", modifiedLabel);

        JPanel capsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 0));
        capsPanel.setOpaque(false);
        capsPanel.add(virtualBox);
        capsPanel.add(writableBox);
        addProperty("Capabilities:", capsPanel);
    }

    /**
     * Helper for creating unbordered, read-only text fields for metadata.
     *
     * @return The configured JTextField.
     */
    protected final JTextField createReadOnlyField() {
        JTextField f = new JTextField();
        f.setEditable(false);
        f.setBorder(null);
        f.setOpaque(false);
        f.setMinimumSize(new Dimension(50, 22));
        f.setPreferredSize(new Dimension(150, 22));
        return f;
    }

    /**
     * Sets the handle to display and triggers a UI refresh.
     *
     * @param handle The handle instance.
     */
    public final void setHandle(H handle) {
        this.handle = handle;
        if (handle != null) {
            refresh();
        }
    }

    /**
     * Helper to add a labelled property to the grid.
     *
     * @param label The property label.
     * @param component The value component.
     */
    protected void addProperty(String label, Component component) {
        gbc.gridx = 0;
        gbc.weightx = 0;
        add(new JLabel(label), gbc);

        gbc.gridx = 1;
        gbc.weightx = 1.0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        add(component, gbc);

        gbc.gridy++;
        gbc.fill = GridBagConstraints.NONE;
    }

    /**
     * Refreshes the common metadata components. Subclasses should call this in
     * their own refresh implementation to maintain consistency.
     */
    public void refresh() {
        if (handle == null) {
            return;
        }

        classLabel.setText(handle.getClass().getName());
        uriField.setText(handle.getUri().toString());
        virtualBox.setSelected(handle.isVirtual());

        statusLabel.setText("Loading...");
        statusLabel.setForeground(Color.GRAY);
        sizeLabel.setText("Loading...");
        modifiedLabel.setText("Loading...");
        mimeLabel.setText("Loading...");
        writableBox.setEnabled(false);

        AgiPanel agiPanel = (AgiPanel) SwingUtilities.getAncestorOfClass(AgiPanel.class, this);
        if (agiPanel != null) {
            new SwingTask<Object[]>(agiPanel, "Fetching Metadata", () -> {
                boolean exists = handle.exists();
                long size = handle.length();
                long lastModified = handle.getLastModified();
                String mime = handle.getMimeType();
                boolean writable = handle.isWritable();
                return new Object[]{exists, size, lastModified, mime, writable};
            }, results -> {
                boolean exists = (boolean) results[0];
                long size = (long) results[1];
                long lastModified = (long) results[2];
                String mime = (String) results[3];
                boolean writable = (boolean) results[4];

                mimeLabel.setText(mime);
                statusLabel.setText(exists ? "ONLINE" : "OFFLINE");
                statusLabel.setForeground(exists ? new Color(0, 150, 0) : Color.RED);
                sizeLabel.setText(size >= 0 ? uno.anahata.asi.internal.TextUtils.formatSize(size) : "Unknown");
                modifiedLabel.setText(TimeUtils.formatSmartTimestamp(Instant.ofEpochMilli(lastModified)));
                writableBox.setSelected(writable);
                onRefreshComplete();
            }, error -> {
                statusLabel.setText("ERROR");
                statusLabel.setForeground(Color.RED);
            }, false).start();
        }
    }

    /**
     * Hook invoked after the asynchronous refresh of handle metadata is
     * complete.
     */
    protected void onRefreshComplete() {
    }
}
