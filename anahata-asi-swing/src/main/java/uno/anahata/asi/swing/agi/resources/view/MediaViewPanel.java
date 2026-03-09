/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources.view;

import javax.swing.JLabel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.resource.view.MediaView;

/**
 * A specialized metadata panel for the {@link MediaView}.
 * <p>
 * This panel provides metadata for blob-based resources, including binary 
 * size and a placeholder for future image scaling factors.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class MediaViewPanel extends AbstractViewPanel<MediaView> {

    private final JLabel sizeLabel = new JLabel();
    private final JSpinner scaleSpinner;

    /**
     * Constructs a new MediaViewPanel.
     */
    public MediaViewPanel() {
        scaleSpinner = new JSpinner(new SpinnerNumberModel(1.0, 0.1, 10.0, 0.1));
        scaleSpinner.setEnabled(false); // Feature planned: image resizing
        
        addProperty("Data Size:", sizeLabel);
        addProperty("Scale Factor:", scaleSpinner);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Displays the raw byte size of the cached media data 
     * and ensures the visibility is public to match the orchestrator's contract.</p>
     */
    @Override
    public void refresh() {
        if (view != null && view.getCachedData() != null) {
            sizeLabel.setText(TextUtils.formatSize(view.getCachedData().length));
        } else {
            sizeLabel.setText("Not Loaded");
        }
    }
}
