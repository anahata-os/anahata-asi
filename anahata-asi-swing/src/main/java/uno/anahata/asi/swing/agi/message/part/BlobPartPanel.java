/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message.part;

import java.awt.BorderLayout;
import java.net.URI;
import java.util.Arrays;
import java.util.Objects;
import javax.swing.JLabel;
import lombok.NonNull;
import uno.anahata.asi.agi.message.BlobPart;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.MediaRenderer;
import uno.anahata.asi.swing.agi.render.MediaViewerComponent;

/**
 * Renders a {@link uno.anahata.asi.agi.message.BlobPart} into a JComponent,
 * handling images, audio, and file information.
 *
 * @author anahata
 */
public class BlobPartPanel extends AbstractPartPanel<BlobPart> {

    /** Label for displaying fallback error messages. */
    private JLabel mainContentLabel; 

    /** Active multimodal media viewer component (image, audio, or video). */
    private MediaViewerComponent activeViewer;

    /** Tracks the last rendered data to avoid redundant updates. */
    private byte[] lastRenderedData; 
    /** Tracks the last rendered MIME type. */
    private String lastRenderedMimeType; 

    /**
     * Constructs a new BlobPartPanel.
     *
     * @param agiPanel The agi panel instance.
     * @param part The BlobPart to be rendered.
     */
    public BlobPartPanel(@NonNull AgiPanel agiPanel, @NonNull BlobPart part) {
        super(agiPanel, part);
    }

    /**
     * {@inheritDoc}
     * <p>Renders the content of the BlobPart via {@link MediaRenderer#createViewer},
     * supporting interactive images, hardware-accelerated video/audio, or fallback JavaSound audio.</p>
     */
    @Override
    protected void renderContent() {
        BlobPart blobPart = part;
        String currentMimeType = blobPart.getMimeType();
        byte[] currentData = blobPart.getData();

        boolean contentChanged = activeViewer == null || !Arrays.equals(currentData, lastRenderedData) || !Objects.equals(currentMimeType, lastRenderedMimeType);

        if (mainContentLabel == null) {
            mainContentLabel = new JLabel();
        }

        if (contentChanged) {
            if (activeViewer != null) {
                activeViewer.dispose();
                activeViewer = null;
            }
            getContentContainer().removeAll();

            if (currentData == null || currentData.length == 0) {
                mainContentLabel.setText("Error: Blob data is empty.");
                getContentContainer().add(mainContentLabel, BorderLayout.CENTER);
            } else {
                AbstractSwingAsiContainer container = (AbstractSwingAsiContainer) getAgiPanel().getAgi().getConfig().getAsiContainer();
                String displayName = blobPart.getSourcePath() != null ? blobPart.getSourcePath().getFileName().toString() : "blob";
                URI sourceUri = blobPart.getSourcePath() != null ? blobPart.getSourcePath().toUri() : null;

                this.activeViewer = MediaRenderer.createViewer(currentData, currentMimeType, displayName, sourceUri, container, agiPanel);
                getContentContainer().add(activeViewer.getComponent(), BorderLayout.CENTER);
            }

            lastRenderedData = currentData;
            lastRenderedMimeType = currentMimeType;
            revalidate();
            repaint();
        }
    }

    /**
     * {@inheritDoc}
     * <p>Re-renders the viewer component if re-attached to the UI hierarchy after being pruned/removed.</p>
     */
    @Override
    public void addNotify() {
        super.addNotify();
        if (activeViewer == null && part != null && part.getData() != null) {
            renderContent();
        }
    }

    /**
     * {@inheritDoc}
     * <p>Stops active media playback when the panel is removed from the UI hierarchy.</p>
     */
    @Override
    public void removeNotify() {
        if (activeViewer != null) {
            activeViewer.dispose();
            activeViewer = null;
        }
        super.removeNotify();
    }
}
