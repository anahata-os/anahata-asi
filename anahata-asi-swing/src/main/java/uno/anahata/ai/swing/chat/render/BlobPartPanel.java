/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.Arrays;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import lombok.NonNull;
import uno.anahata.ai.model.core.BlobPart;
import uno.anahata.ai.internal.TextUtils;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.media.util.AudioPlaybackPanel;

/**
 * Renders a {@link uno.anahata.ai.model.core.BlobPart} into a JComponent,
 * handling images, audio, and file information.
 *
 * @author anahata
 */
public class BlobPartPanel extends AbstractPartPanel<BlobPart> {

    /** Label for displaying image thumbnails or file names. */
    private JLabel mainContentLabel; 
    /** Panel for displaying MIME type and size metadata. */
    private JPanel infoPanel; 
    /** Label for the MIME type string. */
    private JLabel mimeTypeLabel; 
    /** Label for the formatted file size. */
    private JLabel sizeLabel; 
    /** Panel that wraps the image label with a border. */
    private JPanel imageWrapperPanel; 
    /** Outer wrapper panel to prevent the image from stretching. */
    private JPanel centerWrapperPanel; 

    /** Tracks the last rendered data to avoid redundant updates. */
    private byte[] lastRenderedData; 
    /** Tracks the last rendered MIME type. */
    private String lastRenderedMimeType; 
    /** Toggle button for audio playback. */
    private JToggleButton playButton;
    /** Handle to stop the current audio playback. */
    private Runnable currentPlaybackStopper; 

    /** Reference to the global audio playback panel. */
    private final AudioPlaybackPanel audioPlaybackPanel;

    /**
     * Constructs a new BlobPartPanel.
     *
     * @param chatPanel The chat panel instance.
     * @param part The BlobPart to be rendered.
     */
    public BlobPartPanel(@NonNull ChatPanel chatPanel, @NonNull BlobPart part) {
        super(chatPanel, part);
        this.audioPlaybackPanel = chatPanel.getStatusPanel().getAudioPlaybackPanel();
    }

    /**
     * {@inheritDoc}
     * Renders the content of the BlobPart based on its MIME type.
     * Reuses existing components and updates them incrementally.
     */
    @Override
    protected void renderContent() {
        BlobPart blobPart = part;
        String currentMimeType = blobPart.getMimeType();
        byte[] currentData = blobPart.getData();

        boolean contentChanged = !Arrays.equals(currentData, lastRenderedData) || !Objects.equals(currentMimeType, lastRenderedMimeType);

        if (mainContentLabel == null) {
            // Initial render: create all components
            mainContentLabel = new JLabel();
            mainContentLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
            
            imageWrapperPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0)); 
            imageWrapperPanel.setOpaque(false);
            imageWrapperPanel.add(mainContentLabel); 
            imageWrapperPanel.setVisible(false); // Initially hidden
            
            centerWrapperPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
            centerWrapperPanel.setOpaque(false);
            centerWrapperPanel.add(imageWrapperPanel);
            getContentContainer().add(centerWrapperPanel, BorderLayout.CENTER);

            infoPanel = new JPanel();
            infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));
            infoPanel.setOpaque(false);

            mimeTypeLabel = new JLabel();
            mimeTypeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

            sizeLabel = new JLabel();
            sizeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

            infoPanel.add(mimeTypeLabel);
            infoPanel.add(Box.createRigidArea(new Dimension(0, 5)));
            infoPanel.add(sizeLabel);

            getContentContainer().add(infoPanel, BorderLayout.SOUTH);
        }

        if (contentChanged) {
            // Clear previous state
            mainContentLabel.setText(null);
            mainContentLabel.setIcon(null);
            imageWrapperPanel.setVisible(false);
            if (playButton != null) {
                getContentContainer().remove(playButton);
                playButton = null;
            }
            if (currentPlaybackStopper != null) {
                currentPlaybackStopper.run();
                currentPlaybackStopper = null;
            }

            // Update content of existing components
            if (currentData == null || currentData.length == 0) {
                mainContentLabel.setText("Error: Blob data is empty.");
            } else if (currentMimeType.startsWith("audio/")) {
                playButton = MediaRenderer.createAudioComponent(currentData, audioPlaybackPanel, stopper -> {
                    if (stopper == null && currentPlaybackStopper != null) {
                        currentPlaybackStopper.run();
                    }
                    currentPlaybackStopper = stopper;
                });
                getContentContainer().add(playButton, BorderLayout.NORTH);
            } else if (currentMimeType.startsWith("image/")) {
                imageWrapperPanel.setVisible(true);
                imageWrapperPanel.removeAll();
                imageWrapperPanel.add(MediaRenderer.createImageComponent(currentData, this));
            } else {
                // Default for other file types
                String fileName = blobPart.getSourcePath() != null ? blobPart.getSourcePath().getFileName().toString() : "Unknown File";
                mainContentLabel.setText("File: " + fileName);
            }

            mimeTypeLabel.setText("MIME Type: " + currentMimeType);
            sizeLabel.setText("Size: " + TextUtils.formatSize(currentData != null ? currentData.length : 0));

            lastRenderedData = currentData;
            lastRenderedMimeType = currentMimeType;
        }
    }
}
