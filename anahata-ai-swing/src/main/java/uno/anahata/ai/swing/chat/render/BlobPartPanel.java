/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Image;
import java.awt.event.ItemEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;
import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.ai.model.core.BlobPart;
import uno.anahata.ai.internal.TextUtils;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.internal.SwingUtils;
import uno.anahata.ai.swing.media.util.AudioPlaybackPanel;

/**
 * Renders a {@link uno.anahata.ai.model.core.BlobPart} into a JComponent,
 * handling images, audio, and file information.
 *
 * @author anahata
 */
public class BlobPartPanel extends AbstractPartPanel<BlobPart> {

    private JLabel mainContentLabel; // Label for image or file name
    private JPanel infoPanel; // Panel for mimeType and size
    private JLabel mimeTypeLabel; // Label for MIME type
    private JLabel sizeLabel; // Label for size
    private JPanel imageWrapperPanel; // Panel to hold image icon with border
    private JPanel centerWrapperPanel; // New: Wrapper panel for imageWrapperPanel

    private byte[] lastRenderedData; // To track changes in blob data
    private String lastRenderedMimeType; // To track changes in mime type
    private JToggleButton playButton;
    private Runnable currentPlaybackStopper; // Handle to stop specific playback

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
     * Renders the content of the BlobPart into the contentPanel.
     * This method handles different MIME types (image, audio, other files).
     * It reuses existing components and updates their content only if the blob data or mime type has changed.
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
            imageWrapperPanel.setBorder(BorderFactory.createLineBorder(Color.GRAY)); 
            imageWrapperPanel.setVisible(false); // Initially hidden
            
            // New: Wrap imageWrapperPanel in another FlowLayout panel to prevent stretching
            centerWrapperPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
            centerWrapperPanel.setOpaque(false);
            centerWrapperPanel.add(imageWrapperPanel);
            getContentContainer().add(centerWrapperPanel, BorderLayout.CENTER); // Add the new wrapper panel

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
                if (playButton == null) { // Create button only if it doesn't exist
                    playButton = new JToggleButton("▶ Play Audio");
                    playButton.addItemListener(e -> {
                        if (e.getStateChange() == ItemEvent.SELECTED) {
                            // Start playing
                            currentPlaybackStopper = audioPlaybackPanel.playToggleable(currentData, isPlaying -> {
                                SwingUtilities.invokeLater(() -> {
                                    if (!isPlaying) {
                                        playButton.setSelected(false); // Deselect button when playback ends
                                        playButton.setText("▶ Play Audio");
                                        currentPlaybackStopper = null;
                                    }
                                });
                            });
                            playButton.setText("■ Stop Audio");
                        } else {
                            // Stop playing
                            if (currentPlaybackStopper != null) {
                                currentPlaybackStopper.run();
                                currentPlaybackStopper = null;
                            }
                            playButton.setText("▶ Play Audio");
                        }
                    });
                    getContentContainer().add(playButton, BorderLayout.NORTH); // Add button to the top
                }
            } else if (currentMimeType.startsWith("image/")) {
                imageWrapperPanel.setVisible(true); // Show the image wrapper
                try {
                    BufferedImage originalImage = ImageIO.read(new ByteArrayInputStream(currentData));
                    if (originalImage != null) {
                        Image thumbnail = SwingUtils.createThumbnail(originalImage);
                        mainContentLabel.setIcon(new ImageIcon(thumbnail));
                        
                        // Remove existing mouse listeners to prevent duplicates
                        for (MouseListener listener : imageWrapperPanel.getMouseListeners()) {
                            if (listener instanceof MouseAdapter) {
                                imageWrapperPanel.removeMouseListener(listener);
                            }
                        }
                        imageWrapperPanel.addMouseListener(new MouseAdapter() {
                            @Override
                            public void mouseClicked(MouseEvent e) {
                                showFullSizeImagePopup(originalImage);
                            }
                        });

                    } else {
                        mainContentLabel.setText("Failed to load image.");
                    }
                } catch (IOException e) {
                    mainContentLabel.setText("Failed to render image: " + e.getMessage());
                }
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

    /**
     * Displays the full-size image in a non-modal popup dialog.
     * @param image The full-size BufferedImage to display.
     */
    private void showFullSizeImagePopup(BufferedImage image) {
        JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(chatPanel), "Full Size Image", Dialog.ModalityType.MODELESS);
        dialog.setLayout(new BorderLayout());
        
        JLabel imageLabel = new JLabel(new ImageIcon(image));
        JScrollPane scrollPane = new JScrollPane(imageLabel);
        scrollPane.setPreferredSize(new Dimension(Math.min(image.getWidth(), 800), Math.min(image.getHeight(), 600))); // Limit initial size
        
        dialog.add(scrollPane, BorderLayout.CENTER);
        dialog.pack();
        dialog.setLocationRelativeTo(chatPanel);
        dialog.setVisible(true);
    }
}
