/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.event.ItemEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.function.Consumer;
import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.swing.internal.SwingUtils;
import uno.anahata.ai.swing.media.util.AudioPlaybackPanel;

/**
 * A utility class for rendering media content (images, audio) in a consistent way
 * across different UI components.
 * 
 * @author anahata-ai
 */
@Slf4j
public class MediaRenderer {

    /**
     * Creates a component for audio playback.
     * 
     * @param data The audio data.
     * @param audioPanel The audio playback panel to use.
     * @param onStopperCreated A consumer that will receive the stopper Runnable when playback starts.
     * @return A JToggleButton configured for audio playback.
     */
    public static JToggleButton createAudioComponent(byte[] data, AudioPlaybackPanel audioPanel, Consumer<Runnable> onStopperCreated) {
        JToggleButton playBtn = new JToggleButton("▶ Play Audio");
        playBtn.addItemListener(e -> {
            if (e.getStateChange() == ItemEvent.SELECTED) {
                Runnable stopper = audioPanel.playToggleable(data, isPlaying -> {
                    SwingUtilities.invokeLater(() -> {
                        if (!isPlaying) {
                            playBtn.setSelected(false);
                            playBtn.setText("▶ Play Audio");
                            onStopperCreated.accept(null);
                        }
                    });
                });
                onStopperCreated.accept(stopper);
                playBtn.setText("■ Stop Audio");
            } else {
                onStopperCreated.accept(null); // The caller should handle the actual stopping
                playBtn.setText("▶ Play Audio");
            }
        });
        return playBtn;
    }

    /**
     * Creates a component for image display with thumbnail and popup support.
     * 
     * @param data The image data.
     * @param parent The parent component for the popup dialog.
     * @return A JLabel containing the thumbnail, or an error label.
     */
    public static Component createImageComponent(byte[] data, Component parent) {
        try {
            BufferedImage img = ImageIO.read(new ByteArrayInputStream(data));
            if (img != null) {
                Image thumb = SwingUtils.createThumbnail(img);
                JLabel imgLabel = new JLabel(new ImageIcon(thumb));
                imgLabel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
                imgLabel.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent e) {
                        showFullSizeImagePopup(img, parent);
                    }
                });
                return imgLabel;
            }
        } catch (IOException e) {
            log.error("Error rendering image", e);
            return new JLabel("Error loading image: " + e.getMessage());
        }
        return new JLabel("Failed to load image.");
    }

    /**
     * Displays a full-size image in a non-modal popup dialog.
     * 
     * @param image The image to display.
     * @param parent The parent component.
     */
    public static void showFullSizeImagePopup(BufferedImage image, Component parent) {
        JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(parent), "Full Size Image", Dialog.ModalityType.MODELESS);
        dialog.setLayout(new BorderLayout());
        
        JLabel imageLabel = new JLabel(new ImageIcon(image));
        JScrollPane scrollPane = new JScrollPane(imageLabel);
        scrollPane.setPreferredSize(new Dimension(Math.min(image.getWidth(), 800), Math.min(image.getHeight(), 600)));
        
        dialog.add(scrollPane, BorderLayout.CENTER);
        dialog.pack();
        dialog.setLocationRelativeTo(parent);
        dialog.setVisible(true);
    }
}
