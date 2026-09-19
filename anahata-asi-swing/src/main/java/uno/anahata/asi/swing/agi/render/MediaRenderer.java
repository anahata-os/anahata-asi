/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.ItemEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URI;
import java.util.function.Consumer;
import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.audio.AudioPlaybackPanel;
import uno.anahata.asi.swing.internal.JavaFxBridgeClassLoader;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Multimodal rendering engine for non-textual message parts.
 * <p>
 * This utility provides high-fidelity visualizers for image and audio data. 
 * It manages thumbnail generation, asynchronous audio playback integration, 
 * and non-modal popup dialogs for full-size image inspection.
 * </p>
 * 
 * @author anahata
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
        JDialog dialog = new JDialog(SwingUtilities.getWindowAncestor(parent), "Image Viewer", Dialog.ModalityType.MODELESS);
        dialog.setLayout(new BorderLayout());

        AgiPanel ap = (AgiPanel) SwingUtilities.getAncestorOfClass(AgiPanel.class, parent);
        SwingImageViewer viewer = new SwingImageViewer(ap);
        try {
            byte[] pngData = SwingUtils.encodeToPng(image);
            viewer.load(pngData, "image/png", "image.png", null);
        } catch (Exception ex) {
            log.error("Failed to encode image to PNG for viewer", ex);
        }

        dialog.add(viewer, BorderLayout.CENTER);
        dialog.setSize(new Dimension(Math.min(image.getWidth() + 40, 1100), Math.min(image.getHeight() + 80, 800)));
        dialog.setLocationRelativeTo(parent);
        dialog.setVisible(true);
    }

    /**
     * Creates an interactive {@link SwingImageViewer} initialized with raw
     * image data, MIME type, and optional source URI.
     *
     * @param mimeType The detected image MIME type (e.g. 'image/png',
     * 'image/jpeg').
     * @param data The raw binary image data.
     * @param displayName The human-readable display name or file name.
     * @param sourceUri The optional disk or network URI of the image source.
     * @param agiPanel The parent AgiPanel providing session context.
     * @return A fully configured {@link SwingImageViewer} ready for embedding
     * in the UI.
     */
    public static SwingImageViewer createImageViewer(byte[] data, String mimeType, String displayName, URI sourceUri, @NonNull AgiPanel agiPanel) {
        SwingImageViewer viewer = new SwingImageViewer(agiPanel);
        viewer.load(data, mimeType, displayName, sourceUri);
        return viewer;
    }

    /**
     * Creates the optimal {@link MediaViewerComponent} based on MIME type and container runtime capabilities.
     *
     * @param data The raw binary data.
     * @param mimeType The detected MIME type.
     * @param displayName An optional human-readable name or file name.
     * @param sourceUri An optional source URI.
     * @param container The active ASI container (for resolving JavaFX runtime availability).
     * @param agiPanel The parent AgiPanel providing session context.
     * @return A configured {@link MediaViewerComponent}.
     */
    public static MediaViewerComponent createViewer(byte[] data, String mimeType, String displayName, URI sourceUri, AbstractSwingAsiContainer container, @NonNull AgiPanel agiPanel) {
        if (mimeType == null) {
            mimeType = "application/octet-stream";
        }
        String cleanMime = mimeType.toLowerCase().split(";")[0].trim();

        // 1. High-fidelity Interactive Image Viewer (Pure Swing, universal)
        if (cleanMime.startsWith("image/")) {
            return createImageViewer(data, cleanMime, displayName, sourceUri, agiPanel);
        }

        // 2. Host-provided media viewer (e.g. JCEF in IntelliJ)
        if (container != null) {
            MediaViewerComponent hostViewer = container.createHostMediaViewer(data, cleanMime, displayName, sourceUri, agiPanel);
            if (hostViewer != null) {
                return hostViewer;
            }
        }

        // 3. Hardware-accelerated Video & Audio with JavaFX
        boolean fxAvailable = container != null && container.isJavaFxAvailable();
        if (fxAvailable && (cleanMime.startsWith("video/") || cleanMime.startsWith("audio/"))) {
            ClassLoader fxLoader = container.getJavaFxClassLoader();
            if (fxLoader != null) {
                ClassLoader prevCl = Thread.currentThread().getContextClassLoader();
                try {
                    JavaFxBridgeClassLoader bridge = new JavaFxBridgeClassLoader(MediaRenderer.class.getClassLoader(), fxLoader);
                    Thread.currentThread().setContextClassLoader(bridge);
                    Class<?> viewerCls = bridge.loadClass("uno.anahata.asi.swing.agi.render.JavaFxMediaViewerImpl");
                    MediaViewerComponent viewer = (MediaViewerComponent) viewerCls.getDeclaredConstructor(AgiPanel.class).newInstance(agiPanel);
                    viewer.load(data, cleanMime, displayName, sourceUri);
                    return viewer;
                } catch (Throwable t) {
                    //this is an error, if there is a javafx class loader and we get here, we have an error
                    log.error("Failed to instantiate JavaFxMediaViewer via bridge, falling back to Swing audio or card: {}", t);
                } finally {
                    Thread.currentThread().setContextClassLoader(prevCl);
                }
            }
        }

        // 3. Pure Swing Audio fallback (JavaSound)
        if (cleanMime.startsWith("audio/")) {
            SwingAudioViewer audioViewer = new SwingAudioViewer(agiPanel);
            audioViewer.load(data, cleanMime, displayName, sourceUri);
            return audioViewer;
        }

        // 4. Generic Binary File card with MediaToolbar
        return new GenericMediaCard(data, cleanMime, displayName, sourceUri, agiPanel);
    }

    /**
     * Minimal card viewer for generic binary or document files.
     */
    public static class GenericMediaCard extends JPanel implements MediaViewerComponent {
        @Getter
        private final MediaToolbar toolbar;
        private final JLabel nameLabel = new JLabel();

        public GenericMediaCard(byte[] data, String mimeType, String displayName, URI sourceUri, @NonNull AgiPanel agiPanel) {
            super(new BorderLayout(10, 10));
            this.toolbar = new MediaToolbar(agiPanel);
            setOpaque(true);
            setBackground(new Color(24, 28, 36));
            setBorder(BorderFactory.createEmptyBorder(12, 16, 12, 16));

            nameLabel.setFont(nameLabel.getFont().deriveFont(Font.BOLD, 13f));
            nameLabel.setForeground(new Color(226, 232, 240));

            JPanel centerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 4));
            centerPanel.setOpaque(false);
            centerPanel.add(nameLabel);

            add(centerPanel, BorderLayout.CENTER);
            add(toolbar, BorderLayout.SOUTH);

            load(data, mimeType, displayName, sourceUri);
        }

        /**
         * {@inheritDoc}
         * <p>Returns this JPanel as the primary visual component.</p>
         */
        @Override
        public JComponent getComponent() {
            return this;
        }

        /**
         * {@inheritDoc}
         * <p>Updates the card label and configures the action toolbar with binary metadata.</p>
         */
        @Override
        public void load(byte[] data, String mimeType, String displayName, URI sourceUri) {
            nameLabel.setText(displayName != null ? displayName : "Binary Resource");
            toolbar.setData(data);
            toolbar.setMimeType(mimeType);
            toolbar.setDisplayName(displayName);
            toolbar.setSourceUri(sourceUri);
            toolbar.updateMetadata(null);
        }

        /**
         * {@inheritDoc}
         * <p>No active playback stream to stop in generic card view.</p>
         */
        @Override
        public void stop() {
        }

        /**
         * {@inheritDoc}
         * <p>Disposes of toolbar resources.</p>
         */
        @Override
        public void dispose() {
        }
    }
}
