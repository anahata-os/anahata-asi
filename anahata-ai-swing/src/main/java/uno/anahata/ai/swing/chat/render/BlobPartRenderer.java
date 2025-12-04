package uno.anahata.ai.swing.chat.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.BlobPart;

/**
 * Renders a {@link BlobPart}, providing visual components for images and basic
 * info for other binary types.
 *
 * @author Anahata
 */
@Slf4j
public class BlobPartRenderer implements PartRenderer {

    private static final int THUMBNAIL_MAX_SIZE = 250;

    @Override
    public JComponent render(AbstractPart part) {
        BlobPart blobPart = (BlobPart) part;
        String mimeType = blobPart.getMimeType();
        byte[] data = blobPart.getData();

        if (data == null || data.length == 0) {
            return new JLabel("Error: Blob data is empty.");
        }

        JPanel panel = new JPanel(new BorderLayout(10, 10));
        panel.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        panel.setOpaque(false);

        // Handle Audio Blobs
        if (mimeType.startsWith("audio/")) {
            // TODO: Implement audio playback using a V2 media utility.
            JLabel audioLabel = new JLabel(" Audio (playback not yet implemented)");
            panel.add(audioLabel, BorderLayout.CENTER);
        } // Handle Image Blobs
        else if (mimeType.startsWith("image/")) {
            try {
                BufferedImage originalImage = ImageIO.read(new ByteArrayInputStream(data));
                if (originalImage != null) {
                    Image thumbnail = createThumbnail(originalImage);
                    JLabel imageLabel = new JLabel(new ImageIcon(thumbnail));
                    imageLabel.setBorder(BorderFactory.createLineBorder(Color.GRAY));
                    panel.add(imageLabel, BorderLayout.CENTER);
                }
            } catch (IOException e) {
                log.error("Failed to create thumbnail for blob", e);
            }
        }

        // Info Panel for mimeType and size (for all blob types)
        JPanel infoPanel = new JPanel();
        infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.Y_AXIS));
        infoPanel.setOpaque(false);

        JLabel mimeTypeLabel = new JLabel("MIME Type: " + mimeType);
        mimeTypeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        JLabel sizeLabel = new JLabel("Size: " + formatSize(data.length));
        sizeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        infoPanel.add(mimeTypeLabel);
        infoPanel.add(Box.createRigidArea(new Dimension(0, 5)));
        infoPanel.add(sizeLabel);

        panel.add(infoPanel, BorderLayout.SOUTH);

        return panel;
    }

    private Image createThumbnail(BufferedImage original) {
        int width = original.getWidth();
        int height = original.getHeight();

        if (width <= THUMBNAIL_MAX_SIZE && height <= THUMBNAIL_MAX_SIZE) {
            return original;
        }

        double thumbRatio = (double) THUMBNAIL_MAX_SIZE / (double) Math.max(width, height);
        int newWidth = (int) (width * thumbRatio);
        int newHeight = (int) (height * thumbRatio);

        int imageType = original.getType();
        if (imageType == 0) { // Some formats like JPEG return 0, default to ARGB
            imageType = BufferedImage.TYPE_INT_ARGB;
        }
        
        BufferedImage resized = new BufferedImage(newWidth, newHeight, imageType);
        Graphics2D g = resized.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g.drawImage(original, 0, 0, newWidth, newHeight, null);
        g.dispose();

        return resized;
    }

    private String formatSize(long size) {
        if (size < 1024) {
            return size + " B";
        }
        int exp = (int) (Math.log(size) / Math.log(1024));
        char pre = "KMGTPE".charAt(exp - 1);
        return String.format("%.1f %sB", size / Math.pow(1024, exp), pre);
    }
}
