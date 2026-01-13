/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.icons;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import javax.swing.ImageIcon;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

/**
 * A utility class for loading and scaling icons with high-quality rendering.
 * 
 * @author anahata
 */
@Slf4j
@UtilityClass
public class IconUtils {

    /**
     * Loads an icon from the classpath resources and scales it to a default 24x24 size.
     *
     * @param name The name of the icon file (e.g., "attach.png").
     * @return A scaled ImageIcon, or null if the resource is not found.
     */
    public static ImageIcon getIcon(String name) {
        return getIcon(name, 24, 24);
    }

    /**
     * Loads an icon from the classpath resources and scales it to the specified size
     * using high-quality rendering hints.
     *
     * @param name The name of the icon file.
     * @param width The desired width.
     * @param height The desired height.
     * @return A scaled ImageIcon, or null if the resource is not found.
     */
    public static ImageIcon getIcon(String name, int width, int height) {
        try {
            java.net.URL resource = IconUtils.class.getResource("/icons/" + name);
            if (resource == null) {
                log.warn("Icon resource not found: /icons/{}", name);
                return null;
            }
            ImageIcon originalIcon = new ImageIcon(resource);
            Image img = originalIcon.getImage();
            
            // Create a high-quality scaled image
            BufferedImage scaledImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
            Graphics2D g2 = scaledImg.createGraphics();
            
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            
            g2.drawImage(img, 0, 0, width, height, null);
            g2.dispose();
            
            return new ImageIcon(scaledImg);
        } catch (Exception e) {
            log.error("Error loading icon: {}", name, e);
            return null;
        }
    }

    /**
     * Gets a list of images for the Anahata logo in various sizes.
     * This is useful for setting the window icon, allowing the OS to choose the best size.
     * 
     * @return A list of logo images.
     */
    public static List<Image> getLogoImages() {
        List<Image> images = new ArrayList<>();
        int[] sizes = {16, 32, 48, 64, 128, 256};
        for (int size : sizes) {
            ImageIcon icon = getIcon("anahata.png", size, size);
            if (icon != null) {
                images.add(icon.getImage());
            }
        }
        return images;
    }
}
