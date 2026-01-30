/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

/**
 * A utility class for loading, scaling, and managing a global registry of icons.
 * <p>
 * This class provides high-quality rendering hints for icon scaling and 
 * supports a decoupled icon lookup mechanism using string-based IDs.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@UtilityClass
public class IconUtils {

    /** A global registry for mapping icon IDs to actual Icon objects. */
    private static final Map<String, Icon> ICON_REGISTRY = new ConcurrentHashMap<>();

    /**
     * Registers an icon in the global registry.
     * @param id The unique identifier for the icon.
     * @param icon The icon object.
     */
    public static void registerIcon(String id, Icon icon) {
        if (id != null && icon != null) {
            ICON_REGISTRY.put(id, icon);
            log.debug("Registered icon with ID: {}", id);
        }
    }

    /**
     * Retrieves an icon from the global registry or loads it from the classpath.
     * 
     * @param idOrName The icon ID or the resource name (e.g., "attach.png").
     * @return The Icon, or null if not found.
     */
    public static Icon getIcon(String idOrName) {
        if (idOrName == null) return null;
        
        // 1. Check registry
        Icon registered = ICON_REGISTRY.get(idOrName);
        if (registered != null) return registered;
        
        // 2. Fallback to classpath loading (default size 24x24)
        return getIcon(idOrName, 24, 24);
    }

    /**
     * Retrieves a square icon of the specified size.
     * @param idOrName The icon ID or resource name.
     * @param size The desired width and height.
     * @return The scaled Icon.
     */
    public static Icon getIcon(String idOrName, int size) {
        return getIcon(idOrName, size, size);
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
            
            // Use maximum quality hints for resizing
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
            g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
            g2.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY);
            
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

    /**
     * Creates an icon for adding items to the context by overlaying a small 
     * Anahata badge on a large blue '+' symbol.
     * 
     * @return The 'Add to Context' icon.
     */
    public static Icon getAddIcon() {
        return getSymbolBaseIcon(true);
    }

    /**
     * Creates an icon for removing items from the context by overlaying a small 
     * Anahata badge on a large red '-' symbol.
     * 
     * @return The 'Remove from Context' icon.
     */
    public static Icon getRemoveIcon() {
        return getSymbolBaseIcon(false);
    }

    /**
     * Internal helper to create an icon with a symbol base and an Anahata badge.
     * 
     * @param isAdd True for '+', false for '-'.
     * @return The combined icon.
     */
    private static Icon getSymbolBaseIcon(boolean isAdd) {
        ImageIcon badgeIcon = getIcon("anahata.png", 8, 8);
        if (badgeIcon == null) return null;
        
        BufferedImage combined = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = combined.createGraphics();
        
        // Use crisp rendering for the symbols
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
        
        int symbolSize = 12;
        int thickness = 3;
        int offset = 1; // North-West offset to fit the badge

        if (isAdd) {
            g.setColor(new Color(0, 102, 204)); // Lighter Barsa Blue
            // Horizontal bar
            g.fillRect(offset, offset + (symbolSize / 2) - (thickness / 2), symbolSize, thickness);
            // Vertical bar
            g.fillRect(offset + (symbolSize / 2) - (thickness / 2), offset, thickness, symbolSize);
        } else {
            g.setColor(new Color(165, 0, 68)); // Dark Barsa Red
            // Horizontal bar
            g.fillRect(offset, offset + (symbolSize / 2) - (thickness / 2), symbolSize, thickness);
        }
        
        // Draw the Anahata badge in the bottom-right corner
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.drawImage(badgeIcon.getImage(), 8, 8, null);
        
        g.dispose();
        return new ImageIcon(combined);
    }
}
