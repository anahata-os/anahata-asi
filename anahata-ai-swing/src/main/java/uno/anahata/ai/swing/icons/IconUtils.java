package uno.anahata.ai.swing.icons;

import java.awt.Image;
import javax.swing.ImageIcon;

/**
 * A utility class for handling icons.
 */
public class IconUtils {

    /**
     * Loads an icon from the classpath resources and scales it to a 24x24 size.
     *
     * @param name The name of the icon file (e.g., "attach.png").
     * @return A scaled ImageIcon, or null if the resource is not found.
     */
    public static ImageIcon getIcon(String name) {
        try {
            ImageIcon originalIcon = new ImageIcon(IconUtils.class.getResource("/icons/" + name));
            Image scaledImage = originalIcon.getImage().getScaledInstance(24, 24, Image.SCALE_SMOOTH);
            return new ImageIcon(scaledImage);
        } catch (Exception e) {
            System.err.println("Could not load icon: " + name);
            return null;
        }
    }
}
