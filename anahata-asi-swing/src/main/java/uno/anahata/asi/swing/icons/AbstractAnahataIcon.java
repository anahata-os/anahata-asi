/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import java.awt.Color;
import java.awt.Component;
import javax.swing.Icon;
import javax.swing.UIManager;
import lombok.Getter;

/**
 * The foundational base class for all programmatically drawn icons in the Anahata ASI ecosystem.
 * <p>
 * This class encapsulates the common "square size" pattern used across the Swing UI, 
 * providing thread-safe access to the icon dimensions and enforcing a consistent 
 * implementation of the {@link Icon} interface.
 * </p>
 * <p>
 * All programmatically rendered icons should extend this class to ensure that 
 * scaling and dimension reporting remain synchronized.
 * </p>
 * 
 * @author anahata
 */
public abstract class AbstractAnahataIcon implements Icon {
    
    /** 
     * The dimension of the icon in pixels (width and height). 
     * This field is protected to allow direct access within the {@code paintIcon} 
     * implementations of subclasses for coordinate calculations.
     */
    @Getter
    protected final int size;

    /**
     * Constructs a new abstract icon with the specified square dimension.
     * 
     * @param size The size in pixels for both width and height.
     */
    public AbstractAnahataIcon(int size) {
        this.size = size;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the fixed width configured at construction time.
     * </p>
     */
    @Override
    public int getIconWidth() {
        return size;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the fixed height configured at construction time.
     * </p>
     */
    @Override
    public int getIconHeight() {
        return size;
    }

    /**
     * Resolves the Barça Blue color adaptively based on the active Look and Feel.
     * On light themes, returns the official deep Barça Blue.
     * On dark themes, returns a vibrant, high-contrast electric blue.
     *
     * @param c The component being painted.
     * @return The resolved Color.
     */
    protected Color getBlueColor(Component c) {
        if (c != null && !c.isEnabled()) {
            return Color.GRAY;
        }
        return isDarkLaf() 
                ? new Color(64, 156, 255)  // Vibrant neon electric blue
                : new Color(0, 77, 152);   // Official deep Barça Blue
    }

    /**
     * Resolves the Barça Red / Garnet color adaptively based on the active Look and Feel.
     * On light themes, returns the official deep Barça Garnet.
     * On dark themes, returns a vibrant, high-contrast crimson red.
     *
     * @param c The component being painted.
     * @return The resolved Color.
     */
    protected Color getRedColor(Component c) {
        if (c != null && !c.isEnabled()) {
            return Color.GRAY;
        }
        return isDarkLaf() 
                ? new Color(230, 45, 85)   // Vibrant glowing crimson red
                : new Color(165, 0, 68);   // Official deep Barça Garnet
    }

    /**
     * Determines whether the active Look and Feel is dark based on panel background luminance.
     *
     * @return true if dark mode is active.
     */
    protected boolean isDarkLaf() {
        Color bg = UIManager.getColor("Panel.background");
        if (bg == null) {
            return false;
        }
        return (0.2126 * bg.getRed() + 0.7152 * bg.getGreen() + 0.0722 * bg.getBlue()) / 255.0 < 0.5;
    }
    
}
