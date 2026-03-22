/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import javax.swing.Icon;

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
    
}
