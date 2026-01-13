/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.model.core;

import java.beans.PropertyChangeSupport;

/**
 * A standard interface for objects that provide {@link PropertyChangeSupport}.
 * This allows UI components to generically bind to model objects for reactive updates.
 *
 * @author anahata
 */
public interface PropertyChangeSource {

    /**
     * Gets the PropertyChangeSupport instance for this object.
     * @return The PropertyChangeSupport.
     */
    PropertyChangeSupport getPropertyChangeSupport();
}
