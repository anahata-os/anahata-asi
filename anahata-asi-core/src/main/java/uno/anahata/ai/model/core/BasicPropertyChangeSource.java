/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.model.core;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import lombok.Getter;

/**
 * A base class providing a standard implementation of {@link PropertyChangeSource}.
 * This reduces boilerplate for domain objects that need to support reactive UI updates.
 * 
 * @author anahata
 */
public class BasicPropertyChangeSource implements PropertyChangeSource {

    @Getter
    protected final PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

    /**
     * Adds a property change listener.
     * 
     * @param listener The listener to add.
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes a property change listener.
     * 
     * @param listener The listener to remove.
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(listener);
    }
    
    /**
     * Adds a property change listener for a specific property.
     * 
     * @param propertyName The name of the property to listen to.
     * @param listener The listener to add.
     */
    public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
    }

    /**
     * Removes a property change listener for a specific property.
     * 
     * @param propertyName The name of the property.
     * @param listener The listener to remove.
     */
    public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(propertyName, listener);
    }
}
