/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.model.core;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.v3.oas.annotations.media.Schema;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import lombok.Getter;

/**
 * An abstract base class providing a standard implementation of {@link PropertyChangeSource}.
 * This reduces boilerplate for domain objects that need to support reactive UI updates.
 * <p>
 * The {@link PropertyChangeSupport} field is marked as {@code transient} to prevent
 * the serialization of UI listeners, which are typically not serializable and 
 * should not be persisted between sessions.
 * </p>
 * 
 * @author anahata
 */
public abstract class BasicPropertyChangeSource implements PropertyChangeSource, Rebindable {

    /** 
     * Support for firing property change events. 
     * Marked transient to avoid serializing listeners.
     */
    @Getter
    @JsonIgnore
    @Schema(hidden = true)
    protected transient PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

    /**
     * Re-initializes the transient {@link PropertyChangeSupport} after deserialization.
     */
    @Override
    public void rebind() {
        this.propertyChangeSupport = new PropertyChangeSupport(this);
    }

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
