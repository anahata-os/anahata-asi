/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.swing.internal;

import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.function.Consumer;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import lombok.NonNull;
import uno.anahata.ai.model.core.PropertyChangeSource;

/**
 * A smart, lifecycle-aware {@link PropertyChangeListener} that ensures UI updates
 * occur on the Event Dispatch Thread (EDT). It automatically manages its
 * subscription to the {@link PropertyChangeSource} based on the displayability
 * of the associated {@link JComponent}.
 * <p>
 * This class eliminates the need for manual addNotify/removeNotify boilerplate
 * and prevents memory leaks by unregistering itself when the component is
 * removed from the UI hierarchy.
 *
 * @author pablo
 */
public class EdtPropertyChangeListener implements PropertyChangeListener, HierarchyListener {

    /** The execution mode for thread switching. */
    public enum Mode { 
        /** Executes the action via {@link SwingUtilities#invokeLater(Runnable)}. */
        INVOKE_LATER, 
        /** Executes the action via {@link SwingUtilities#invokeAndWait(Runnable)}. Use with caution. */
        INVOKE_AND_WAIT 
    }

    private final JComponent component;
    private final PropertyChangeSource source;
    private final String propertyName;
    private final Consumer<PropertyChangeEvent> action;
    private final Mode mode;

    /**
     * Constructs a new EdtPropertyChangeListener and binds it to the component's lifecycle.
     *
     * @param component The UI component whose lifecycle determines the subscription.
     * @param source The model object providing property change events.
     * @param propertyName The name of the property to listen for, or null for all properties.
     * @param action The action to perform on the EDT when the property changes.
     */
    public EdtPropertyChangeListener(@NonNull JComponent component, @NonNull PropertyChangeSource source, String propertyName, @NonNull Consumer<PropertyChangeEvent> action) {
        this(component, source, propertyName, action, Mode.INVOKE_LATER);
    }

    /**
     * Constructs a new EdtPropertyChangeListener with a specific execution mode.
     *
     * @param component The UI component whose lifecycle determines the subscription.
     * @param source The model object providing property change events.
     * @param propertyName The name of the property to listen for, or null for all properties.
     * @param action The action to perform on the EDT when the property changes.
     * @param mode The thread switching mode.
     */
    public EdtPropertyChangeListener(@NonNull JComponent component, @NonNull PropertyChangeSource source, String propertyName, @NonNull Consumer<PropertyChangeEvent> action, @NonNull Mode mode) {
        this.component = component;
        this.source = source;
        this.propertyName = propertyName;
        this.action = action;
        this.mode = mode;

        component.addHierarchyListener(this);
        updateSubscription(); // Initial check in case component is already displayable
    }

    /**
     * Handles the property change event, switching to the EDT if necessary.
     * @param evt The property change event.
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if (SwingUtilities.isEventDispatchThread()) {
            action.accept(evt);
        } else {
            if (mode == Mode.INVOKE_AND_WAIT) {
                try {
                    SwingUtilities.invokeAndWait(() -> action.accept(evt));
                } catch (Exception e) {
                    throw new RuntimeException("Error executing UI update via invokeAndWait", e);
                }
            } else {
                SwingUtilities.invokeLater(() -> action.accept(evt));
            }
        }
    }

    /**
     * Monitors the component's displayability and updates the model subscription accordingly.
     * @param e The hierarchy event.
     */
    @Override
    public void hierarchyChanged(HierarchyEvent e) {
        if ((e.getChangeFlags() & HierarchyEvent.DISPLAYABILITY_CHANGED) != 0) {
            updateSubscription();
        }
    }

    /**
     * Adds or removes this listener from the model source based on the component's displayability.
     */
    private void updateSubscription() {
        if (component.isDisplayable()) {
            if (propertyName != null) {
                source.getPropertyChangeSupport().addPropertyChangeListener(propertyName, this);
            } else {
                source.getPropertyChangeSupport().addPropertyChangeListener(this);
            }
        } else {
            if (propertyName != null) {
                source.getPropertyChangeSupport().removePropertyChangeListener(propertyName, this);
            } else {
                source.getPropertyChangeSupport().removePropertyChangeListener(this);
            }
        }
    }
}
