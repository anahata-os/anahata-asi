/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi.tool;

import java.beans.PropertyChangeSupport;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.event.PropertyChangeSource;
import uno.anahata.asi.persistence.Rebindable;


/**
 * The base class for all AI toolkits. It integrates the tool execution context
 * with the hierarchical context provider system, allowing toolkits to natively
 * contribute system instructions and RAG data.
 * <p>
 * This class is designed for autonomy: toolkits manage their own state and can 
 * notify the UI of changes via the {@link PropertyChangeSource} interface.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@RequiredArgsConstructor
public abstract class AnahataToolkit extends ToolContext implements ContextProvider, Rebindable, PropertyChangeSource {
    
    /** Whether this toolkit is currently providing context augmentation. */
    @Setter
    @Getter
    private boolean providing = true;
    
    /**
     * Support for firing property change events. 
     * Marked transient to avoid serializing listeners.
     */
    protected transient PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

    /**
     * The list of child context providers managed by this toolkit.
     * Uses CopyOnWriteArrayList for thread-safe concurrent access during IDE events.
     */
    protected List<ContextProvider> childrenProviders = new CopyOnWriteArrayList<>();
    
    /** {@inheritDoc} */
    @Override
    public String getId() {
        return toolkit.getName() + "@" + System.identityHashCode(this);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return toolkit.getName();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return toolkit.getDescription();
    }

    /** {@inheritDoc} */
    @Override
    public ContextProvider getParentProvider() {
        return getToolManager();
    }

    /** {@inheritDoc} */
    @Override
    public List<ContextProvider> getChildrenProviders() {
        return childrenProviders;
    }

    /**
     * Rebinds the toolkit after deserialization.
     * <p>
     * This method is called by the framework during session activation to restore 
     * transient state, reconnect listeners, or refresh cached project instances. 
     * It ensures the {@code propertyChangeSupport} is re-initialized.
     * </p>
     */
    @Override
    public void rebind() {
        log.info("Rebinding toolkit: {}", getName());
        this.propertyChangeSupport = new PropertyChangeSupport(this);
    }

    /**
     * Performs initial setup for a newly created toolkit instance.
     * <p>
     * This method is called exactly once by the {@link ToolManager} after the 
     * toolkit has been instantiated and registered. It provides a clean hook 
     * for toolkits to perform one-time initialization logic that requires 
     * the tool environment to be fully configured, such as scanning for 
     * project instructions or registering default resources.
     * </p>
     */
    public void initialize() {
        log.info("Initializing toolkit: {}", getName());
    }

    /** {@inheritDoc} */
    @Override
    public PropertyChangeSupport getPropertyChangeSupport() {
        return propertyChangeSupport;
    }
    

    /** {@inheritDoc} */
    public void postActivate() {
        log.info("Post-activating toolkit: {}", getName());
    }
}
