/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit.render;

import java.awt.BorderLayout;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JPanel;
import lombok.Getter;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * A base class for specialized toolkit UIs.
 * <p>
 * This class provides automatic EDT-safe property change binding to the toolkit
 * instance. It re-initializes its listener whenever it is bound to a new instance.
 * </p>
 * 
 * @param <T> The specific toolkit type.
 * @author anahata
 */
public abstract class AbstractToolkitRenderer<T extends AnahataToolkit> extends JPanel implements ToolkitRenderer<T>, PropertyChangeListener {

    /** The toolkit instance. */
    @Getter
    protected T anahataToolkit;
    
    /** The parent AgiPanel. */
    @Getter
    protected AgiPanel agiPanel;

    /** Reactive property change listener delegate. */
    private EdtPropertyChangeListener edtListener;

    /**
     * Constructs a new AbstractToolkitRenderer.
     */
    protected AbstractToolkitRenderer() {
        setLayout(new BorderLayout());
    }

    /** 
     * {@inheritDoc}
     * <p>
     * This implementation manages the lifecycle of the {@link EdtPropertyChangeListener},
     * ensuring it is correctly bound to the new toolkit instance.
     * </p>
     */
    @Override
    public JPanel createToolkitPanel(T toolkit, AgiPanel parent) {
        if (this.edtListener != null) {
            this.edtListener.unbind();
        }
        
        this.anahataToolkit = toolkit;
        this.agiPanel = parent;
        
        // Dynamic binding of the listener to the new toolkit instance
        this.edtListener = new EdtPropertyChangeListener(this, toolkit, null, this::propertyChange);
        
        onBind();
        return this;
    }

    /**
     * Hook for subclasses to perform their specific layout and initial state logic.
     * Called whenever the renderer is bound to a new toolkit instance.
     */
    protected abstract void onBind();

    /** 
     * {@inheritDoc} 
     * <p>
     * Subclasses should override this method to handle reactive state changes 
     * on the Event Dispatch Thread (EDT).
     * </p> 
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        // Subclasses should override to handle state changes on the EDT
    }
}
