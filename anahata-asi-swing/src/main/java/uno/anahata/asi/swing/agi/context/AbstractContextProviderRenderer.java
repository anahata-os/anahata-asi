/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.context;

import java.awt.BorderLayout;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JPanel;
import lombok.Getter;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.event.PropertyChangeSource;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * Abstract base class for specialized UI renderers for {@link ContextProvider} instances.
 * <p>
 * Extends {@link JPanel} and provides automatic EDT-safe property change listener binding
 * to the context provider instance.
 * </p>
 *
 * @param <T> The specific context provider type.
 * @author anahata
 */
public abstract class AbstractContextProviderRenderer<T extends ContextProvider> extends JPanel implements PropertyChangeListener {

    /** The bound context provider instance. */
    @Getter
    protected T contextProvider;

    /** The parent ContextPanel. */
    @Getter
    protected ContextPanel contextPanel;

    /** Reactive property change listener delegate. */
    private EdtPropertyChangeListener edtListener;

    /**
     * Constructs a new AbstractContextProviderRenderer with BorderLayout.
     */
    protected AbstractContextProviderRenderer() {
        setLayout(new BorderLayout());
    }

    /**
     * Binds the renderer to a specific context provider instance and parent context panel.
     * Automatically wires the reactive {@link EdtPropertyChangeListener}.
     *
     * @param provider The context provider instance.
     * @param parent The parent ContextPanel.
     * @return This renderer instance as a JPanel.
     */
    public JPanel createProviderPanel(T provider, ContextPanel parent) {
        if (this.edtListener != null) {
            this.edtListener.unbind();
        }

        this.contextProvider = provider;
        this.contextPanel = parent;

        if (provider instanceof PropertyChangeSource pcs) {
            this.edtListener = new EdtPropertyChangeListener(this, pcs, null, this::propertyChange);
        }

        onBind();
        return this;
    }

    /**
     * Hook for subclasses to perform their specific layout and initial state logic.
     * Called whenever the renderer is bound to a new context provider instance.
     */
    protected abstract void onBind();

    /**
     * Subclasses should override this method to handle reactive state changes on the EDT.
     *
     * @param evt The property change event.
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        // Subclasses override to handle state changes on the EDT
    }
}
