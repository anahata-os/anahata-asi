/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.awt.BorderLayout;
import lombok.Getter;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.swing.AbstractAsiContainerPanel;

/**
 * Abstract base class for container-level dashboard {@link TopComponent}s
 * (Cards and Table views).
 * <p>
 * Manages the embedding of an {@link AbstractAsiContainerPanel}, automatically
 * binding its periodic refresh timer to the TopComponent's visibility lifecycle
 * ({@link #componentOpened()} and {@link #componentClosed()}), and implementing
 * {@link ReloadableTopComponent} to guarantee timer stoppage during module
 * reloads.
 * </p>
 *
 * @param <P> The concrete {@link AbstractAsiContainerPanel} type.
 * @author anahata
 */
@Getter
public abstract class AbstractAsiContainerTopComponent<P extends AbstractAsiContainerPanel>
        extends TopComponent implements ReloadableTopComponent {

    /**
     * The UI panel displaying the container dashboard.
     */
    protected final P sessionsPanel;

    /**
     * Constructs a new container TopComponent wrapping the given sessions
     * panel.
     *
     * @param sessionsPanel The container UI panel instance.
     */
    protected AbstractAsiContainerTopComponent(P sessionsPanel) {
        this.sessionsPanel = sessionsPanel;
        setLayout(new BorderLayout());
        if (sessionsPanel != null) {
            add(sessionsPanel, BorderLayout.CENTER);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Starts the periodic refresh timer on the underlying panel when the tab
     * becomes active.
     * </p>
     */
    @Override
    public void componentOpened() {
        super.componentOpened();
        if (sessionsPanel != null) {
            sessionsPanel.startRefresh();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Stops the periodic refresh timer on the underlying panel to conserve CPU
     * and prevent leaks.
     * </p>
     */
    @Override
    public void componentClosed() {
        super.componentClosed();
        if (sessionsPanel != null) {
            sessionsPanel.stopRefresh();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Unconditionally stops the background refresh timer and closes this
     * TopComponent, ensuring no lingering timer tasks hold onto the module
     * classloader during a reload.
     * </p>
     */
    @Override
    public void detachForNbmReload() {
        if (sessionsPanel != null) {
            sessionsPanel.stopRefresh();
        }
        Mode mode = WindowManager.getDefault().findMode(this);
        if (mode instanceof org.netbeans.core.windows.ModeImpl modeImpl) {
            modeImpl.removeTopComponent(this);
        }
        removeAll();
        close();
    }
}
