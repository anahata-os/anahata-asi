/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

/**
 * Interface implemented by all NetBeans {@link org.openide.windows.TopComponent}s
 * in the Anahata ASI module that require deterministic detachment, listener unbinding,
 * and timer stoppage during module lifecycle events such as {@code nbmreload}.
 *
 * @author anahata
 */
public interface ReloadableTopComponent {

    /**
     * Detaches this TopComponent from active listeners, background timers, and domain models,
     * ensuring that all hard references to the current {@code OneModuleClassLoader} are severed
     * before the component is closed.
     */
    void detachForNbmReload();
}
