/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import lombok.Getter;
import lombok.Setter;

/**
 * Base implementation for resource views providing parent resource management.
 * <p>
 * This class manages the link back to the parent {@link Resource}, allowing 
 * views to trigger reactive reloads when their internal configuration changes.
 * </p>
 */
public abstract class AbstractResourceView implements ResourceView {

    /** 
     * The parent resource orchestrator. 
     * Circularity is handled natively by Kryo.
     */
    @Getter
    @Setter
    protected Resource owner;

    /**
     * Triggers a markDirty on the owner resource to signal that 
     * the view's settings have changed and need re-interpretation.
     */
    public void markDirty() {
        if (owner != null) {
            owner.markDirty();
        }
    }
}
