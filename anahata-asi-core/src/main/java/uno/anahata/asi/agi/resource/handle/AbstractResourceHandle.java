/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.handle;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;

/**
 * Base implementation of {@link ResourceHandle} providing common owner management.
 * <p>
 * This class centralizes the back-reference to the parent {@link Resource}, 
 * ensuring that all handle implementations can trigger reactive reloads via 
 * {@code owner.markDirty()}.
 * </p>
 */
@Slf4j
public abstract class AbstractResourceHandle implements ResourceHandle {

    /** 
     * The parent resource orchestrator. 
     * We don't mark this transient because Kryo handles circular references 
     * automatically, preserving the bidirectional link during persistence.
     */
    @Getter
    @Setter
    protected Resource owner;

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        log.debug("Rebinding resource handle: {}", getUri());
        // Subclasses can override to restore listeners or connection state
    }
}
