/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A context tree node representing a single managed resource.
 * <p>
 * This node displays the resource's name, its position in the prompt, 
 * and its current status (e.g., refresh policy, staleness, or deletion).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class ResourceNode extends AbstractContextNode<AbstractResource<?, ?>> {

    /**
     * Constructs a new ResourceNode.
     * @param agiPanel The parent agi panel.
     * @param userObject The resource to wrap.
     */
    public ResourceNode(AgiPanel agiPanel, AbstractResource<?, ?> userObject) {
        super(agiPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        String html = userObject.getHtmlDisplayName();
        return html != null ? html : userObject.getName();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return userObject.getDescription();
    }

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        int tokens = userObject.getTokenCount();
        if (userObject.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
            this.instructionsTokens = tokens;
        } else {
            this.ragTokens = tokens;
        }
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else if (!userObject.isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else if (!userObject.exists()) {
            this.status = "DELETED";
        } else {
            try {
                if (userObject.isStale()) {
                    this.status = "STALE";
                } else {
                    this.status = userObject.getRefreshPolicy().name().toLowerCase();
                }
            } catch (IOException e) {
                log.error("Error checking resource status", e);
                this.status = "ERROR";
            }
        }
    }
}
