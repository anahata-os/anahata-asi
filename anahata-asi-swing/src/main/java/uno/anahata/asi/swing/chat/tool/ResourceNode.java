/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.swing.chat.ChatPanel;

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
     * @param chatPanel The parent chat panel.
     * @param userObject The resource to wrap.
     */
    public ResourceNode(ChatPanel chatPanel, AbstractResource<?, ?> userObject) {
        super(chatPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return userObject.getName();
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return userObject.getDescription();
    }

    /**
     * {@inheritDoc}
     * Implementation details: A resource is always a leaf node.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        return Collections.emptyList();
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
        // Determine status string: DELETED/STALE take priority, otherwise show lowercase policy
        if (!userObject.exists()) {
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
