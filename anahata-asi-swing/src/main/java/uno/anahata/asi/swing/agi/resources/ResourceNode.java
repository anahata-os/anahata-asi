/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources;

import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.context.AbstractContextNode;

/**
 * A context tree node representing a single V2 managed resource.
 */
@Slf4j
public class ResourceNode extends AbstractContextNode<Resource> {

    /**
     * Constructs a new ResourceNode.
     * @param agiPanel The parent AgiPanel.
     * @param userObject The Resource domain object.
     */
    public ResourceNode(AgiPanel agiPanel, Resource userObject) {
        super(agiPanel, userObject);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Prioritizes the model-provided HTML display 
     * name for rich rendering in the context tree.</p>
     */
    @Override
    public String getName() {
        String html = userObject.getHtmlDisplayName();
        return html != null ? html : userObject.getName();
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Provides the underlying resource URI as the 
     * primary descriptive identifier.</p>
     */
    @Override
    public String getDescription() {
        return "URI: " + userObject.getHandle().getUri();
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Resources are leaf nodes in the context tree 
     * and do not have children.</p>
     */
    @Override
    protected List<?> fetchChildObjects() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        return null;
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Calculates token usage based on the 
     * associated view and assigns it to either SYSTEM or RAG buckets.</p>
     */
    @Override
    protected void calculateLocalTokens() {
        int tokens = (userObject.getView() != null) ? userObject.getView().getTokenCount() : 0;
        if (userObject.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
            this.instructionsTokens = tokens;
        } else {
            this.ragTokens = tokens;
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Reflects the real-time availability and 
     * refresh policy of the resource.</p>
     */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else if (!userObject.isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else if (!userObject.getHandle().exists()) {
            this.status = "OFFLINE";
        } else {
            this.status = userObject.getRefreshPolicy().name().toLowerCase();
        }
    }
}
