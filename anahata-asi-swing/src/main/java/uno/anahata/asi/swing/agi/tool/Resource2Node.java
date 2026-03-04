/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.tool;

import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.resource.v2.Resource;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A context tree node representing a single V2 managed resource.
 */
@Slf4j
public class Resource2Node extends AbstractContextNode<Resource> {

    public Resource2Node(AgiPanel agiPanel, Resource userObject) {
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
        return "URI: " + userObject.getHandle().getUri();
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
        int tokens = (userObject.getView() != null) ? userObject.getView().getTokenCount(userObject.getHandle()) : 0;
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
        } else if (!userObject.getHandle().exists()) {
            this.status = "OFFLINE";
        } else {
            this.status = userObject.getRefreshPolicy().name().toLowerCase();
        }
    }
}
