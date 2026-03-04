/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.tool;

import java.util.List;
import javax.swing.Icon;
import uno.anahata.asi.resource.v2.Resource;
import uno.anahata.asi.resource.v2.ResourceManager2;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.icons.PrismIcon;

/**
 * A context tree node representing the V2 Resource Manager.
 */
public class Resources2Node extends AbstractContextNode<ResourceManager2> {

    private static final Icon ICON = new PrismIcon(16);

    public Resources2Node(AgiPanel agiPanel, ResourceManager2 userObject) {
        super(agiPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "Resources (V2)";
    }

    /** {@inheritDoc} */
    @Override
    public Icon getIcon() {
        return ICON;
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "URI-centric multimodal resources.";
    }

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        return userObject.getResourcesList();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof Resource res) {
            return new Resource2Node(agiPanel, res);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Tokens aggregated from children
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else {
            this.status = "Active (" + userObject.getResourcesList().size() + ")";
        }
    }
}
