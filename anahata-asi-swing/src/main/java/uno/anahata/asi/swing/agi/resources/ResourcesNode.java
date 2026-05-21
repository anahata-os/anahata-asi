/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.resources;

import java.util.List;
import javax.swing.Icon;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.ResourceManager;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.context.AbstractContextNode;
import uno.anahata.asi.swing.icons.PrismIcon;

/**
 * A context tree node representing the V2 Resource Manager.
 */
public class ResourcesNode extends AbstractContextNode<ResourceManager> {

    /**
     * The standard icon used for visual identification of resources root node.
     */
    private static final Icon ICON = new PrismIcon(16);

    /**
     * Constructs a new ResourcesNode.
     * @param agiPanel The parent AgiPanel.
     * @param userObject The ResourceManager domain instance.
     */
    public ResourcesNode(AgiPanel agiPanel, ResourceManager userObject) {
        super(agiPanel, userObject);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Returns the static "Resources (V2)" name 
     * for the root manager node.</p>
     */
    @Override
    public String getName() {
        return "Resources (V2)";
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Uses a specialized PrismIcon to represent 
     * the multifaceted nature of multimodal resources.</p>
     */
    @Override
    public Icon getIcon() {
        return ICON;
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "URI-centric multimodal resources.";
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Delegates to the ResourceManager to list 
     * all currently registered resources.</p>
     */
    @Override
    protected List<?> fetchChildObjects() {
        return userObject.getResourcesList();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof Resource res) {
            return new ResourceNode(agiPanel, res);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Tokens aggregated from children
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Reflects the total count of active resources 
     * managed by this container.</p>
     */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else {
            this.status = "Active (" + userObject.getResourcesList().size() + ")";
        }
    }
}
