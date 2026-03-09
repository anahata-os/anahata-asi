/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.util.List;
import javax.swing.Icon;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.icons.AtomsIcon;

/**
 * A context tree node that acts as a container for all tools within a toolkit.
 * It uses the AtomsIcon to represent the modular capabilities of the ASI.
 * 
 * @author anahata
 */
public class ToolsNode extends AbstractContextNode<AbstractToolkit<?>> {

    private static final Icon ICON = new AtomsIcon(16);

    /**
     * Constructs a new ToolsNode.
     * @param agiPanel The parent agi panel.
     * @param userObject The parent toolkit.
     */
    public ToolsNode(AgiPanel agiPanel, AbstractToolkit<?> userObject) {
        super(agiPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "Tools";
    }

    /** {@inheritDoc} */
    @Override
    public Icon getIcon() {
        return ICON;
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "Available tools provided by the " + userObject.getName() + " toolkit.";
    }

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        return userObject.getAllTools();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractTool<?, ?> tool) {
            return new ToolNode(agiPanel, tool);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // Tools tokens are aggregated from ToolNodes
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (!userObject.isEnabled()) {
            this.status = "Disabled";
        } else if (!userObject.getToolManager().isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else {
            this.status = userObject.getAllTools().size() + " tools";
        }
    }
}
