/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.tool.ToolManager;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A specialized context tree node representing the {@link ToolManager}.
 * <p>
 * This node manages the display of the root tool manager and exposes
 * its registered toolkits as child nodes.
 * </p>
 * 
 * @author anahata
 */
public class ToolManagerNode extends AbstractContextNode<ToolManager> {

    /**
     * Constructs a new ToolManagerNode.
     *
     * @param agiPanel The parent agi panel.
     * @param userObject The tool manager to wrap.
     */
    public ToolManagerNode(AgiPanel agiPanel, ToolManager userObject) {
        super(agiPanel, userObject);
    }

    /**
     * {@inheritDoc}
     * @return The human-readable name of the tool manager.
     * @see ToolManager#getName()
     */
    @Override
    public String getName() {
        return userObject.getName();
    }

    /**
     * {@inheritDoc}
     * @return The detailed description of the tool manager's purpose.
     * @see ToolManager#getDescription()
     */
    @Override
    public String getDescription() {
        return userObject.getDescription();
    }

    /**
     * {@inheritDoc}
     * @return A list of registered toolkits as children.
     */
    @Override
    protected List<?> fetchChildObjects() {
        return new ArrayList<>(userObject.getToolkits().values());
    }

    /**
     * {@inheritDoc}
     * @param obj The toolkit domain object to wrap.
     * @return A new ToolkitNode wrapping the toolkit.
     */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractToolkit<?> tk) {
            return new ToolkitNode(agiPanel, tk);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void calculateLocalTokens() {
        if (userObject.isEffectivelyProviding()) {
            this.instructionsTokens = userObject.getInstructionsTokenCount();
            this.ragTokens = userObject.getRagTokenCount();
        } else {
            this.instructionsTokens = 0;
            this.ragTokens = 0;
        }
    }

    /**
     * {@inheritDoc}
     * @return True if the tool manager is providing context.
     */
    @Override
    public boolean isActive() {
        return userObject.isEffectivelyProviding();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void updateStatus() {
        if (!userObject.isProviding()) {
            this.status = "Disabled";
        } else if (!userObject.isEffectivelyProviding()) {
            this.status = "Disabled (Inherited)";
        } else {
            this.status = "Providing";
        }
    }
}
