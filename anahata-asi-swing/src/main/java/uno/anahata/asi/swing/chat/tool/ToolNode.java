/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.model.tool.AbstractToolkit;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A context tree node representing an {@link AbstractTool}.
 * <p>
 * This is a leaf node in the context hierarchy, representing an individual 
 * executable function. It displays the tool's declaration token count and 
 * its current permission status.
 * </p>
 *
 * @author anahata
 */
public class ToolNode extends AbstractContextNode<AbstractTool<?, ?>> {

    /**
     * Constructs a new ToolNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The tool to wrap.
     */
    public ToolNode(ChatPanel chatPanel, AbstractTool<?, ?> userObject) {
        super(chatPanel, userObject);
    }

    /** 
     * {@inheritDoc} 
     * Returns the simple name of the tool, removing any toolkit prefix.
     */
    @Override
    public String getName() {
        String fullName = userObject.getName();
        int lastDot = fullName.lastIndexOf('.');
        return lastDot != -1 ? fullName.substring(lastDot + 1) : fullName;
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
        this.declarationsTokens = userObject.getTokenCount();
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        if (userObject.getPermission() == ToolPermission.DENY_NEVER) {
            this.status = "Disabled";
        } else {
            AbstractToolkit<?> tk = userObject.getToolkit();
            if (tk != null && (!tk.isEnabled() || !tk.getToolManager().isEffectivelyProviding())) {
                this.status = "Disabled (Inherited)";
            } else {
                this.status = userObject.getPermission().name();
            }
        }
    }
}
