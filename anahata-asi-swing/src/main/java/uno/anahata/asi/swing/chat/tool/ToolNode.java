/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.model.tool.AbstractTool;

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
     * @param userObject The tool to wrap.
     */
    public ToolNode(AbstractTool<?, ?> userObject) {
        super(userObject);
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

    /**
     * {@inheritDoc}
     * Implementation details: A tool is always a leaf node.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    public void refreshTokens() {
        this.instructionsTokens = 0;
        this.declarationsTokens = userObject.getTokenCount();
        this.historyTokens = 0;
        this.ragTokens = 0;
        this.status = userObject.getPermission().name();
    }
}
