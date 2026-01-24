/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.chat.Chat;
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
     * Implementation details: A tool is always a leaf node.
     */
    @Override
    public List<AbstractContextNode<?>> getChildren() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    public int getInstructionsTokens(Chat chat) {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public int getDeclarationsTokens() {
        return userObject.getTokenCount();
    }

    @Override
    public int getHistoryTokens(Chat chat) {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public int getRagTokens(Chat chat) {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public String getStatus() {
        return userObject.getPermission().name();
    }
}
