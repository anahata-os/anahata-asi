/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.tool;

import java.util.List;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A context tree node representing the conversation history.
 *
 * @author anahata
 */
public class HistoryNode extends AbstractContextNode<ContextManager> {

    /**
     * Constructs a new HistoryNode.
     * @param agiPanel The parent agi panel.
     * @param userObject The context manager to wrap.
     */
    public HistoryNode(AgiPanel agiPanel, ContextManager userObject) {
        super(agiPanel, userObject);
    }

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return "History";
    }

    /** {@inheritDoc} */
    @Override
    public String getDescription() {
        return "The persistent conversation history, including user messages, model responses, and tool calls.";
    }

    /** {@inheritDoc} */
    @Override
    protected List<?> fetchChildObjects() {
        return userObject.getHistory();
    }

    /** {@inheritDoc} */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractMessage msg) {
            return new MessageNode(agiPanel, msg);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    protected void calculateLocalTokens() {
        // History tokens are aggregated from MessageNodes
    }

    /** {@inheritDoc} */
    @Override
    protected void updateStatus() {
        List<AbstractMessage> history = userObject.getHistory();
        this.status = (history != null ? history.size() : 0) + " messages";
    }
}
