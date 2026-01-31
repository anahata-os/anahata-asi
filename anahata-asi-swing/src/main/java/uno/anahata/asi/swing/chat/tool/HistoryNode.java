/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A context tree node representing the conversation history.
 *
 * @author anahata
 */
public class HistoryNode extends AbstractContextNode<ContextManager> {

    /** The cached list of children. */
    private List<AbstractContextNode<?>> children;

    /**
     * Constructs a new HistoryNode.
     * @param chatPanel The parent chat panel.
     * @param userObject The context manager to wrap.
     */
    public HistoryNode(ChatPanel chatPanel, ContextManager userObject) {
        super(chatPanel, userObject);
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
    public List<AbstractContextNode<?>> getChildren() {
        if (children == null) {
            children = new ArrayList<>();
            List<AbstractMessage> history = userObject.getHistory();
            if (history != null) {
                for (AbstractMessage msg : history) {
                    children.add(new MessageNode(chatPanel, msg));
                }
            }
        }
        return children;
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
