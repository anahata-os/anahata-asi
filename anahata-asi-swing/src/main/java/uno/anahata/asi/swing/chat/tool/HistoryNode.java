/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.model.core.AbstractMessage;

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
     * @param userObject The context manager to wrap.
     */
    public HistoryNode(ContextManager userObject) {
        super(userObject);
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
                    children.add(new MessageNode(msg));
                }
            }
        }
        return children;
    }

    /** {@inheritDoc} */
    @Override
    public void refreshTokens() {
        this.instructionsTokens = 0;
        this.declarationsTokens = 0;
        this.historyTokens = 0;
        this.ragTokens = 0;
        
        List<AbstractMessage> history = userObject.getHistory();
        if (history != null) {
            for (AbstractMessage msg : history) {
                this.historyTokens += msg.getTokenCount(false);
            }
        }
        
        this.status = (history != null ? history.size() : 0) + " messages";
        
        for (AbstractContextNode<?> child : getChildren()) {
            child.refreshTokens();
        }
    }
}
