/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.tool;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.model.core.AbstractMessage;

/**
 * A context tree node representing the conversation history.
 *
 * @author anahata
 */
public class HistoryNode extends AbstractContextNode<ContextManager> {

    public HistoryNode(ContextManager userObject) {
        super(userObject);
    }

    @Override
    public String getName() {
        return "History";
    }

    @Override
    public String getDescription() {
        return "The persistent conversation history, including user messages, model responses, and tool calls.";
    }

    @Override
    public List<AbstractContextNode<?>> getChildren() {
        List<AbstractContextNode<?>> children = new ArrayList<>();
        List<AbstractMessage> history = userObject.getHistory();
        if (history != null) {
            for (AbstractMessage msg : history) {
                children.add(new MessageNode(msg));
            }
        }
        return children;
    }

    @Override
    public int getInstructionsTokens(Chat chat) {
        return 0;
    }

    @Override
    public int getDeclarationsTokens() {
        return 0;
    }

    @Override
    public int getHistoryTokens(Chat chat) {
        int total = 0;
        List<AbstractMessage> history = userObject.getHistory();
        if (history != null) {
            for (AbstractMessage msg : history) {
                total += msg.getTokenCount(false);
            }
        }
        return total;
    }

    @Override
    public int getRagTokens(Chat chat) {
        return 0;
    }

    @Override
    public String getStatus() {
        List<AbstractMessage> history = userObject.getHistory();
        return (history != null ? history.size() : 0) + " messages";
    }
}
