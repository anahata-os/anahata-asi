/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.context;

import java.util.List;
import javax.swing.Icon;
import uno.anahata.asi.agi.context.ContextManager;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.icons.PulseIcon;

/**
 * A context tree node representing the conversation history.
 */
public class HistoryNode extends AbstractContextNode<ContextManager> {

    /**
     * The standard pulse icon to visualize the live conversation feed.
     */
    private static final Icon ICON = new PulseIcon(16);

    /**
     * Constructs a new HistoryNode.
     * @param agiPanel The parent agi panel.
     * @param userObject The context manager to wrap.
     */
    public HistoryNode(AgiPanel agiPanel, ContextManager userObject) {
        super(agiPanel, userObject);
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: Returns the localized "History" label for 
     * the conversation branch.
     * </p>
     */
    @Override
    public String getName() {
        return "History";
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: Returns the standard {@link PulseIcon} to 
     * visualize the live conversation feed.
     * </p>
     */
    @Override
    public Icon getIcon() {
        return ICON;
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: Explains the contents of the history branch, 
     * spanning messages from all actors and tool interactions.
     * </p>
     */
    @Override
    public String getDescription() {
        return "The persistent conversation history, including user messages, model responses, and tool calls.";
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: Retrieves the full list of {@link AbstractMessage}s 
     * from the context manager's memory.
     * </p>
     */
    @Override
    protected List<?> fetchChildObjects() {
        return userObject.getHistory();
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: Transforms each {@link AbstractMessage} in the 
     * timeline into a navigable {@link MessageNode}.
     * </p>
     */
    @Override
    protected AbstractContextNode<?> createChildNode(Object obj) {
        if (obj instanceof AbstractMessage msg) {
            return new MessageNode(agiPanel, msg);
        }
        return null;
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: Token counts for history are entirely aggregated 
     * from the constituent message nodes.
     * </p>
     */
    @Override
    protected void calculateLocalTokens() {
        // History tokens are aggregated from MessageNodes
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Implementation details: Updates the status with a count of the current 
     * message stack depth.
     * </p>
     */
    @Override
    protected void updateStatus() {
        List<AbstractMessage> history = userObject.getHistory();
        this.status = (history != null ? history.size() : 0) + " messages";
    }
}
