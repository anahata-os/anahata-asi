/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.UserMessage;
import uno.anahata.ai.swing.chat.render.AbstractMessagePanel;
import uno.anahata.ai.swing.chat.render.ModelMessagePanel;
import uno.anahata.ai.swing.chat.render.UserMessagePanel;
import uno.anahata.ai.swing.components.ScrollablePanel;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;

/**
 * The main container for the conversation history, responsible for rendering
 * a list of {@link AbstractMessagePanel} instances. It handles incremental
 * updates by listening to the {@link uno.anahata.ai.context.ContextManager} for history changes.
 *
 * @author pablo
 */
@Getter
@Slf4j
public class ConversationPanel extends JPanel {

    /** The parent chat panel. */
    private final ChatPanel chatPanel;
    /** The chat session. */
    private Chat chat;
    /** The panel containing the message components. */
    private final ScrollablePanel messagesPanel;
    /** The scroll pane for the conversation. */
    private final JScrollPane scrollPane;
    /** Cache of message panels to support incremental updates. */
    private final Map<AbstractMessage, AbstractMessagePanel> cachedMessagePanels = new HashMap<>();
    /** The listener for history changes. */
    private EdtPropertyChangeListener historyListener;
    
    /**
     * Constructs a new ConversationPanel.
     *
     * @param chatPanel The parent chat panel.
     */
    public ConversationPanel(@NonNull ChatPanel chatPanel) {
        super(new BorderLayout());
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();

        this.messagesPanel = new ScrollablePanel();
        this.messagesPanel.setLayout(new BoxLayout(messagesPanel, BoxLayout.Y_AXIS));
        this.messagesPanel.setOpaque(false);

        this.scrollPane = new JScrollPane(messagesPanel);
        this.scrollPane.setBorder(null);
        this.scrollPane.getVerticalScrollBar().setUnitIncrement(16);
        add(scrollPane, BorderLayout.CENTER);

        // Declarative, thread-safe binding to the history property
        this.historyListener = new EdtPropertyChangeListener(this, chat.getContextManager(), "history", evt -> render());
    }

    /**
     * Reloads the panel with the new chat state.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        
        // Re-bind the listeners to the new chat/context manager
        if (historyListener != null) {
            historyListener.unbind();
        }
        
        this.historyListener = new EdtPropertyChangeListener(this, chat.getContextManager(), "history", evt -> render());
        //this.chatStateListener = new EdtPropertyChangeListener(this, chat, "running", evt -> render());
        
        // Clear cache and UI
        cachedMessagePanels.clear();
        messagesPanel.removeAll();
        
        render();
    }

    /**
     * Renders the conversation view by incrementally updating the message panels.
     * It identifies new, removed, or changed messages and updates the UI accordingly.
     */
    public void render() {        
        List<AbstractMessage> history = chat.getContextManager().getHistory();
        log.info("Rendering history begins: " + history.size() + " messages: " + history);

        // 1. Remove panels for messages no longer in history
        List<AbstractMessage> toRemove = cachedMessagePanels.keySet().stream()
                .filter(msg -> !history.contains(msg))
                .collect(Collectors.toList());
        
        log.info("Rendering history will remove : " + toRemove.size() + " messages. Before remove: " + history.size() + " messages");

        for (AbstractMessage msg : toRemove) {
            AbstractMessagePanel panel = cachedMessagePanels.remove(msg);
            if (panel != null) {
                log.info("Removing panel for" + panel);
                messagesPanel.remove(panel);
            }
        }

        
        // 2. Add or update panels for current history
        log.info("Entering history loop, history size: " + history.size() );
        boolean added = false;
        for (int i = 0; i < history.size(); i++) {
            
            AbstractMessage msg = history.get(i);            
            AbstractMessagePanel panel = cachedMessagePanels.get(msg);
            log.info(i + " / " + history.size() + " Processing message: " + msg + " panel " + panel);

            if (panel == null) {
                panel = createMessagePanel(msg);
                log.info("Created Message Panel for " + msg);
                if (panel != null) {
                    cachedMessagePanels.put(msg, panel);
                    added = true;
                    log.info("Created Message Panel");
                }
            }

            if (panel != null) {
                if (i >= messagesPanel.getComponentCount() || messagesPanel.getComponent(i) != panel) {
                    messagesPanel.add(panel, i);
                }
                panel.render();
            }
        }

        // 3. Clean up trailing components and add glue
        while (messagesPanel.getComponentCount() > history.size()) {
            messagesPanel.remove(messagesPanel.getComponentCount() - 1);
        }
        messagesPanel.add(Box.createVerticalGlue());

        if (added) {
            scrollToBottom();
        }

        revalidate();
        repaint();
        
        log.info("Rendering history ends: " + history.size() + " elements");
    }

    /**
     * Factory method to create a specific message panel for a given message.
     *
     * @param message The message to create a panel for.
     * @return The created message panel, or null if no panel is available for the message type.
     */
    private AbstractMessagePanel createMessagePanel(AbstractMessage message) {
        if (message instanceof UserMessage userMessage) {
            return new UserMessagePanel(chatPanel, userMessage);
        } else if (message instanceof AbstractModelMessage modelMessage) {
            return new ModelMessagePanel(chatPanel, modelMessage);
        }
        return null;
    }

    /**
     * Checks if the scroll pane is currently at the bottom.
     * 
     * @return true if at the bottom.
     */
    public boolean isAtBottom() {
        JScrollBar verticalBar = scrollPane.getVerticalScrollBar();
        int extent = verticalBar.getModel().getExtent();
        int maximum = verticalBar.getModel().getMaximum();
        int value = verticalBar.getModel().getValue();
        return (value + extent) >= (maximum - 10); // 10 pixel threshold
    }

    /**
     * Scrolls the conversation view to the bottom.
     */
    public void scrollToBottom() {
        SwingUtilities.invokeLater(() -> {
            scrollPane.getVerticalScrollBar().setValue(scrollPane.getVerticalScrollBar().getMaximum());
        });
    }
}
