/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.BorderLayout;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
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
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.swing.chat.render.AbstractMessagePanel;
import uno.anahata.asi.swing.chat.render.MessagePanelFactory;
import uno.anahata.asi.swing.components.ScrollablePanel;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * The main container for the conversation history, responsible for rendering
 * a list of {@link AbstractMessagePanel} instances. It handles incremental
 * updates by listening to the {@link uno.anahata.asi.context.ContextManager} for history changes.
 *
 * @author anahata
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
     * Flag indicating if the view should automatically scroll to the bottom 
     * when content changes. 
     */
    private boolean autoScroll = true;

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

        // --- Smart Scroll Logic (Opt-Out) ---
        
        // 1. MouseWheel: Explicitly detect user intent to scroll up or down.
        this.scrollPane.addMouseWheelListener(e -> {
            if (e.getWheelRotation() < 0) {
                if (autoScroll) {
                    log.debug("[Scroll] User scrolled UP with wheel, disabling autoScroll");
                    autoScroll = false;
                }
            } else {
                if (isAtBottom() && !autoScroll) {
                    log.debug("[Scroll] User scrolled to BOTTOM with wheel, re-enabling autoScroll");
                    autoScroll = true;
                }
            }
        });

        // 2. AdjustmentListener: Handle scrollbar dragging and track clicks.
        this.scrollPane.getVerticalScrollBar().addAdjustmentListener(e -> {
            boolean atBottom = isAtBottom();
            
            if (e.getValueIsAdjusting()) {
                // User is actively dragging the scrollbar.
                if (autoScroll && !atBottom) {
                    log.debug("[Scroll] User dragged scrollbar UP, disabling autoScroll");
                    autoScroll = false;
                } else if (!autoScroll && atBottom) {
                    //log.info("[Scroll] User dragged scrollbar to BOTTOM, re-enabling autoScroll");
                    autoScroll = true;
                }
            } else {
                // Programmatic change or track click.
                // We ONLY re-enable autoScroll if we land at the bottom.
                // We NEVER disable it here to avoid false positives during layout shifts.
                if (atBottom && !autoScroll) {
                    log.info("[Scroll] Adjustment landed at BOTTOM, re-enabling autoScroll");
                    autoScroll = true;
                }
            }
        });

        // 3. ComponentListener: Trigger the actual scroll when content size changes.
        // This is the correct way to detect when the messagesPanel grows.
        this.messagesPanel.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                if (autoScroll) {
                    scrollToBottom();
                }
            }
        });

        // Declarative, thread-safe binding to the history property.
        // We use triggerImmediately=true to ensure the initial history is rendered 
        // as soon as the component becomes displayable.
        this.historyListener = new EdtPropertyChangeListener(this, chat.getContextManager(), "history", 
                evt -> render(), EdtPropertyChangeListener.Mode.INVOKE_LATER, true);
        
        // Global listener for "show pruned" toggle to refresh visibility of all panels.
        new EdtPropertyChangeListener(this, chatPanel.getChatConfig(), "showPruned", evt -> refreshVisibility());
    }

    /**
     * Reloads the panel with the new chat state.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        
        if (historyListener != null) {
            historyListener.unbind();
        }
        
        this.historyListener = new EdtPropertyChangeListener(this, chat.getContextManager(), "history", 
                evt -> render(), EdtPropertyChangeListener.Mode.INVOKE_LATER, true);
        
        cachedMessagePanels.clear();
        messagesPanel.removeAll();
        
        // Note: No manual render() call needed here as the listener will trigger it 
        // immediately upon subscription if the component is displayable.
    }

    /**
     * Renders the conversation view by incrementally updating the message panels.
     * <p>
     * This method is now strictly structural: it adds or removes panels based on 
     * the history but does NOT call render() on the panels themselves. Panels 
     * are responsible for their own rendering via property change listeners.
     * </p>
     */
    public void render() {        
        // Filter out tool messages from the visible history.
        List<AbstractMessage> history = chat.getContextManager().getHistory().stream()
                .filter(msg -> !(msg instanceof AbstractToolMessage))
                .collect(Collectors.toList());
        
        log.info("Updating conversation structure for session {}: {} messages", 
                chat.getConfig().getSessionId(), history.size());

        List<AbstractMessage> toRemove = cachedMessagePanels.keySet().stream()
                .filter(msg -> !history.contains(msg))
                .collect(Collectors.toList());
        
        for (AbstractMessage msg : toRemove) {
            AbstractMessagePanel panel = cachedMessagePanels.remove(msg);
            if (panel != null) {
                messagesPanel.remove(panel);
            }
        }

        boolean added = false;
        for (int i = 0; i < history.size(); i++) {
            AbstractMessage msg = history.get(i);            
            AbstractMessagePanel panel = cachedMessagePanels.get(msg);

            if (panel == null) {
                panel = createMessagePanel(msg);
                if (panel != null) {
                    cachedMessagePanels.put(msg, panel);
                    added = true;
                }
            }

            if (panel != null) {
                if (i >= messagesPanel.getComponentCount() || messagesPanel.getComponent(i) != panel) {
                    messagesPanel.add(panel, i);
                }
                // FIXED: Removed recursive panel.render() call. 
                // The panel will render itself via addNotify() or property change listeners.
            }
        }

        while (messagesPanel.getComponentCount() > history.size()) {
            messagesPanel.remove(messagesPanel.getComponentCount() - 1);
        }
        messagesPanel.add(Box.createVerticalGlue());

        if (added) {
            log.info("New message added, forcing autoScroll to true.");
            autoScroll = true; 
            scrollToBottom();
        }

        revalidate();
        repaint();
    }

    /**
     * Refreshes the visibility and visual state of all cached message panels.
     * This is called when global settings like 'showPruned' change.
     */
    public void refreshVisibility() {
        log.info("Refreshing visibility for all message panels.");
        for (AbstractMessagePanel panel : cachedMessagePanels.values()) {
            panel.render();
        }
        revalidate();
        repaint();
    }

    private AbstractMessagePanel createMessagePanel(AbstractMessage message) {
        return MessagePanelFactory.createMessagePanel(chatPanel, message);
    }

    public boolean isAtBottom() {
        JScrollBar verticalBar = scrollPane.getVerticalScrollBar();
        int extent = verticalBar.getModel().getExtent();
        int maximum = verticalBar.getModel().getMaximum();
        int value = verticalBar.getModel().getValue();
        
        if (maximum <= extent) {
            return true; 
        }
        
        // Use a slightly larger threshold (40px) to account for layout jitter during streaming.
        boolean atBottom = (value + extent) >= (maximum - 40);
        //log.info("[Scroll] isAtBottom: value={}, extent={}, maximum={}, result={}", value, extent, maximum, atBottom);
        return atBottom;
    }

    public void scrollToBottom() {
        log.debug("[Scroll] scrollToBottom() triggered");
        SwingUtilities.invokeLater(() -> {
            JScrollBar verticalBar = scrollPane.getVerticalScrollBar();
            int max = verticalBar.getMaximum();
            log.debug("[Scroll] Setting scrollbar value to {}", max);
            verticalBar.setValue(max);
        });
    }
}
