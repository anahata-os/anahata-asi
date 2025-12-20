/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import lombok.NonNull;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.provider.AbstractAiProvider;

/**
 * The main container for the Anahata AI Swing UI, managing multiple chat sessions.
 * It provides a session list and a display area for the active chat.
 * 
 * @author gemini-3-flash-preview
 */
public class MainPanel extends JPanel implements LiveSessionsPanel.SessionController {

    private final LiveSessionsPanel sessionsPanel;
    private final JPanel chatContainer;
    private final CardLayout cardLayout;
    private final SwingChatConfig baseConfig;
    private final List<Class<? extends AbstractAiProvider>> defaultProviders = new ArrayList<>();

    /**
     * Constructs a new MainPanel.
     * 
     * @param baseConfig The base configuration to use for new sessions.
     */
    public MainPanel(@NonNull SwingChatConfig baseConfig) {
        this.baseConfig = baseConfig;
        this.defaultProviders.addAll(baseConfig.getProviderClasses());
        
        setLayout(new BorderLayout());

        sessionsPanel = new LiveSessionsPanel();
        sessionsPanel.setController(this);

        cardLayout = new CardLayout();
        chatContainer = new JPanel(cardLayout);

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, sessionsPanel, chatContainer);
        splitPane.setDividerLocation(300);
        add(splitPane, BorderLayout.CENTER);
    }

    /**
     * Starts the background refresh of the session list.
     */
    public void start() {
        sessionsPanel.startRefresh();
    }

    @Override
    public void focus(@NonNull Chat chat) {
        String id = chat.getConfig().getSessionId();
        
        // Check if we already have a panel for this chat
        boolean exists = false;
        for (java.awt.Component c : chatContainer.getComponents()) {
            if (id.equals(c.getName())) {
                exists = true;
                break;
            }
        }

        if (!exists) {
            ChatPanel panel = new ChatPanel(chat);
            panel.setName(id);
            panel.initComponents();
            chatContainer.add(panel, id);
        }

        cardLayout.show(chatContainer, id);
    }

    @Override
    public void close(@NonNull Chat chat) {
        String id = chat.getConfig().getSessionId();
        for (java.awt.Component c : chatContainer.getComponents()) {
            if (id.equals(c.getName())) {
                chatContainer.remove(c);
                break;
            }
        }
        revalidate();
        repaint();
    }

    @Override
    public void dispose(@NonNull Chat chat) {
        close(chat);
        chat.shutdown();
    }

    @Override
    public void createNew() {
        SwingChatConfig newConfig = new SwingChatConfig(baseConfig.getAiConfig());
        newConfig.getProviderClasses().addAll(defaultProviders);
        Chat newChat = new Chat(newConfig);
        focus(newChat);
    }
}
