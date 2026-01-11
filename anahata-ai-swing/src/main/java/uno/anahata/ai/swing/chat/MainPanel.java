/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.chat.ChatRegistry;
import uno.anahata.ai.model.provider.AbstractAiProvider;

/**
 * The main container for the Anahata AI Swing UI, managing multiple chat sessions.
 * It provides a session list and a tabbed area for active chats.
 * <p>
 * This panel implements {@link ChatRegistry.ChatRegistryListener} to automatically
 * synchronize the tabbed pane with the global list of active sessions.
 * 
 * @author gemini-3-flash-preview
 */
@Slf4j
public class MainPanel extends JPanel implements SessionsPanel.SessionController, ChatRegistry.ChatRegistryListener {

    private final SessionsPanel sessionsPanel;
    private final JTabbedPane tabbedPane;
    private final SwingChatConfig baseConfig;
    private final List<Class<? extends AbstractAiProvider>> defaultProviders = new ArrayList<>();
    private final PropertyChangeListener nicknameListener = this::handleNicknameChange;

    /**
     * Constructs a new MainPanel.
     * 
     * @param baseConfig The base configuration to use for new sessions.
     */
    public MainPanel(@NonNull SwingChatConfig baseConfig) {
        this.baseConfig = baseConfig;
        this.defaultProviders.addAll(baseConfig.getProviderClasses());
        
        setLayout(new BorderLayout());

        sessionsPanel = new SessionsPanel();
        sessionsPanel.setController(this);

        tabbedPane = new JTabbedPane();
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        
        // Enable closable tabs via FlatLaf properties
        tabbedPane.putClientProperty("JTabbedPane.tabClosable", true);
        tabbedPane.putClientProperty("JTabbedPane.tabCloseCallback", (BiConsumer<JTabbedPane, Integer>) (tabPane, tabIndex) -> {
            Component comp = tabPane.getComponentAt(tabIndex);
            if (comp instanceof ChatPanel chatPanel) {
                close(chatPanel.getChat());
            }
        });

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, sessionsPanel, tabbedPane);
        splitPane.setDividerLocation(300);
        splitPane.setOneTouchExpandable(true);
        add(splitPane, BorderLayout.CENTER);
        
        ChatRegistry.addListener(this);
    }

    /**
     * Starts the background refresh of the session list.
     */
    public void start() {
        sessionsPanel.startRefresh();
        // Sync existing chats
        for (Chat chat : ChatRegistry.getActiveChats()) {
            chatRegistered(chat);
        }
    }

    @Override
    public void focus(@NonNull Chat chat) {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(() -> focus(chat));
            return;
        }

        String id = chat.getConfig().getSessionId();
        log.info("Focusing session: {}", id);
        
        // Check if we already have a tab for this chat
        int tabIndex = -1;
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            Component comp = tabbedPane.getComponentAt(i);
            if (id.equals(comp.getName())) {
                tabIndex = i;
                break;
            }
        }

        if (tabIndex == -1) {
            log.info("Creating new tab for session: {}", id);
            ChatPanel panel = new ChatPanel(chat);
            panel.setName(id);
            panel.initComponents();
            
            String title = chat.getNickname();
            if (title == null || title.isEmpty()) {
                title = id.substring(0, Math.min(id.length(), 8));
            }
            
            tabbedPane.addTab(title, panel);
            tabIndex = tabbedPane.getTabCount() - 1;
            
            // Listen for nickname changes to update the tab title
            chat.addPropertyChangeListener(nicknameListener);
        }

        tabbedPane.setSelectedIndex(tabIndex);
        revalidate();
        repaint();
    }

    @Override
    public void close(@NonNull Chat chat) {
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingUtilities.invokeLater(() -> close(chat));
            return;
        }

        String id = chat.getConfig().getSessionId();
        log.info("Closing tab for session: {}", id);
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            if (id.equals(tabbedPane.getComponentAt(i).getName())) {
                tabbedPane.removeTabAt(i);
                chat.removePropertyChangeListener(nicknameListener);
                break;
            }
        }
        revalidate();
        repaint();
    }

    @Override
    public void dispose(@NonNull Chat chat) {
        log.info("Disposing session: {}", chat.getConfig().getSessionId());
        close(chat);
        chat.shutdown();
    }

    @Override
    public void createNew() {
        log.info("Creating new session...");
        SwingChatConfig newConfig = new SwingChatConfig(baseConfig.getAiConfig());
        newConfig.getProviderClasses().addAll(defaultProviders);
        Chat newChat = new Chat(newConfig);
        // Chat constructor registers itself in ChatRegistry, which triggers chatRegistered
        focus(newChat);
    }

    @Override
    public void chatRegistered(Chat chat) {
        log.info("Chat registered event received for: {}", chat.getConfig().getSessionId());
        focus(chat);
    }

    @Override
    public void chatUnregistered(Chat chat) {
        log.info("Chat unregistered event received for: {}", chat.getConfig().getSessionId());
        close(chat);
    }

    private void handleNicknameChange(PropertyChangeEvent evt) {
        if ("nickname".equals(evt.getPropertyName())) {
            Chat chat = (Chat) evt.getSource();
            String id = chat.getConfig().getSessionId();
            String newNickname = (String) evt.getNewValue();
            
            SwingUtilities.invokeLater(() -> {
                for (int i = 0; i < tabbedPane.getTabCount(); i++) {
                    if (id.equals(tabbedPane.getComponentAt(i).getName())) {
                        tabbedPane.setTitleAt(i, newNickname);
                        break;
                    }
                }
            });
        }
    }
}
