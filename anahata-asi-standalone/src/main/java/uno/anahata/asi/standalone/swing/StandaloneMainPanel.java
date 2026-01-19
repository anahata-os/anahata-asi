/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.standalone.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.AsiContainerPanel;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * The main container for the Anahata AI Swing UI, managing multiple chat sessions.
 * It provides a session list and a tabbed area for active chats.
 * 
 * @author gemini-3-flash-preview
 */
@Slf4j
public class StandaloneMainPanel extends JPanel implements AsiContainerPanel.SessionController {

    private final StandaloneAsiContainer asiContainer;
    private final AsiContainerPanel asiContainerPanel;
    private final JTabbedPane tabbedPane;
    
    private final EdtPropertyChangeListener asiListener;

    /**
     * Constructs a new MainPanel.
     * 
     * @param container The standalone ASI container.
     */
    public StandaloneMainPanel(StandaloneAsiContainer container) {
        this.asiContainer = container;
        
        setLayout(new BorderLayout());

        asiContainerPanel = new AsiContainerPanel(container);
        asiContainerPanel.setController(this);

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

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, asiContainerPanel, tabbedPane);
        splitPane.setDividerLocation(300);
        splitPane.setOneTouchExpandable(true);
        add(splitPane, BorderLayout.CENTER);
        
        this.asiListener = new EdtPropertyChangeListener(this, container, "activeChats", this::handleAsiChange);
    }

    /**
     * Starts the background refresh of the session list and loads persisted sessions.
     */
    public void start() {
        asiContainerPanel.startRefresh();
        
        // Load persisted sessions from disk
        asiContainer.loadSessions();
        
        // Sync existing chats - use a copy to avoid ConcurrentModificationException
        for (Chat chat : new ArrayList<>(asiContainer.getActiveChats())) {
            focus(chat);
        }
    }

    @Override
    public void focus(@NonNull Chat chat) {
        String id = chat.getConfig().getSessionId();
        log.info("Focusing session: {}", id);
        
        // Check if we already have a tab for this chat
        int tabIndex = -1;
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            Component comp = tabbedPane.getTabCount() > i ? tabbedPane.getComponentAt(i) : null;
            if (comp != null && id.equals(comp.getName())) {
                tabIndex = i;
                break;
            }
        }

        if (tabIndex == -1) {
            log.info("Creating new tab for session: {}", id);
            // Use the existing chat instance instead of creating a new one
            ChatPanel panel = new ChatPanel(chat);
            panel.setName(id);
            panel.initComponents();
            
            tabbedPane.addTab(chat.getNickname(), panel);
            tabIndex = tabbedPane.getTabCount() - 1;
            
            // Listen for nickname changes to update the tab title
            new EdtPropertyChangeListener(this, chat, "nickname", this::handleNicknameChange);
        }

        tabbedPane.setSelectedIndex(tabIndex);
    }

    @Override
    public void close(@NonNull Chat chat) {
        String id = chat.getConfig().getSessionId();
        log.info("Closing tab for session: {}", id);
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            if (id.equals(tabbedPane.getComponentAt(i).getName())) {
                tabbedPane.removeTabAt(i);
                // EdtPropertyChangeListener will be GC'd as it's not strongly held by the chat
                break;
            }
        }
    }

    @Override
    public void dispose(@NonNull Chat chat) {
        String sessionId = chat.getConfig().getSessionId();
        log.info("Disposing session: {}", sessionId);
        close(chat);
        chat.shutdown();
        
        // Move the session file to the disposed directory
        Path sessionFile = asiContainer.getSessionsDir().resolve(sessionId + ".kryo");
        if (Files.exists(sessionFile)) {
            try {
                Path disposedFile = asiContainer.getDisposedSessionsDir().resolve(sessionId + ".kryo");
                Files.move(sessionFile, disposedFile);
                log.info("Moved session file to disposed directory: {}", disposedFile);
            } catch (Exception e) {
                log.error("Failed to move session file to disposed directory", e);
            }
        }
    }

    @Override
    public void createNew() {
        log.info("Creating new session...");
        // Chat constructor registers itself in AsiContainer, which triggers property change
        new Chat(new StandaloneChatConfig(asiContainer));
    }

    private void handleNicknameChange(PropertyChangeEvent evt) {
        Chat chat = (Chat) evt.getSource();
        String id = chat.getConfig().getSessionId();
        String newNickname = (String) evt.getNewValue();
        
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
            if (id.equals(tabbedPane.getComponentAt(i).getName())) {
                tabbedPane.setTitleAt(i, newNickname);
                break;
            }
        }
    }

    private void handleAsiChange(PropertyChangeEvent evt) {
        List<Chat> oldList = (List<Chat>) evt.getOldValue();
        List<Chat> newList = (List<Chat>) evt.getNewValue();
        
        // Handle additions
        if (newList != null) {
            for (Chat chat : new ArrayList<>(newList)) {
                if (oldList == null || !oldList.contains(chat)) {
                    focus(chat);
                }
            }
        }
        
        // Handle removals
        if (oldList != null) {
            for (Chat chat : new ArrayList<>(oldList)) {
                if (newList == null || !newList.contains(chat)) {
                    close(chat);
                }
            }
        }
    }
}
