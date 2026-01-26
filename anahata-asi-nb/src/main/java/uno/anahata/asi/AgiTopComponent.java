/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.awt.BorderLayout;
import java.beans.PropertyChangeEvent;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.UUID;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * The main TopComponent for Anahata ASI V2.
 * It manages a single chat session and its corresponding UI panel.
 * 
 * @author anahata
 */
@ActionID(category = "Window", id = "uno.anahata.asi.OpenAgiTopComponent")
@ActionReference(path = "Menu/Window", position = 334)
@TopComponent.Description(
        preferredID = "agi",
        iconBase = "icons/anahata_16.png",
        persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED)
@TopComponent.Registration(mode = "output", openAtStartup = false, position = 0)
@TopComponent.OpenActionRegistration(displayName = "AGI")
@Slf4j
public final class AgiTopComponent extends TopComponent implements Externalizable {

    /** The UI panel for the chat session. */
    private transient ChatPanel chatPanel;
    
    /** The unique ID of the session managed by this component. */
    private String sessionId;

    /**
     * Default constructor required for deserialization.
     */
    public AgiTopComponent() {
        this((String)null);
    }
    
    /**
     * Constructs a new component for an existing chat session.
     * 
     * @param chat The chat session.
     */
    public AgiTopComponent(Chat chat) {
        this(chat.getConfig().getSessionId());
        initPanel(chat);
    }

    /**
     * Constructs a new component for a specific session ID.
     * 
     * @param sessionId The session ID.
     */
    public AgiTopComponent(String sessionId) {
        this.sessionId = sessionId;
        setIcon(ImageUtilities.loadImage("icons/anahata_16.png"));
        updateTitles(null);
    }

    /**
     * Initializes the chat panel and adds it to the component.
     * 
     * @param chat The chat session.
     */
    private void initPanel(Chat chat) {
        if (chatPanel != null) return;
        chatPanel = new ChatPanel(chat);
        chatPanel.initComponents();
        setLayout(new BorderLayout());
        add(chatPanel, BorderLayout.CENTER);
        
        updateTitles(chat);
        
        // Listen for nickname changes to update the TopComponent title
        new EdtPropertyChangeListener(this, chat, "nickname", this::handleNicknameChange);
    }

    /**
     * Updates the TopComponent's name, display name, and tooltip based on the chat session.
     * 
     * @param chat The chat session, or null if not yet initialized.
     */
    private void updateTitles(Chat chat) {
        String displayName = chat != null ? chat.getDisplayName() : "Anahata AGI";
        setName(displayName);
        setDisplayName(displayName);
        setToolTipText(chat != null ? "Anahata Session: " + chat.getConfig().getSessionId() : "Anahata AGI");
    }

    /**
     * Handles nickname changes by updating the TopComponent titles.
     * 
     * @param evt The property change event.
     */
    private void handleNicknameChange(PropertyChangeEvent evt) {
        updateTitles((Chat) evt.getSource());
    }

    /**
     * {@inheritDoc}
     * Ensures the chat panel is initialized when the component is opened.
     */
    @Override
    public void componentOpened() {
        if (chatPanel == null) {
            if (sessionId == null) {
                sessionId = UUID.randomUUID().toString();
            }
            
            // Find existing chat or create a new one
            Chat chat = AnahataInstaller.getContainer().getActiveChats().stream()
                    .filter(c -> c.getConfig().getSessionId().equals(sessionId))
                    .findFirst()
                    .orElseGet(() -> {
                        NetBeansChatConfig config = new NetBeansChatConfig(AnahataInstaller.getContainer(), sessionId);
                        return new Chat(config);
                    });
            initPanel(chat);
        }
    }

    /**
     * Gets the chat session managed by this component.
     * 
     * @return The chat session, or null if not initialized.
     */
    public Chat getChat() {
        return chatPanel != null ? chatPanel.getChat() : null;
    }

    /**
     * Sets the session ID for handoff during initialization.
     * 
     * @param sessionId The session ID.
     */
    public void setSessionIdForHandoff(String sessionId) {
        this.sessionId = sessionId;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(sessionId);
        super.writeExternal(out);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
        sessionId = (String) in.readObject();
        super.readExternal(in);
    }
}
