/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.awt.BorderLayout;
import java.awt.Color;
import java.beans.PropertyChangeEvent;
import java.io.Serializable;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.status.ChatStatus;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.SwingChatConfig;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * The main TopComponent for Anahata ASI V2.
 * It manages a single chat session and its corresponding UI panel.
 * <p>
 * This component uses the {@code writeReplace} pattern for robust persistence,
 * ensuring that only the session ID is saved and the component is correctly
 * reconstructed after an IDE restart or a module reload.
 * </p>
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
public final class AgiTopComponent extends TopComponent {

    /** The UI panel for the chat session. */
    private transient ChatPanel chatPanel;
    
    /** The unique ID of the session managed by this component. */
    private String sessionId;

    /**
     * Default constructor required for NetBeans persistence mechanism.
     * It creates a component without a session ID, which will be populated
     * during restoration or initialization.
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
        updateTitles();
    }

    /**
     * Initializes the chat panel and adds it to the component.
     * 
     * @param chat The chat session.
     */
    private void initPanel(Chat chat) {
        if (chatPanel != null) return;
        this.sessionId = chat.getConfig().getSessionId();
        chatPanel = new ChatPanel(chat);
        chatPanel.initComponents();
        setLayout(new BorderLayout());
        add(chatPanel, BorderLayout.CENTER);
        
        updateTitles();
        
        // Listen for nickname changes to update the TopComponent title
        new EdtPropertyChangeListener(this, chat, "nickname", this::handleNicknameChange);
        
        // Listen for status changes to update the tab color
        new EdtPropertyChangeListener(this, chat.getStatusManager(), "currentStatus", evt -> updateTitles());
    }

    /**
     * Updates the TopComponent's name, display name, and tooltip based on the current chat session.
     */
    private void updateTitles() {
        String displayName = "Anahata AGI";
        ChatStatus status = ChatStatus.IDLE;
        
        Chat chat = getChat();
        if (chat == null && sessionId != null) {
            // Try to find the chat in the container even if the panel isn't ready
            chat = AnahataInstaller.getContainer().getActiveChats().stream()
                    .filter(c -> c.getConfig().getSessionId().equals(sessionId))
                    .findFirst().orElse(null);
        }
        
        if (chat != null) {
            displayName = chat.getDisplayName();
            status = chat.getStatusManager().getCurrentStatus();
        }
        
        Color color = SwingChatConfig.getColor(status);
        String hexColor = SwingUtils.toHtmlColor(color);
        
        setName(displayName);
        setDisplayName(displayName);
        setHtmlDisplayName("<html><font color='" + hexColor + "'>" + displayName + "</font></html>");
        
        String tooltip = sessionId != null ? "Anahata Session: " + sessionId + " [" + status.getDisplayName() + "]" : "Anahata AGI";
        setToolTipText(tooltip);
    }

    /**
     * Handles nickname changes by updating the TopComponent titles.
     * 
     * @param evt The property change event.
     */
    private void handleNicknameChange(PropertyChangeEvent evt) {
        updateTitles();
    }

    /**
     * {@inheritDoc}
     * Ensures the chat panel is initialized when the component is opened.
     */
    @Override
    public void componentOpened() {
        if (chatPanel == null) {
            NetBeansAsiContainer container = (NetBeansAsiContainer) AnahataInstaller.getContainer();
            Chat chat = container.findOrCreateChat(sessionId);
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
     * Returns a stable, session-specific ID for the window system.
     * 
     * @return The preferred ID.
     */
    @Override
    protected String preferredID() {
        return "agi_" + (sessionId != null ? sessionId : "new");
    }

    /**
     * The standard NetBeans persistence pattern: replace the component with a 
     * serializable proxy that only holds the essential state (the session ID).
     * 
     * @return A serializable Resolvable object.
     */
    @Override
    protected Object writeReplace() throws java.io.ObjectStreamException {
        return new Resolvable(sessionId);
    }

    /**
     * A static inner class used for serializing the state of an AgiTopComponent.
     */
    private static final class Resolvable implements Serializable {
        private static final long serialVersionUID = 1L;
        private final String sessionId;

        Resolvable(String sessionId) {
            this.sessionId = sessionId;
        }

        /**
         * Reconstructs the AgiTopComponent using the saved session ID.
         * 
         * @return A new AgiTopComponent instance.
         */
        Object readResolve() throws java.io.ObjectStreamException {
            return new AgiTopComponent(sessionId);
        }
    }
}
