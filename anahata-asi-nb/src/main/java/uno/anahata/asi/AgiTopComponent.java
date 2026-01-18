/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import java.awt.BorderLayout;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.UUID;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.nb.mine.NetBeansEditorKitProvider;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.SwingChatConfig;

/**
 * The main TopComponent for Anahata ASI V2.
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

    private transient ChatPanel chatPanel;
    private transient boolean isInitialized = false;
    private String sessionId;

    public AgiTopComponent() {
        setName("AGI");
        setDisplayName("Anahata AGI");
    }

    @Override
    public void componentOpened() {
        if (!isInitialized) {
            if (sessionId == null) {
                sessionId = UUID.randomUUID().toString();
            }
            
            // V2 Initialization logic using shared AsiConfig
            AsiConfig asiConfig = AnahataInstaller.getAsiConfig();
            SwingChatConfig chatConfig = new SwingChatConfig(asiConfig, sessionId);
            chatConfig.setEditorKitProvider(new NetBeansEditorKitProvider());
            chatConfig.getProviderClasses().add(GeminiAiProvider.class);
            
            Chat chat = new Chat(chatConfig);
            chatPanel = new ChatPanel(chat);
            chatPanel.initComponents();
            
            setLayout(new BorderLayout());
            add(chatPanel, BorderLayout.CENTER);
            
            isInitialized = true;
        }
    }

    public Chat getChat() {
        return chatPanel != null ? chatPanel.getChat() : null;
    }

    public void setSessionIdForHandoff(String sessionId) {
        this.sessionId = sessionId;
    }

    @Override
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(sessionId);
        super.writeExternal(out);
    }

    @Override
    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
        sessionId = (String) in.readObject();
        super.readExternal(in);
    }
}
