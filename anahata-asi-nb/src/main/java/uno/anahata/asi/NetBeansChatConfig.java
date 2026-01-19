/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi;

import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.nb.mine.NetBeansEditorKitProvider;
import uno.anahata.asi.nb.tools.java.CodeModel;
import uno.anahata.asi.nb.tools.maven.Maven;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.swing.chat.SwingChatConfig;

/**
 *
 * @author pablo
 */
public class NetBeansChatConfig extends SwingChatConfig{

    public NetBeansChatConfig(AsiContainer asiConfig) {
        super(asiConfig);
    }

    public NetBeansChatConfig(AsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
        getToolClasses().add(Maven.class);
        getToolClasses().add(Projects.class);
        getToolClasses().add(CodeModel.class);
        setEditorKitProvider(new NetBeansEditorKitProvider());
        getProviderClasses().add(GeminiAiProvider.class);
    }
    
    
}
