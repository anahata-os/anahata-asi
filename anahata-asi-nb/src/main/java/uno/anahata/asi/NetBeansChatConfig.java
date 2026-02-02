/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.gemini.GeminiAiProvider;
import uno.anahata.asi.nb.mine.NetBeansEditorKitProvider;
import uno.anahata.asi.nb.mine.NetBeansIconProvider;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.nb.tools.ide.IDE;
import uno.anahata.asi.nb.tools.java.CodeModel;
import uno.anahata.asi.nb.tools.java.NbJava;
import uno.anahata.asi.nb.tools.maven.Maven;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.swing.chat.SwingChatConfig;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.toolkit.files.Files;

/**
 * NetBeans-specific chat configuration.
 * It replaces the core {@link Files} toolkit with the IDE-integrated {@link NbFiles}
 * and adds NetBeans-specific toolkits like {@link Maven}, {@link Projects}, and {@link CodeModel}.
 * <p>
 * It also configures the {@link NetBeansIconProvider} to display authentic IDE icons 
 * in the context hierarchy.
 * </p>
 * 
 * @author anahata
 */
public class NetBeansChatConfig extends SwingChatConfig {

    {
        // Replace core Files with NbFiles
        getToolClasses().remove(Files.class);
        getToolClasses().add(NbFiles.class);
        
        // Replace core Java with NbJava
        getToolClasses().remove(Java.class);
        getToolClasses().add(NbJava.class);
        
        getToolClasses().add(Maven.class);
        getToolClasses().add(Projects.class);
        getToolClasses().add(CodeModel.class);
        getToolClasses().add(IDE.class);
        
        setEditorKitProvider(new NetBeansEditorKitProvider());
        setIconProvider(new NetBeansIconProvider());
        getProviderClasses().add(GeminiAiProvider.class);
    }
    
    /**
     * Constructs a new NetBeansChatConfig.
     * @param asiConfig The global AI configuration.
     */
    public NetBeansChatConfig(AsiContainer asiConfig) {
        super(asiConfig);
    }

    /**
     * Constructs a new NetBeansChatConfig with a specific session ID.
     * @param asiConfig The global AI configuration.
     * @param sessionId The unique session ID.
     */
    public NetBeansChatConfig(AsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
    }
}
