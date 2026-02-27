/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi;

import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.gemini.GeminiAgiProvider;
import uno.anahata.asi.nb.mine.NetBeansEditorKitProvider;
import uno.anahata.asi.nb.mine.NetBeansIconProvider;
import uno.anahata.asi.nb.tools.Refactor;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.nb.tools.ide.Editor;
import uno.anahata.asi.nb.tools.ide.IDE;
import uno.anahata.asi.swing.tools.Screens;
import uno.anahata.asi.nb.tools.java.CodeModel;
import uno.anahata.asi.nb.tools.java.Hints;
import uno.anahata.asi.nb.tools.java.NbJava;
import uno.anahata.asi.nb.tools.maven.Maven;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.toolkit.Host;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.toolkit.files.Files;

/**
 * NetBeans-specific agi configuration.
 * It replaces the core {@link Files} toolkit with the IDE-integrated {@link NbFiles}
 * and adds NetBeans-specific toolkits like {@link Maven}, {@link Projects}, and {@link CodeModel}.
 * <p>
 * It also configures the {@link NetBeansIconProvider} to display authentic IDE icons 
 * in the context hierarchy.
 * </p>
 * 
 * @author anahata
 */
public class NetBeansAgiConfig extends SwingAgiConfig {

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
        getToolClasses().add(Editor.class);
        getToolClasses().add(Hints.class);
        getToolClasses().add(Refactor.class);
        getToolClasses().add(Host.class);
        getToolClasses().add(Screens.class);
        
        setEditorKitProvider(new NetBeansEditorKitProvider());
        setIconProvider(new NetBeansIconProvider());
        getProviderClasses().add(GeminiAgiProvider.class);
    }
    
    /**
     * Constructs a new NetBeansAgiConfig.
     * @param asiConfig The global AI configuration.
     */
    public NetBeansAgiConfig(AsiContainer asiConfig) {
        super(asiConfig);
    }

    /**
     * Constructs a new NetBeansAgiConfig with a specific session ID.
     * @param asiConfig The global AI configuration.
     * @param sessionId The unique session ID.
     */
    public NetBeansAgiConfig(AsiContainer asiConfig, String sessionId) {
        super(asiConfig, sessionId);
    }
}
