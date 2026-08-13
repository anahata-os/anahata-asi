/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij;

import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.intellij.tools.ide.Editor;
import uno.anahata.asi.intellij.tools.ide.IDE;
import uno.anahata.asi.intellij.tools.ide.Refactor;
import uno.anahata.asi.intellij.tools.java.BatchCodeRefiner;
import uno.anahata.asi.intellij.tools.java.CodeModel;
import uno.anahata.asi.intellij.tools.java.CodeRefiner;
import uno.anahata.asi.intellij.tools.java.Hints;
import uno.anahata.asi.intellij.tools.java.IntellijJava;
import uno.anahata.asi.intellij.tools.maven.Maven;
import uno.anahata.asi.intellij.tools.project.Projects;
import uno.anahata.asi.intellij.tools.run.RunConfigurations;
import uno.anahata.asi.intellij.tools.terminal.Terminals;
import uno.anahata.asi.intellij.tools.vcs.Vcs;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.toolkit.SwingJava;

/**
 * IntelliJ-specific AGI configuration.
 * <p>
 * Customizes the default model, provider, and tool availability for the
 * IntelliJ IDEA platform environment. Mirrors the NetBeans
 * {@code NetBeansAgiConfig} registration pattern: IDE-native toolkits are
 * contributed purely by adding their {@link Class} to {@code getToolClasses()};
 * the {@code ToolManager} then reflectively registers each as a
 * {@code JavaObjectToolkit}, and because every Anahata toolkit is also a
 * {@code ContextProvider}, their context-provider subtrees are wired up
 * automatically with no further plumbing.
 * </p>
 *
 * @author anahata
 */
public class IntellijAgiConfig extends SwingAgiConfig {

    /**
     * Default initialization block to set IntelliJ-specific settings.
     */
    {
        setSelectedProviderUuid("Gemini");
        setSelectedModelId("models/gemini-flash-latest");
    }

    /**
     * Default initialization block registering the IntelliJ-native toolkits and
     * selecting the default provider/model.
     * <p>
     * Registration order matches the NetBeans reference implementation. Further
     * IntelliJ toolkits (Editor, IDE, CodeRefiner, Refactor, Maven, Terminal)
     * are appended here as they are implemented.
     * </p>
     */
    {

        // Replace the Swing Java toolkit with the IntelliJ project-aware one (mirrors NbJava).
        getToolClasses().remove(SwingJava.class);
        getToolClasses().add(IntellijJava.class);

        getToolClasses().add(Projects.class);
        getToolClasses().add(Maven.class);
        getToolClasses().add(CodeModel.class);
        getToolClasses().add(Editor.class);
        getToolClasses().add(IDE.class);
        getToolClasses().add(RunConfigurations.class);
        getToolClasses().add(Vcs.class);
        getToolClasses().add(CodeRefiner.class);
        getToolClasses().add(BatchCodeRefiner.class);
        getToolClasses().add(Hints.class);
        getToolClasses().add(Refactor.class);
        getToolClasses().add(Terminals.class);

    }

    /**
     * Constructs a new IntelliJ AGI configuration.
     *
     * @param container The host ASI container.
     */
    public IntellijAgiConfig(AbstractAsiContainer container) {
        super(container);
    }

    /**
     * Constructs a new IntelliJ AGI configuration with a specific session ID.
     *
     * @param container The host ASI container.
     * @param sessionId The unique session ID.
     */
    public IntellijAgiConfig(AbstractAsiContainer container, String sessionId) {
        super(container, sessionId);
    }
}
