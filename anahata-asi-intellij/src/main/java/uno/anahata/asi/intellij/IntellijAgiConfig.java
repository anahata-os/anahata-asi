/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij;

import com.intellij.icons.AllIcons;
import com.intellij.ui.JBColor;
import java.awt.Dimension;
import java.awt.Insets;
import java.net.URI;
import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import lombok.NonNull;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import uno.anahata.asi.intellij.resources.handle.IntellijHandle;
import uno.anahata.asi.intellij.ui.IntellijIconProvider;
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
import uno.anahata.asi.intellij.tools.gradle.Gradle;
import uno.anahata.asi.intellij.tools.debugger.Debugger;
import uno.anahata.asi.intellij.tools.run.RunConfigurations;
import uno.anahata.asi.intellij.tools.terminal.Terminals;
import uno.anahata.asi.intellij.tools.vcs.Vcs;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.icons.ActionIconKey;
import uno.anahata.asi.swing.toolkit.DesktopJava;

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
        getToolClasses().remove(DesktopJava.class);
        getToolClasses().add(IntellijJava.class);

        getToolClasses().add(Projects.class);
        getToolClasses().add(Maven.class);
        getToolClasses().add(Gradle.class);
        getToolClasses().add(CodeModel.class);
        getToolClasses().add(Editor.class);
        getToolClasses().add(IDE.class);
        getToolClasses().add(RunConfigurations.class);
        getToolClasses().add(Debugger.class);
        getToolClasses().add(Vcs.class);
        getToolClasses().add(CodeRefiner.class);
        getToolClasses().add(BatchCodeRefiner.class);
        getToolClasses().add(Hints.class);
        getToolClasses().add(Refactor.class);
        getToolClasses().add(Terminals.class);

        setIconProvider(new IntellijIconProvider());
    }

    /** 
     * {@inheritDoc} 
     * <p>
     * Overrides the factory to return the reactive IntellijHandle for local or JAR resources.
     * </p>
     */
    @Override
    public ResourceHandle createResourceHandle(URI uri) {
        if ("file".equalsIgnoreCase(uri.getScheme()) || "jar".equalsIgnoreCase(uri.getScheme())) {
            return new IntellijHandle(uri);
        }
        return super.createResourceHandle(uri);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Maps semantic action keys to native, theme-adaptive IntelliJ {@link AllIcons}.
     * Falls back to built-in Anahata vector icons for any keys without a platform equivalent.
     * </p>
     */
    @Override
    public Icon getActionIcon(@NonNull ActionIconKey key, int size) {
        Icon icon = switch (key) {
            case CANCEL -> AllIcons.Actions.Cancel;
            case DELETE -> AllIcons.General.Delete;
            case SAVE -> AllIcons.Actions.MenuSaveall;
            case EDIT, EDIT_STAGED -> AllIcons.Actions.Edit;
            case COPY -> AllIcons.Actions.Copy;
            case SEND, RUN_AND_SEND -> AllIcons.Actions.Execute;
            case STOP -> AllIcons.Actions.Suspend;
            case ATTACH -> AllIcons.Actions.Attach;
            case LINK -> AllIcons.Ide.Link;
            case SCREENSHOT -> AllIcons.Actions.Dump;
            case SEARCH -> AllIcons.Actions.Search;
            case OPEN_SESSION -> AllIcons.General.OpenInToolWindow;
            case REFRESH -> AllIcons.Actions.Refresh;
            case CLEAR_HISTORY -> AllIcons.Actions.GC;
            case NEW_SESSION -> AllIcons.General.Add;
            case IMPORT -> AllIcons.ToolbarDecorator.Import;
            case SETTINGS -> AllIcons.General.Settings;
            case EXTERNAL -> AllIcons.Ide.External_link_arrow;
            case OPEN_IN_IDE, NEXT -> AllIcons.Actions.Forward;
            case PIN -> AllIcons.General.Pin_tab;
            case LOCAL_TOOLS -> AllIcons.Nodes.Toolbox;
            case SERVER_TOOLS -> AllIcons.Nodes.Plugin;
            case AUTO_REPLY -> AllIcons.Actions.Rerun;
            case TEST_CONNECTION -> AllIcons.Actions.Lightning;
            default -> null;
        };
        return icon != null ? icon : super.getActionIcon(key, size);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Queries IntelliJ's {@link JBColor#isBright()} to determine whether a dark theme is active.
     * </p>
     */
    @Override
    public boolean isDark() {
        return !JBColor.isBright();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Customizes toolbar buttons for IntelliJ: applies the {@code "toolBarButton"} FlatLaf client
     * property and constrains icon-only buttons to square dimensions so IntelliJ's New UI does not
     * render them overly wide.
     * </p>
     */
    @Override
    public JButton createSquareButton(@NonNull ActionIconKey key, int size, String tooltip) {
        JButton button = super.createSquareButton(key, size, tooltip);
        forceSquare(button, size);
        return button;
    }
    
    /**
     * Creates and styles an action toggle button for the current host environment.
     *
     * @param key The semantic action icon key.
     * @param size The nominal icon size.
     * @param tooltip Optional tooltip text.
     * @param selected Initial selection state.
     * @return The configured JToggleButton.
     */
    @Override
    public JToggleButton createSquareToggleButton(@NonNull ActionIconKey key, int size, String tooltip, boolean selected) {
        JToggleButton button = super.createSquareToggleButton(key, size, tooltip, selected);
        forceSquare(button, size);
        return button;
    }
    
    /**
     * Forces a button to be square in intellij.
     * 
     * @param button the fricking button
     * @param size the size
     */
    @Override
    public void forceSquare(AbstractButton button, int size) {
       button.putClientProperty("JButton.buttonType", "toolBarButton");
       button.setMargin(new Insets(2, 2, 2, 2));
       button.setFocusPainted(false);
       int side = size + 10;
       Dimension square = new Dimension(side, side);
       button.setPreferredSize(square);
       button.setMinimumSize(square);
       button.setMaximumSize(square);
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
