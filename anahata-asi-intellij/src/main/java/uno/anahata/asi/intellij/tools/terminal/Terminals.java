/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.terminal;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.Disposer;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.plugins.terminal.ShellTerminalWidget;
import org.jetbrains.plugins.terminal.TerminalToolWindowManager;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A toolkit for opening, driving and closing IntelliJ terminal tabs.
 * <p>
 * This is the IntelliJ port of the NetBeans {@code NbTerminal}. Where NetBeans crawls the
 * dlight terminal container and reflects into a private {@code Term.fireChars}, IntelliJ
 * offers first-class APIs: {@link TerminalToolWindowManager#createLocalShellWidget} to open a
 * shell and {@link ShellTerminalWidget#executeCommand} to run a command. Opened widgets are
 * tracked by identity id in a transient map so subsequent calls can address them; the map is
 * rebuilt on session activation (terminals do not survive a restart).
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for opening, driving and closing IntelliJ terminal tabs.")
public class Terminals extends AnahataToolkit {

    /**
     * Identity-keyed registry of terminals opened by this toolkit. Transient: terminals are
     * live UI resources that do not survive serialization.
     */
    private transient Map<Long, ShellTerminalWidget> widgets = new ConcurrentHashMap<>();

    /**
     * Constructs the Terminals toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Terminals() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Re-initializes the transient terminal registry after deserialization.
     * </p>
     */
    @Override
    public void rebind() {
        super.rebind();
        widgets = new ConcurrentHashMap<>();
    }

    /**
     * Opens a new local terminal tab and returns its tracking id.
     *
     * @param title            the tab title, or {@code null} for the default.
     * @param workingDirectory the initial working directory, or {@code null} for the default.
     * @return the identity id used to address this terminal in later calls.
     * @throws AgiToolException if no project is open.
     */
    @AgiTool("Opens a new local terminal tab and returns its id for use with typeCommand/closeTerminal.")
    public long openLocalTerminal(
            @AgiToolParam(value = "The tab title, or null for the default.", required = false) String title,
            @AgiToolParam(value = "The initial working directory, or null for the default.", required = false) String workingDirectory) throws AgiToolException {

        Project project = firstOpenProject();
        ShellTerminalWidget[] holder = new ShellTerminalWidget[1];
        ApplicationManager.getApplication().invokeAndWait(() ->
                holder[0] = TerminalToolWindowManager.getInstance(project).createLocalShellWidget(workingDirectory, title));

        long id = System.identityHashCode(holder[0]);
        widgets.put(id, holder[0]);
        log("Opened terminal '" + (title != null ? title : "Local") + "' with id " + id);
        return id;
    }

    /**
     * Types a command into a tracked terminal and executes it.
     *
     * @param terminalId the id returned by {@link #openLocalTerminal}.
     * @param command    the command line to execute.
     * @return a confirmation message.
     * @throws AgiToolException if the terminal id is unknown or the command cannot be written.
     */
    @AgiTool("Types a command into a terminal tab (by id) and executes it.")
    public String typeCommand(
            @AgiToolParam("The id of the terminal (from openLocalTerminal).") long terminalId,
            @AgiToolParam("The command line to execute.") String command) throws AgiToolException {

        ShellTerminalWidget widget = widgets.get(terminalId);
        if (widget == null) {
            throw new AgiToolException("No terminal tracked with id: " + terminalId);
        }
        IOException[] failure = new IOException[1];
        ApplicationManager.getApplication().invokeAndWait(() -> {
            try {
                widget.executeCommand(command);
            } catch (IOException e) {
                failure[0] = e;
            }
        });
        if (failure[0] != null) {
            throw new AgiToolException("Failed to execute command in terminal " + terminalId + ": " + failure[0].getMessage());
        }
        return "Executed in terminal " + terminalId + ": " + command;
    }

    /**
     * Closes a tracked terminal tab.
     *
     * @param terminalId the id returned by {@link #openLocalTerminal}.
     * @return a confirmation message.
     * @throws AgiToolException if the terminal id is unknown.
     */
    @AgiTool("Closes a terminal tab by id.")
    public String closeTerminal(
            @AgiToolParam("The id of the terminal to close.") long terminalId) throws AgiToolException {

        ShellTerminalWidget widget = widgets.remove(terminalId);
        if (widget == null) {
            throw new AgiToolException("No terminal tracked with id: " + terminalId);
        }
        ApplicationManager.getApplication().invokeAndWait(() -> Disposer.dispose(widget));
        return "Closed terminal " + terminalId;
    }

    /**
     * Returns the first open project, which hosts newly created terminals.
     *
     * @return the first open project.
     * @throws AgiToolException if no project is open.
     */
    private Project firstOpenProject() throws AgiToolException {
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        if (open.length == 0) {
            throw new AgiToolException("No open project to host a terminal.");
        }
        return open[0];
    }
}
