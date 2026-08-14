/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide;

import com.intellij.ide.projectView.ProjectView;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.agi.resource.view.TextViewportSettings;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.intellij.tools.ide.context.OpenToolWindowsContextProvider;

import java.io.File;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

/**
 * Provides tools for interacting with the IntelliJ IDEA IDE itself: tailing the IDE
 * log, revealing files in the Project view, and reporting the state of the tool-window
 * system.
 * <p>
 * This is the IntelliJ port of the NetBeans {@code IDE} toolkit. Log tailing reuses the
 * shared core resource pipeline ({@link PathHandle} + {@link TextView} +
 * {@link TextViewportSettings}) unchanged. The NetBeans {@code TopComponent}/Output-window
 * crawl is replaced by the platform {@link ToolWindowManager}, exposed both as a tool and
 * as the child {@link OpenToolWindowsContextProvider}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for interacting with the IntelliJ IDEA IDE.")
public class IDE extends AnahataToolkit {

    /**
     * Constructs a new IDE toolkit and registers its child context providers.
     * <p>
     * Registers the {@link OpenToolWindowsContextProvider} so the ASI receives a live
     * snapshot of the IDE's windowing system on every turn.
     * </p>
     */
    public IDE() {
        OpenToolWindowsContextProvider toolWindows = new OpenToolWindowsContextProvider();
        toolWindows.setParentProvider(this);
        childrenProviders.add(toolWindows);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Explains, host-specifically, that in-context resources reflect on-disk state
     * (not unsaved editor buffers) and points the model at the Editor/ToolWindows
     * providers for live editor state.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
                "Your host application is IntelliJ IDEA. "
                + "LIVE resources in the RAG message are refreshed from disk each turn and reflect the files on disk, not unsaved editor buffers. "
                + "Use the 'Open Editor Files' provider (Editor toolkit) to see which files are open and whether they have unsaved changes. "
                + "The CodeRefiner and CodeModel toolkits operate on the live PSI tree and can persist changes to disk.");
    }

    /**
     * Tails the IDE log ({@code idea.log}) into the session context, with optional
     * grep filtering, by registering it as a tailed text resource.
     *
     * @param grepPattern optional regex to filter log lines (e.g. {@code ERROR}), or {@code null}.
     * @param tailLines   number of trailing lines to include (defaults to 100).
     * @throws AgiToolException if the log file cannot be located.
     */
    @AgiTool("Monitors the IDE log (idea.log) by loading it into the context with 'tail' enabled and optional grepping.")
    public void monitorLogs(
            @AgiToolParam(value = "Optional regex pattern to filter log lines (e.g. 'ERROR' or a logger name).", required = false) String grepPattern,
            @AgiToolParam(value = "Number of lines to tail from the end of the file or matching results.", required = false) Integer tailLines) throws AgiToolException {

        File logFile = new File(PathManager.getLogPath(), "idea.log");
        if (!logFile.exists()) {
            throw new AgiToolException("IDE log file not found at: " + logFile.getAbsolutePath());
        }

        TextViewportSettings settings = new TextViewportSettings();
        settings.setTail(true);
        settings.setTailLines(tailLines != null ? tailLines : 100);
        settings.setGrepPattern(grepPattern);
        settings.setIncludeLineNumbers(false);

        PathHandle handle = new PathHandle(logFile.getAbsolutePath());
        Resource resource = new Resource(handle);
        resource.setView(new TextView(resource, settings));
        getAgi().getResourceManager().register(resource, "IDE Logs (Tailed)");
        log("Registered idea.log as a tailed resource (" + settings.getTailLines() + " lines"
                + (grepPattern != null ? ", grep='" + grepPattern + "'" : "") + ").");
    }

    /**
     * Reveals and selects a file or folder in the Project tool window.
     * <p>
     * Maps the NetBeans "Select in Projects" behaviour. Runs on the EDT because it
     * drives the Project view UI.
     * </p>
     *
     * @param path the absolute path of the file or folder to reveal.
     * @return a confirmation message.
     * @throws AgiToolException if the path cannot be resolved or hosted by an open project.
     */
    @AgiTool("Reveals and selects the specified file or folder in the IDE Project view.")
    public String selectIn(
            @AgiToolParam("The absolute path of the file or folder to reveal.") String path) throws AgiToolException {
        VirtualFile vf = VfsUtil.findFile(Path.of(path), true);
        if (vf == null) {
            throw new AgiToolException("Target not found: " + path);
        }
        Project project = findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host: " + path);
        }
        ApplicationManager.getApplication().invokeAndWait(() ->
                ProjectView.getInstance(project).select(null, vf, true));
        return "Selected " + path + " in the Project view.";
    }

    /**
     * Produces a Markdown table of every registered tool window across all open
     * projects, with visibility/active state.
     *
     * @return a Markdown table, or a message when nothing is open.
     */
    @AgiTool("Gets a Markdown table of all open IDE tool windows.")
    public String getToolWindowsMarkdown() {
        StringBuilder rows = new StringBuilder();
        ApplicationManager.getApplication().invokeAndWait(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                ToolWindowManager manager = ToolWindowManager.getInstance(project);
                for (String id : manager.getToolWindowIds()) {
                    ToolWindow toolWindow = manager.getToolWindow(id);
                    if (toolWindow == null) {
                        continue;
                    }
                    rows.append("| ").append(id).append(" | ").append(project.getName()).append(" | ")
                        .append(toolWindow.isVisible() ? "Y" : "N").append(" | ")
                        .append(toolWindow.isActive() ? "Y" : "N").append(" |\n");
                }
            }
        });
        if (rows.length() == 0) {
            return "No open projects / tool windows.";
        }
        return "| Tool Window | Project | Visible | Active |\n|---|---|---|---|\n" + rows;
    }

    /**
     * Resolves the open project whose content roots contain the given file, falling
     * back to the first open project.
     *
     * @param file the file to host.
     * @return a hosting project, or {@code null} if no projects are open.
     */
    private Project findHostProject(VirtualFile file) {
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        for (Project project : open) {
            if (ProjectRootManager.getInstance(project).getFileIndex().isInContent(file)) {
                return project;
            }
        }
        return open.length > 0 ? open[0] : null;
    }
}
