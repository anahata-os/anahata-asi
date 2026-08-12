/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide.context;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.BasicContextProvider;
import uno.anahata.asi.agi.message.RagMessage;

/**
 * Injects a live snapshot of the IntelliJ windowing system — every registered tool
 * window per open project, flagged with its visible/active state — into the RAG
 * message.
 * <p>
 * This is the IntelliJ analogue of the NetBeans {@code OpenTopComponentsContextProvider}.
 * Tool-window state is read on the EDT because {@link ToolWindow#isVisible()} and
 * {@link ToolWindow#isActive()} are UI-model queries, while context is assembled on a
 * background AI thread.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class OpenToolWindowsContextProvider extends BasicContextProvider {

    /**
     * Constructs the tool-windows context provider with a stable id and label.
     */
    public OpenToolWindowsContextProvider() {
        super("intellij-open-toolwindows", "Tool Windows", "Live snapshot of open IDE tool windows");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Renders a Markdown table (Tool Window | Project | Visible | Active) covering all
     * registered tool windows across every open project.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        StringBuilder sb = new StringBuilder();
        ApplicationManager.getApplication().invokeAndWait(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                ToolWindowManager manager = ToolWindowManager.getInstance(project);
                for (String id : manager.getToolWindowIds()) {
                    ToolWindow toolWindow = manager.getToolWindow(id);
                    if (toolWindow == null) {
                        continue;
                    }
                    sb.append("| ").append(id).append(" | ").append(project.getName()).append(" | ")
                      .append(toolWindow.isVisible() ? "Y" : "N").append(" | ")
                      .append(toolWindow.isActive() ? "Y" : "N").append(" |\n");
                }
            }
        });

        if (sb.length() == 0) {
            return;
        }
        StringBuilder out = new StringBuilder();
        out.append("## Open IDE Tool Windows\n");
        out.append("| Tool Window | Project | Visible | Active |\n");
        out.append("|---|---|---|---|\n");
        out.append(sb);
        ragMessage.addTextPart(out.toString());
    }
}
