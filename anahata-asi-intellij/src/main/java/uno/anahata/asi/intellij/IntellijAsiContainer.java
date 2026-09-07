/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import java.io.IOException;
import javax.swing.JFrame;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.intellij.ui.IntellijTextResourceWriteRenderer;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRendererFactory;
import uno.anahata.asi.toolkit.resources.text.FullTextFileCreate;
import uno.anahata.asi.toolkit.resources.text.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.resources.text.TextResourceReplacements;
import uno.anahata.asi.toolkit.resources.text.lines.TextResourceLineEdits;

/**
 * Concrete implementation of the ASI Container for IntelliJ IDEA.
 * <p>
 * This container integrates the Anahata framework with the IntelliJ IDEA platform as an
 * application-level singleton service, managing sessions, AI providers, and multi-window
 * tool window tabs.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class IntellijAsiContainer extends AbstractSwingAsiContainer {

    /**
     * Registers the IntelliJ diff visualization for the core text-write tool arguments and the IntelliJ ResourceUI.
     */
    static {
        initEnvironment();
    }

    /**
     * Bootstraps the IntelliJ global environment configuration, registering parameter
     * renderers, JSON serialization modules, and the IntelliJ native {@link uno.anahata.asi.swing.agi.resources.ResourceUI} strategy.
     */
    public static void initEnvironment() {
        ParameterRendererFactory.register(FullTextResourceUpdate.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(FullTextFileCreate.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(TextResourceReplacements.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(TextResourceLineEdits.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementBatch.class, IntellijTextResourceWriteRenderer.class);
        uno.anahata.asi.swing.agi.resources.ResourceUiRegistry.getInstance().setResourceUI(new uno.anahata.asi.intellij.ui.resources.IntellijResourceUI());
    }

    /**
     * Default constructor initializing with the "intellij" host application ID as an
     * application-level singleton service, and loading all active sessions from disk.
     *
     * @throws IOException if container directory initialization or reading from disk fails.
     */
    public IntellijAsiContainer() throws IOException {
        super("intellij");
        int loaded = loadSessions();
        log.info("IntellijAsiContainer initialized as application service; loaded {} active sessions from disk.", loaded);
    }

    /**
     * Retrieves the application-level singleton instance of {@link IntellijAsiContainer}.
     *
     * @return the singleton container instance.
     */
    public static IntellijAsiContainer getInstance() {
        return ApplicationManager.getApplication().getService(IntellijAsiContainer.class);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns an IntelliJ-specific AGI configuration.
     * </p>
     */
    @Override
    public AgiConfig createNewAgiConfig() {
        return new IntellijAgiConfig(this);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns {@code "anahata-asi-intellij"} to allow resolving {@code pom.properties}
     * in development mode when running directly off {@code target/classes}.
     * </p>
     *
     * @return {@code "anahata-asi-intellij"}.
     */
    @Override
    public String getMavenArtifactId() {
        return "anahata-asi-intellij";
    }

    /**
     * {@inheritDoc}
     * <p>
     * Focuses or creates a dedicated tab for the given AGI session. If the session is already
     * open in any open project's tool window, that window and tab are brought to the front.
     * Otherwise, a new tab is opened in the currently active or first open project window.
     * </p>
     *
     * @param agi the AGI session to focus or open.
     */
    @Override
    protected void focusUI(Agi agi) {
        // Option 1: Focus existing window if tab is already open in any project's tool window
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            ToolWindow tw = ToolWindowManager.getInstance(project).getToolWindow("Anahata ASI");
            if (tw != null) {
                for (Content c : tw.getContentManager().getContents()) {
                    if (c.getComponent() instanceof AgiPanel panel && panel.getAgi() == agi) {
                        tw.getContentManager().setSelectedContent(c);
                        tw.show();
                        JFrame frame = WindowManager.getInstance().getFrame(project);
                        if (frame != null) {
                            frame.toFront();
                        }
                        return;
                    }
                }
            }
        }

        // If not already open in any project window, open in the active (or first) project window
        Project targetProject = findActiveOrFirstProject();
        if (targetProject != null) {
            ToolWindow tw = ToolWindowManager.getInstance(targetProject).getToolWindow("Anahata ASI");
            if (tw != null) {
                AgiPanel agiPanel = new AgiPanel(agi);
                agiPanel.initComponents();

                ContentFactory contentFactory = ContentFactory.getInstance();
                Content content = contentFactory.createContent(agiPanel, agi.getDisplayName(), false);
                content.setCloseable(true);

                tw.getContentManager().addContent(content);
                tw.getContentManager().setSelectedContent(content);
                tw.show();
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Closes and removes the dedicated tab for the given AGI session across all open project tool windows.
     * </p>
     *
     * @param agi the AGI session to close.
     */
    @Override
    protected void closeUI(Agi agi) {
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            ToolWindow tw = ToolWindowManager.getInstance(project).getToolWindow("Anahata ASI");
            if (tw != null) {
                for (Content c : tw.getContentManager().getContents()) {
                    if (c.getComponent() instanceof AgiPanel panel && panel.getAgi() == agi) {
                        tw.getContentManager().removeContent(c, true);
                        return;
                    }
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Locates the active AgiPanel component for the given AGI session across open project tool windows.
     * </p>
     *
     * @param agi the AGI session to locate.
     * @return the active AgiPanel, or {@code null} if not open in any tool window.
     */
    @Override
    public Object getUI(Agi agi) {
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            ToolWindow tw = ToolWindowManager.getInstance(project).getToolWindow("Anahata ASI");
            if (tw != null) {
                for (Content c : tw.getContentManager().getContents()) {
                    if (c.getComponent() instanceof AgiPanel panel && panel.getAgi() == agi) {
                        return panel;
                    }
                }
            }
        }
        return null;
    }

    /**
     * Resolves the currently active or focused project window, falling back to the first open project.
     *
     * @return the active or first open Project, or null if no projects are open.
     */
    private Project findActiveOrFirstProject() {
        Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
        if (openProjects.length == 0) {
            return null;
        }
        for (Project p : openProjects) {
            JFrame frame = WindowManager.getInstance().getFrame(p);
            if (frame != null && frame.isActive()) {
                return p;
            }
        }
        return openProjects[0];
    }
}
