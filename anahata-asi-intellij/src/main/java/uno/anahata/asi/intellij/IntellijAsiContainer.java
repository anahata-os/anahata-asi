/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij;

import com.intellij.ide.projectView.ProjectView;
import com.intellij.ide.ui.LafManagerListener;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.JBColor;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.intellij.ui.IntellijJavaCodeParameterRenderer;
import uno.anahata.asi.intellij.ui.IntellijTextResourceWriteRenderer;
import uno.anahata.asi.swing.AbstractSwingAsiContainer;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRendererFactory;
import uno.anahata.asi.toolkit.resources.text.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.resources.text.TextResourceReplacements;
import uno.anahata.asi.toolkit.resources.text.lines.TextResourceLineEdits;

/**
 * Concrete implementation of the ASI Container for IntelliJ IDEA.
 * <p>
 * This container integrates the Anahata framework with the IntelliJ IDEA
 * platform as an application-level singleton service, managing sessions, AI
 * providers, and multi-window tool window tabs. Implements {@link Disposable}
 * to ensure clean dynamic plugin unloading.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijAsiContainer extends AbstractSwingAsiContainer implements Disposable {

    /**
     * Registers the IntelliJ diff visualization for the core text-write tool
     * arguments and the IntelliJ ResourceUI.
     */
    static {
        initEnvironment();
    }

    /**
     * Bootstraps the IntelliJ global environment configuration, registering parameter
     * renderers, JSON serialization modules, and the IntelliJ native {@link uno.anahata.asi.swing.agi.resources.ResourceUI} strategy.
     */
    public static void initEnvironment() {
        // Make the shared Swing UI follow IntelliJ's theme authoritatively. IntelliJ's New UI does
        // not expose a reliable Panel.background to the swing module's luminance heuristic, so the
        // chat/dashboard rendered light even under a dark IDE theme; JBColor.isBright() is the IDE's
        // own light/dark flag. Set before any UITheme is constructed (this runs in the container's
        // static initializer, ahead of the tool-window dashboard build).
        SwingAgiConfig.setDarkModeDetector(() -> !JBColor.isBright());
        // Render the shared UI's action buttons with native, theme-adaptive IntelliJ AllIcons.
        uno.anahata.asi.swing.icons.AsiIcons.setProvider(new uno.anahata.asi.intellij.ui.IntellijAsiIcons());
        ParameterRendererFactory.register(FullTextResourceUpdate.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(TextResourceReplacements.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(TextResourceLineEdits.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.register(uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementBatch.class, IntellijTextResourceWriteRenderer.class);
        ParameterRendererFactory.registerById("java", IntellijJavaCodeParameterRenderer.class);
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

        // Follow the IDE theme live: when the user switches the IntelliJ theme
        // (Settings | Appearance & Behavior | Appearance | Theme) refresh the open Anahata UIs so
        // they re-adopt the new light/dark palette. Disposed with this application service.
        ApplicationManager.getApplication().getMessageBus().connect(this)
                .subscribe(LafManagerListener.TOPIC, (LafManagerListener) source -> refreshOpenUiThemes());
    }

    /**
     * Refreshes every open Anahata tool-window content after an IDE theme change, re-running the
     * Swing UI delegates so components pick up the new light/dark palette.
     * <p>
     * The authoritative dark-mode flag is read live via the detector registered in
     * {@link #initEnvironment()}, so newly built panels are already correct; this updates the panels
     * that are currently on screen.
     * </p>
     */
    private void refreshOpenUiThemes() {
        SwingUtilities.invokeLater(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                ToolWindow tw = ToolWindowManager.getInstance(project).getToolWindow("Anahata ASI");
                if (tw == null) {
                    continue;
                }
                for (Content c : tw.getContentManager().getContents()) {
                    Component component = c.getComponent();
                    if (component != null) {
                        SwingUtilities.updateComponentTreeUI(component);
                        component.revalidate();
                        component.repaint();
                    }
                }
            }
        });
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
     * in development mode when running directly off
     * {@code target/classes}.
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
     * Automatically approves migration of settings from predecessor versions
     * without displaying a modal dialog, preventing EDT deadlocks and circular
     * initialization exceptions during IDE startup.
     * </p>
     *
     * @param previousVersion The predecessor version string.
     * @param currentVersion The running container version string.
     * @return Always {@code true} to automatically import settings.
     */
    @Override
    protected boolean promptUpgrade(String previousVersion, String currentVersion) {
        log.info("Automatically importing settings from predecessor version {} to {}", previousVersion, currentVersion);
        return true;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Suppresses the modal confirmation dialog during IDE startup, logging the
     * result and recording a container notification instead.
     * </p>
     *
     * @param count The number of entities imported.
     * @param prevVerStr The predecessor version string.
     */
    @Override
    protected void showImportSuccess(int count, String prevVerStr) {
        log.info("Successfully imported {} settings from version {}.", count, prevVerStr);
        addNotification("Imported " + count + " settings from version " + prevVerStr);
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
     * Shuts down all active AGI sessions without closing them or modifying their persisted open state,
     * ensuring that session-level executor thread pools are terminated cleanly, then proceeds with
     * the standard container shutdown.
     * </p>
     */
    @Override
    public void shutdown() {
        log.info("Shutting down IntellijAsiContainer and all active AGI sessions");
        for (Agi agi : getActiveAgis()) {
            try {
                agi.shutdown();
            } catch (Throwable t) {
                log.warn("Error shutting down AGI session {}: {}", agi.getShortId(), t.getMessage());
            }
        }
        super.shutdown();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Shuts down background threads, key watcher, and container executors when
     * the plugin is dynamically unloaded by IntelliJ IDEA.
     * </p>
     */
    @Override
    public void dispose() {
        log.info("IntellijAsiContainer disposed by IntelliJ platform - shutting down container");
        shutdown();
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
     * {@inheritDoc}
     * <p>
     * Implementation details: Triggers a reactive UI refresh of the Project View tree across
     * all open IntelliJ project windows whenever session resources, nicknames, or visibility changes.
     * </p>
     *
     * @param agi The session whose state changed.
     * @param propertyName The property that changed.
     */
    @Override
    protected void onSessionContextChanged(Agi agi, String propertyName) {
        refreshProjectViews();
    }

    /**
     * Triggers a reactive UI refresh of the Project View tree across all open IntelliJ project windows.
     */
    public static void refreshProjectViews() {
        ApplicationManager.getApplication().invokeLater(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                ProjectView pv = ProjectView.getInstance(project);
                if (pv != null) {
                    pv.refresh();
                }
            }
        }, ModalityState.any());
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
