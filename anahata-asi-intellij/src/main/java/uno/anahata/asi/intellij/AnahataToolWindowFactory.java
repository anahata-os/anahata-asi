package uno.anahata.asi.intellij;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.openapi.wm.ex.ToolWindowEx;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import com.intellij.ui.content.ContentManagerEvent;
import com.intellij.ui.content.ContentManagerListener;
import com.intellij.util.ui.JBUI;
import org.jetbrains.annotations.NotNull;
import uno.anahata.asi.swing.AsiCardsContainerPanel;
import uno.anahata.asi.swing.agi.AgiPanel;

import com.intellij.ide.plugins.DynamicPlugins;
import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginMainDescriptor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.extensions.PluginId;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import lombok.extern.slf4j.Slf4j;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.wm.ToolWindowManager;
import java.awt.Component;
import java.awt.Container;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.intellij.internal.IntellijPluginUtils;
import uno.anahata.asi.intellij.ui.AnahataNotifications;
import uno.anahata.asi.swing.AbstractAsiContainerDashboardPanel;

import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.stream.Stream;
import lombok.SneakyThrows;

/**
 * Factory for creating the Anahata ASI Tool Window in IntelliJ.
 * <p>
 * Instantiates the {@link IntellijAsiContainer} and boots the master dashboard of session
 * cards. For a native look, the dashboard's own Swing toolbar is hidden and its actions
 * (new / import / preferences) are re-exposed as IntelliJ tool-window title-bar actions,
 * alongside a "Show Dashboard" action to jump back to the sticky overview tab and a dynamic
 * "Reload Plugin" action for fast development iteration.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class AnahataToolWindowFactory implements ToolWindowFactory {

    /**
     * The display name of the sticky dashboard content tab.
     */
    private static final String DASHBOARD_TAB = "Dashboard";

    /**
     * Flag indicating whether a dynamic plugin reload is currently in progress.
     * When true, content removal listeners do not mark active sessions as closed.
     */
    private static volatile boolean reloading = false;

    /**
     * Checks whether a dynamic plugin reload is currently in progress.
     *
     * @return {@code true} if dynamic reload is active, {@code false} otherwise.
     */
    public static boolean isReloading() {
        return reloading;
    }

    /**
     * Constructs the tool-window factory (instantiated by the platform via its public no-arg constructor).
     */
    public AnahataToolWindowFactory() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Builds the dashboard content, hides its in-panel toolbar, and installs native
     * title-bar actions.
     * </p>
     */
    @Override
    @SneakyThrows
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        IntellijAsiContainer asiContainer = IntellijAsiContainer.getInstance();

        toolWindow.getContentManager().addContentManagerListener(new ContentManagerListener() {
            @Override
            public void contentRemoved(@NotNull ContentManagerEvent event) {
                if (!reloading && event.getContent().getComponent() instanceof AgiPanel panel) {
                    asiContainer.close(panel.getAgi());
                }
            }
        });

        JPanel mainView = new JPanel(new BorderLayout());

        // Branded header banner (the wide Anahata wordmark reads best here, with horizontal room).
        JLabel header = new JLabel(IconLoader.getIcon("/icons/anahataHeader.png", AnahataToolWindowFactory.class));
        header.setBorder(JBUI.Borders.empty(6, 8));
        mainView.add(header, BorderLayout.NORTH);

        AsiCardsContainerPanel dashboard = new AsiCardsContainerPanel(asiContainer);
        // Hide the panel's own Swing toolbar; its actions are re-exposed natively below.
        dashboard.setToolBarVisible(false);
        dashboard.startRefresh();
        mainView.add(dashboard, BorderLayout.CENTER);

        ContentFactory contentFactory = ContentFactory.getInstance();
        Content content = contentFactory.createContent(mainView, DASHBOARD_TAB, false);
        content.setCloseable(false); // The master dashboard tab remains sticky and cannot be closed.
        toolWindow.getContentManager().addContent(content);

        for (Agi agi : asiContainer.getOpenAgis()) {
            AgiPanel agiPanel = new AgiPanel(agi);
            agiPanel.initComponents();

            Content sessionContent = contentFactory.createContent(agiPanel, agi.getDisplayName(), false);
            sessionContent.setCloseable(true);
            toolWindow.getContentManager().addContent(sessionContent);
        }

        if (!asiContainer.getOpenAgis().isEmpty()) {
            Content lastContent = toolWindow.getContentManager().getContent(toolWindow.getContentManager().getContentCount() - 1);
            if (lastContent != null) {
                toolWindow.getContentManager().setSelectedContent(lastContent);
            }
        }

        installTitleActions(toolWindow, dashboard);
    }

    /**
     * Installs the Anahata title-bar actions on the tool window header.
     *
     * @param toolWindow the tool window whose header hosts the actions.
     * @param dashboard  the master dashboard the actions delegate to.
     */
    private void installTitleActions(ToolWindow toolWindow, AsiCardsContainerPanel dashboard) {
        AnAction newSession = new DumbAwareAction("New Session", "Create a new Anahata ASI session", AllIcons.General.Add) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                dashboard.createNew();
                AnahataNotifications.info(e.getProject(), "New Anahata session created.");
            }
        };
        AnAction importSession = new DumbAwareAction("Import Session", "Import a saved Anahata ASI session", AllIcons.ToolbarDecorator.Import) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                dashboard.importSession();
            }
        };
        AnAction showDashboard = new DumbAwareAction("Show Dashboard", "Show the master session dashboard", AllIcons.Nodes.HomeFolder) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                for (Content c : toolWindow.getContentManager().getContents()) {
                    if (DASHBOARD_TAB.equals(c.getDisplayName())) {
                        toolWindow.getContentManager().setSelectedContent(c);
                        break;
                    }
                }
                toolWindow.show();
            }
        };
        AnAction preferences = new DumbAwareAction("Preferences", "Open Anahata ASI preferences", AllIcons.General.Settings) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                dashboard.showPreferences();
            }
        };
        AnAction reloadPlugin = new DumbAwareAction("Reload Plugin", "Reload the Anahata ASI plugin dynamically", AllIcons.Actions.Refresh) {
            @Override
            public void actionPerformed(@NotNull AnActionEvent e) {
                Project project = e.getProject();
                ApplicationManager.getApplication().invokeLater(() -> {
                    reloadPlugin(project);
                }, ModalityState.nonModal());
            }
        };

        if (toolWindow instanceof ToolWindowEx toolWindowEx) {
            toolWindowEx.setTitleActions(newSession, importSession, showDashboard, preferences, reloadPlugin);
        }
    }

    /**
     * Recursively traverses a component hierarchy and stops any active dashboard refresh timers.
     *
     * @param component the root component to inspect.
     */
    private static void stopDashboardTimers(Component component) {
        if (component instanceof AbstractAsiContainerDashboardPanel dashboard) {
            dashboard.stopRefresh();
        } else if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                stopDashboardTimers(child);
            }
        }
    }

    /**
     * Dynamically reloads the Anahata ASI plugin inside IntelliJ IDEA without restarting the IDE.
     * <p>
     * Cleans up open UI tabs, stops refresh timers, terminates background executors, unloads the
     * plugin descriptor, syncs newly built assembly JARs from the target directory into the plugin
     * lib folder, and reloads the plugin descriptor cleanly.
     * </p>
     *
     * @param project the active project context.
     */
    private static void reloadPlugin(Project project) {
        try {
            PluginId pluginId = PluginId.getId("uno.anahata.asi.intellij");
            IdeaPluginDescriptor descriptor = PluginManagerCore.getPlugin(pluginId);
            if (!(descriptor instanceof PluginMainDescriptor descriptorImpl)) {
                AnahataNotifications.error(project, "Cannot reload: Anahata plugin descriptor not found.");
                return;
            }

            reloading = true;

            // 1. Pre-unload cleanup: stop dashboard timers, detach tool window tabs, and shutdown container
            for (Project p : ProjectManager.getInstance().getOpenProjects()) {
                ToolWindow tw = ToolWindowManager.getInstance(p).getToolWindow("Anahata ASI");
                if (tw != null) {
                    for (Content c : tw.getContentManager().getContents()) {
                        Component comp = c.getComponent();
                        stopDashboardTimers(comp);
                    }
                    tw.getContentManager().removeAllContents(true);
                }
            }

            IntellijAsiContainer asiContainer = IntellijAsiContainer.getInstance();
            if (asiContainer != null) {
                asiContainer.shutdown();
            }

            IntellijPluginUtils.resetCachedPluginClasspath();

            // 2. Trigger dynamic unload via DynamicPlugins without tying modal progress to the project coroutine scope
            DynamicPlugins.UnloadPluginOptions options = new DynamicPlugins.UnloadPluginOptions()
                    .withDisable(false)
                    .withUpdate(true)
                    .withSave(true)
                    .withWaitForClassloaderUnload(false);

            boolean unloaded = DynamicPlugins.INSTANCE.unloadPlugin(
                    descriptorImpl,
                    options);

            if (!unloaded) {
                reloading = false;
                AnahataNotifications.warn(project, "Plugin could not be unloaded cleanly. Check idea.log for details.");
                return;
            }

            // 3. Post-unload file sync: now that the classloader is unloaded and file locks released, sync JARs
            Path pluginPath = descriptor.getPluginPath();
            if (project != null && project.getBasePath() != null && pluginPath != null) {
                Path projectRoot = Path.of(project.getBasePath());
                Path targetDir = projectRoot.resolve("anahata-asi-intellij").resolve("target");
                Path installedLib = pluginPath.resolve("lib");

                if (Files.isDirectory(targetDir) && Files.isDirectory(installedLib)) {
                    // Clean out old anahata-asi-*.jar to prevent duplicate version jars in lib
                    try (Stream<Path> stream = Files.list(installedLib)) {
                        stream.filter(p -> p.getFileName().toString().startsWith("anahata-asi-") && p.toString().endsWith(".jar"))
                                .forEach(oldJar -> {
                                    try {
                                        Files.deleteIfExists(oldJar);
                                    } catch (Exception ex) {
                                        log.warn("Could not delete old jar {}: {}", oldJar, ex.getMessage());
                                    }
                                });
                    }

                    Path assemblyZip = null;
                    try (Stream<Path> stream = Files.list(targetDir)) {
                        assemblyZip = stream
                                .filter(p -> p.getFileName().toString().endsWith(".zip") && !p.getFileName().toString().startsWith("."))
                                .findFirst()
                                .orElse(null);
                    }

                    if (assemblyZip != null) {
                        try (ZipFile zip = new ZipFile(assemblyZip.toFile())) {
                            var entries = zip.entries();
                            while (entries.hasMoreElements()) {
                                ZipEntry entry = entries.nextElement();
                                String entryName = entry.getName();
                                if (!entry.isDirectory() && entryName.contains("/lib/") && entryName.endsWith(".jar")) {
                                    String fileName = Path.of(entryName).getFileName().toString();
                                    Path dest = installedLib.resolve(fileName);
                                    try (var is = zip.getInputStream(entry)) {
                                        Files.copy(is, dest, StandardCopyOption.REPLACE_EXISTING);
                                    }
                                }
                            }
                            log.info("Synced plugin assembly ZIP from {} to {}", assemblyZip, installedLib);
                        }
                    } else {
                        log.warn("No assembly ZIP found in {}. Ensure 'mvn package' has run.", targetDir);
                    }
                }
            }

            // 4. Load the updated plugin
            boolean loaded = DynamicPlugins.INSTANCE.loadPlugin(descriptorImpl, project);
            reloading = false;
            if (loaded) {
                AnahataNotifications.info(project, "Anahata ASI plugin reloaded successfully!");
            } else {
                AnahataNotifications.error(project, "Failed to load plugin after unload. Restart may be required.");
            }
        } catch (Throwable t) {
            reloading = false;
            log.error("Plugin reload failed", t);
            AnahataNotifications.error(project, "Plugin reload failed: " + t.getMessage());
        }
    }
}
