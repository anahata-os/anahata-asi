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
import com.intellij.openapi.extensions.PluginId;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.intellij.ui.AnahataNotifications;

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
                if (event.getContent().getComponent() instanceof AgiPanel panel) {
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
                uno.anahata.asi.intellij.ui.AnahataNotifications.info(e.getProject(), "New Anahata session created.");
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
                reloadPlugin(e.getProject(), toolWindow);
            }
        };

        if (toolWindow instanceof ToolWindowEx toolWindowEx) {
            toolWindowEx.setTitleActions(newSession, importSession, showDashboard, preferences, reloadPlugin);
        }
    }

    /**
     * Dynamically reloads the Anahata ASI plugin inside IntelliJ IDEA without restarting the IDE.
     * <p>
     * Syncs the freshly packaged JAR from the workspace target directory directly into the
     * active plugin lib folder, then uses IntelliJ's {@link DynamicPlugins} engine to unload
     * and reload the plugin descriptor in place.
     * </p>
     *
     * @param project    the active project context.
     * @param toolWindow the tool window instance.
     */
    private static void reloadPlugin(Project project, ToolWindow toolWindow) {
        try {
            PluginId pluginId = PluginId.getId("uno.anahata.asi.intellij");
            IdeaPluginDescriptor descriptor = PluginManagerCore.getPlugin(pluginId);
            if (!(descriptor instanceof PluginMainDescriptor descriptorImpl)) {
                AnahataNotifications.error(project, "Cannot reload: Anahata plugin descriptor not found.");
                return;
            }

            // 1. Sync updated JARs or assembly ZIP from target/ into the installed plugin directory
            Path pluginPath = descriptor.getPluginPath();
            if (project != null && project.getBasePath() != null && pluginPath != null) {
                Path projectRoot = Path.of(project.getBasePath());
                Path targetDir = projectRoot.resolve("anahata-asi-intellij").resolve("target");
                Path installedLib = pluginPath.resolve("lib");

                if (Files.isDirectory(targetDir) && Files.isDirectory(installedLib)) {
                    // Option A: If the full assembly ZIP was built, unpack all runtime JARs from it
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
                            log.info("Synced full plugin assembly ZIP from {} to {}", assemblyZip, installedLib);
                        }
                    } else {
                        // Option B: Sync all anahata-asi-*.jar from open module target directories (core, swing, intellij)
                        List<Path> candidateModules = List.of(
                                projectRoot.resolve("anahata-asi-intellij"),
                                projectRoot.resolve("anahata-asi-core"),
                                projectRoot.resolve("anahata-asi-swing")
                        );
                        for (Path mod : candidateModules) {
                            Path modTarget = mod.resolve("target");
                            if (Files.isDirectory(modTarget)) {
                                try (Stream<Path> stream = Files.list(modTarget)) {
                                    stream.filter(p -> p.getFileName().toString().startsWith("anahata-asi-")
                                                    && p.toString().endsWith(".jar")
                                                    && !p.toString().endsWith("-sources.jar")
                                                    && !p.toString().endsWith("-javadoc.jar"))
                                            .forEach(candidateJar -> {
                                                try {
                                                    Path dest = installedLib.resolve(candidateJar.getFileName());
                                                    Files.copy(candidateJar, dest, StandardCopyOption.REPLACE_EXISTING);
                                                    log.info("Synced updated jar from {} to {}", candidateJar, dest);
                                                } catch (Exception ex) {
                                                    log.warn("Failed to copy jar {}: {}", candidateJar, ex.getMessage());
                                                }
                                            });
                                }
                            }
                        }
                    }
                }
            }

            // 2. Trigger dynamic reload via DynamicPlugins
            DynamicPlugins.UnloadPluginOptions options = new DynamicPlugins.UnloadPluginOptions()
                    .withDisable(false)
                    .withUpdate(true)
                    .withSave(true)
                    .withWaitForClassloaderUnload(false);

            boolean unloaded = DynamicPlugins.INSTANCE.unloadPluginWithProgress(
                    project,
                    toolWindow != null ? toolWindow.getComponent() : null,
                    descriptorImpl,
                    options);

            if (!unloaded) {
                AnahataNotifications.warn(project, "Plugin could not be unloaded cleanly. Check idea.log for details.");
                return;
            }

            boolean loaded = DynamicPlugins.INSTANCE.loadPlugin(descriptorImpl, project);
            if (loaded) {
                AnahataNotifications.info(project, "Anahata ASI plugin reloaded successfully!");
            } else {
                AnahataNotifications.error(project, "Failed to load plugin after unload. Restart may be required.");
            }
        } catch (Throwable t) {
            log.error("Plugin reload failed", t);
            AnahataNotifications.error(project, "Plugin reload failed: " + t.getMessage());
        }
    }
}
