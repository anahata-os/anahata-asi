package uno.anahata.asi.intellij;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.openapi.wm.ex.ToolWindowEx;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import com.intellij.util.ui.JBUI;
import org.jetbrains.annotations.NotNull;
import uno.anahata.asi.swing.AsiCardsContainerPanel;

import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;

/**
 * Factory for creating the Anahata ASI Tool Window in IntelliJ.
 * <p>
 * Instantiates the {@link IntellijAsiContainer} and boots the master dashboard of session
 * cards. For a native look, the dashboard's own Swing toolbar is hidden and its actions
 * (new / import / preferences) are re-exposed as IntelliJ tool-window title-bar actions,
 * alongside a "Show Dashboard" action to jump back to the sticky overview tab.
 * </p>
 *
 * @author anahata
 */
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
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        IntellijAsiContainer asiContainer = new IntellijAsiContainer(toolWindow);
        // Keep the persisted AGI template's toolkit list in sync with the currently registered
        // toolkits, so newly added toolkits appear in Preferences and in new sessions.
        asiContainer.syncTemplateToolkits();
        // Deregister the container from the live registry when the project is disposed,
        // so closed projects no longer surface stale sessions to the Project-view UI.
        Disposer.register(project, () -> IntellijAsiContainer.removeInstance(asiContainer));

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

        if (toolWindow instanceof ToolWindowEx toolWindowEx) {
            toolWindowEx.setTitleActions(newSession, importSession, showDashboard, preferences);
        }
    }
}
