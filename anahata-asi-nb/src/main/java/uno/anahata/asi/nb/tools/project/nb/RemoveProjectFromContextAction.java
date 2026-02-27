/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.nb;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import org.netbeans.api.project.Project;
import org.openide.awt.ActionID;
import org.openide.awt.ActionRegistration;
import org.openide.awt.ActionReference;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * Action to remove one or more projects from the AI context.
 * It provides a dynamic submenu listing all active agi sessions.
 * <p>
 * This action implements {@link Presenter.Popup} to generate the dynamic menu.
 * 
 * @author anahata
 */
@ActionID(category = "Tools", id = "uno.anahata.asi.nb.tools.project.actions.RemoveProjectFromContextAction")
@ActionRegistration(displayName = "#CTL_RemoveProjectFromContextAction", lazy = false)
@ActionReference(path = "Projects/Actions", position = 510)
public final class RemoveProjectFromContextAction extends AbstractAction implements ContextAwareAction, Presenter.Popup {

    private static final Logger LOG = Logger.getLogger(RemoveProjectFromContextAction.class.getName());
    
    /** The lookup context containing the selected projects. */
    private final Lookup context;

    /**
     * Default constructor required by NetBeans action registration.
     */
    public RemoveProjectFromContextAction() {
        this(Lookup.EMPTY);
    }

    /**
     * Constructs the action with a specific lookup context.
     * @param context The lookup context.
     */
    private RemoveProjectFromContextAction(Lookup context) {
        super(NbBundle.getMessage(RemoveProjectFromContextAction.class, "CTL_RemoveProjectFromContextAction"));
        this.context = context;
    }

    /**
     * {@inheritDoc}
     * This method is not used directly as the action is a presenter.
     */
    @Override
    public void actionPerformed(ActionEvent ev) {
        // Presenter action, not called directly.
    }

    /**
     * {@inheritDoc}
     * Creates a new instance of this action for the given context.
     */
    @Override
    public Action createContextAwareInstance(Lookup context) {
        return new RemoveProjectFromContextAction(context);
    }

    /**
     * {@inheritDoc}
     * Generates a dynamic submenu listing all active agi sessions.
     * It only shows agis that have at least one of the selected projects in context.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        List<Project> projects = new ArrayList<>(context.lookupAll(Project.class));
        if (projects.isEmpty()) {
            projects = new ArrayList<>(Utilities.actionsGlobalContext().lookupAll(Project.class));
        }
        
        int count = projects.size();
        if (count == 0) {
            JMenuItem item = new JMenuItem(NbBundle.getMessage(RemoveProjectFromContextAction.class, "CTL_RemoveProjectFromContextAction"));
            item.setEnabled(false);
            return item;
        }
        
        String label = count > 1 ? "Remove " + count + " projects from AI context" : "Remove project from AI context";
        JMenu main = new JMenu(label);
        main.setIcon(IconUtils.getRemoveIcon());
        
        List<Agi> activeAgis = AnahataInstaller.getContainer().getActiveAgis();
        final List<Project> finalProjects = projects;

        List<Agi> eligibleAgis = activeAgis.stream()
                .filter(agi -> anyProjectInContext(agi, finalProjects))
                .collect(Collectors.toList());
        
        if (eligibleAgis.isEmpty()) {
            JMenuItem item = new JMenuItem(activeAgis.isEmpty() ? "No active sessions" : "No projects in context");
            item.setEnabled(false);
            main.add(item);
        } else {
            // Remove from all sessions option
            if (eligibleAgis.size() > 1) {
                JMenuItem allItem = new JMenuItem("Remove from all active sessions");
                allItem.addActionListener(e -> {
                    for (Agi agi : eligibleAgis) {
                        removeProjectsFromAgi(agi, finalProjects);
                    }
                });
                main.add(allItem);
                main.addSeparator();
            }

            for (Agi agi : eligibleAgis) {
                JMenuItem item = new JMenuItem(agi.getDisplayName());
                item.addActionListener(e -> removeProjectsFromAgi(agi, finalProjects));
                main.add(item);
            }
        }
        
        return main;
    }

    /**
     * Checks if any of the selected projects are in the context of the given agi.
     * 
     * @param agi The agi session to check.
     * @param projects The list of projects.
     * @return {@code true} if at least one project is in context.
     */
    private boolean anyProjectInContext(Agi agi, List<Project> projects) {
        return projects.stream().anyMatch(p -> ProjectsContextActionLogic.isProjectInContext(p, agi));
    }

    /**
     * Disables the project context provider for all selected projects in the given agi.
     * 
     * @param agi The target agi session.
     * @param projects The list of projects to remove.
     */
    private void removeProjectsFromAgi(Agi agi, List<Project> projects) {
        agi.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
            for (Project p : projects) {
                String path = p.getProjectDirectory().getPath();
                projectsTool.setProjectProviderEnabled(path, false);
                LOG.info("Disabled project context for: " + path + " in session: " + agi.getDisplayName());
            }
        });
    }
}
