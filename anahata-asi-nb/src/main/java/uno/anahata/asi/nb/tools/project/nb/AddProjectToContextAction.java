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
import uno.anahata.asi.AgiTopComponent;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.swing.icons.IconUtils;

/**
 * Action to add one or more projects to the AI context.
 * It provides a dynamic submenu listing all active agi sessions and an option
 * to create a new agi.
 * <p>
 * This action implements {@link Presenter.Popup} to generate the dynamic menu.
 * 
 * @author anahata
 */
@ActionID(category = "Tools", id = "uno.anahata.asi.nb.tools.project.actions.AddProjectToContextAction")
@ActionRegistration(displayName = "#CTL_AddProjectToContextAction", lazy = false)
@ActionReference(path = "Projects/Actions", position = 500)
public final class AddProjectToContextAction extends AbstractAction implements ContextAwareAction, Presenter.Popup {

    private static final Logger LOG = Logger.getLogger(AddProjectToContextAction.class.getName());
    
    /** The lookup context containing the selected projects. */
    private final Lookup context;

    /**
     * Default constructor required by NetBeans action registration.
     */
    public AddProjectToContextAction() {
        this(Lookup.EMPTY);
    }

    /**
     * Constructs the action with a specific lookup context.
     * @param context The lookup context.
     */
    private AddProjectToContextAction(Lookup context) {
        super(NbBundle.getMessage(AddProjectToContextAction.class, "CTL_AddProjectToContextAction"));
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
        return new AddProjectToContextAction(context);
    }

    /**
     * {@inheritDoc}
     * Generates a dynamic submenu listing all active agi sessions.
     * It filters out agis where all selected projects are already in context.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        List<Project> projects = new ArrayList<>(context.lookupAll(Project.class));
        if (projects.isEmpty()) {
            projects = new ArrayList<>(Utilities.actionsGlobalContext().lookupAll(Project.class));
        }
        
        int count = projects.size();
        if (count == 0) {
            JMenuItem item = new JMenuItem(NbBundle.getMessage(AddProjectToContextAction.class, "CTL_AddProjectToContextAction"));
            item.setEnabled(false);
            return item;
        }
        
        String label = count > 1 ? "Add " + count + " projects to AI context" : "Add project to AI context";
        JMenu main = new JMenu(label);
        main.setIcon(IconUtils.getAddIcon());
        
        List<Agi> activeAgis = AnahataInstaller.getContainer().getActiveAgis();
        final List<Project> finalProjects = projects;

        // 1. Option to create a new session
        JMenuItem newAgiItem = new JMenuItem("Create new session...");
        newAgiItem.addActionListener(e -> {
            Agi newAgi = AnahataInstaller.getContainer().createNewAgi();
            
            // Open the TopComponent for the new agi
            AgiTopComponent tc = new AgiTopComponent(newAgi);
            tc.open();
            tc.requestActive();
            
            addProjectsToAgi(newAgi, finalProjects);
            LOG.info("Created new session and added projects.");
        });
        main.add(newAgiItem);
        main.addSeparator();

        // 2. List active sessions (filtered)
        List<Agi> eligibleAgis = activeAgis.stream()
                .filter(agi -> !allProjectsInContext(agi, finalProjects))
                .collect(Collectors.toList());

        if (eligibleAgis.isEmpty()) {
            JMenuItem item = new JMenuItem(activeAgis.isEmpty() ? "No active sessions" : "All projects already in context");
            item.setEnabled(false);
            main.add(item);
        } else {
            // Add to all sessions option
            if (eligibleAgis.size() > 1) {
                JMenuItem allItem = new JMenuItem("Add to all active sessions");
                allItem.addActionListener(e -> {
                    for (Agi agi : eligibleAgis) {
                        addProjectsToAgi(agi, finalProjects);
                    }
                });
                main.add(allItem);
                main.addSeparator();
            }

            for (Agi agi : eligibleAgis) {
                JMenuItem item = new JMenuItem(agi.getDisplayName());
                item.addActionListener(e -> addProjectsToAgi(agi, finalProjects));
                main.add(item);
            }
        }
        
        return main;
    }

    /**
     * Checks if all selected projects are already in the context of the given agi.
     * 
     * @param agi The agi session to check.
     * @param projects The list of projects.
     * @return {@code true} if all projects are in context.
     */
    private boolean allProjectsInContext(Agi agi, List<Project> projects) {
        return projects.stream().allMatch(p -> ProjectsContextActionLogic.isProjectInContext(p, agi));
    }

    /**
     * Enables the project context provider for all selected projects in the given agi.
     * 
     * @param agi The target agi session.
     * @param projects The list of projects to add.
     */
    private void addProjectsToAgi(Agi agi, List<Project> projects) {
        agi.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
            for (Project p : projects) {
                String path = p.getProjectDirectory().getPath();
                projectsTool.setProjectProviderEnabled(path, true);
                LOG.info("Enabled project context for: " + path + " in session: " + agi.getDisplayName());
            }
        });
    }
}
