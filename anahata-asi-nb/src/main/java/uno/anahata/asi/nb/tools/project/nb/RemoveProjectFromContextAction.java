/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.nb;

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import org.netbeans.api.project.Project;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * Action to remove one or more projects from the AI context.
 * It provides a dynamic submenu listing all active chat sessions.
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
     * Generates a dynamic submenu listing all active chat sessions.
     * It only shows chats that have at least one of the selected projects in context.
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
        
        Image img = org.openide.util.ImageUtilities.loadImage("icons/delete.png");
        if (img != null) {
            main.setIcon(new ImageIcon(img.getScaledInstance(16, 16, Image.SCALE_SMOOTH)));
        }
        
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        final List<Project> finalProjects = projects;

        List<Chat> eligibleChats = activeChats.stream()
                .filter(chat -> anyProjectInContext(chat, finalProjects))
                .collect(Collectors.toList());
        
        if (eligibleChats.isEmpty()) {
            JMenuItem item = new JMenuItem(activeChats.isEmpty() ? "No active chats" : "No projects in context");
            item.setEnabled(false);
            main.add(item);
        } else {
            // Remove from all chats option
            if (eligibleChats.size() > 1) {
                JMenuItem allItem = new JMenuItem("Remove from all active chats");
                allItem.addActionListener(e -> {
                    for (Chat chat : eligibleChats) {
                        removeProjectsFromChat(chat, finalProjects);
                    }
                });
                main.add(allItem);
                main.addSeparator();
            }

            for (Chat chat : eligibleChats) {
                JMenuItem item = new JMenuItem(chat.getDisplayName());
                item.addActionListener(e -> removeProjectsFromChat(chat, finalProjects));
                main.add(item);
            }
        }
        
        return main;
    }

    /**
     * Checks if any of the selected projects are in the context of the given chat.
     * 
     * @param chat The chat session to check.
     * @param projects The list of projects.
     * @return {@code true} if at least one project is in context.
     */
    private boolean anyProjectInContext(Chat chat, List<Project> projects) {
        return projects.stream().anyMatch(p -> isProjectInContext(chat, p));
    }

    /**
     * Checks if a specific project is in the context of the given chat.
     * 
     * @param chat The chat session to check.
     * @param project The project to check.
     * @return {@code true} if the project is in context.
     */
    private boolean isProjectInContext(Chat chat, Project project) {
        return chat.getToolManager().getToolkitInstance(Projects.class)
                .flatMap(pt -> pt.getProjectProvider(project.getProjectDirectory().getPath()))
                .map(pcp -> pcp.isProviding())
                .orElse(false);
    }

    /**
     * Disables the project context provider for all selected projects in the given chat.
     * 
     * @param chat The target chat session.
     * @param projects The list of projects to remove.
     */
    private void removeProjectsFromChat(Chat chat, List<Project> projects) {
        chat.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
            for (Project p : projects) {
                String path = p.getProjectDirectory().getPath();
                projectsTool.setProjectProviderEnabled(path, false);
                LOG.info("Disabled project context for: " + path + " in chat: " + chat.getDisplayName());
            }
        });
    }
}
