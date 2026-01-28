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
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;
import uno.anahata.asi.AgiTopComponent;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * Action to add one or more projects to the AI context.
 * It provides a dynamic submenu listing all active chat sessions and an option
 * to create a new chat.
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
     * Generates a dynamic submenu listing all active chat sessions.
     * It filters out chats where all selected projects are already in context.
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
        main.setIcon(new javax.swing.ImageIcon(org.openide.util.ImageUtilities.loadImage("icons/anahata_16.png")));
        
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        final List<Project> finalProjects = projects;

        // 1. Option to create a new chat
        JMenuItem newChatItem = new JMenuItem("Create new chat...");
        newChatItem.addActionListener(e -> {
            Chat newChat = AnahataInstaller.getContainer().createNewChat();
            
            // Open the TopComponent for the new chat
            AgiTopComponent tc = new AgiTopComponent(newChat);
            tc.open();
            tc.requestActive();
            
            addProjectsToChat(newChat, finalProjects);
            LOG.info("Created new chat and added projects.");
        });
        main.add(newChatItem);
        main.addSeparator();

        // 2. List active chats (filtered)
        List<Chat> eligibleChats = activeChats.stream()
                .filter(chat -> !allProjectsInContext(chat, finalProjects))
                .collect(Collectors.toList());

        if (eligibleChats.isEmpty()) {
            JMenuItem item = new JMenuItem(activeChats.isEmpty() ? "No active chats" : "All projects already in context");
            item.setEnabled(false);
            main.add(item);
        } else {
            // Add to all chats option
            if (eligibleChats.size() > 1) {
                JMenuItem allItem = new JMenuItem("Add to all active chats");
                allItem.addActionListener(e -> {
                    for (Chat chat : eligibleChats) {
                        addProjectsToChat(chat, finalProjects);
                    }
                });
                main.add(allItem);
                main.addSeparator();
            }

            for (Chat chat : eligibleChats) {
                JMenuItem item = new JMenuItem(chat.getDisplayName());
                item.addActionListener(e -> addProjectsToChat(chat, finalProjects));
                main.add(item);
            }
        }
        
        return main;
    }

    /**
     * Checks if all selected projects are already in the context of the given chat.
     * 
     * @param chat The chat session to check.
     * @param projects The list of projects.
     * @return {@code true} if all projects are in context.
     */
    private boolean allProjectsInContext(Chat chat, List<Project> projects) {
        return projects.stream().allMatch(p -> isProjectInContext(chat, p));
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
     * Enables the project context provider for all selected projects in the given chat.
     * 
     * @param chat The target chat session.
     * @param projects The list of projects to add.
     */
    private void addProjectsToChat(Chat chat, List<Project> projects) {
        chat.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
            for (Project p : projects) {
                String path = p.getProjectDirectory().getPath();
                projectsTool.setProjectProviderEnabled(path, true);
                LOG.info("Enabled project context for: " + path + " in chat: " + chat.getDisplayName());
            }
        });
    }
}
