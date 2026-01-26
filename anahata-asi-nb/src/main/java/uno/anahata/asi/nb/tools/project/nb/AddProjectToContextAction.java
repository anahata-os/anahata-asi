/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.nb;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import org.netbeans.api.project.Project;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;
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
@ActionRegistration(displayName = "#CTL_AddProjectToContextAction", iconBase = "icons/anahata.png", asynchronous = true, lazy = true)
@ActionReference(path = "Projects/Actions", position = 500)
public final class AddProjectToContextAction extends AbstractAction implements Presenter.Popup {

    private static final Logger LOG = Logger.getLogger(AddProjectToContextAction.class.getName());
    
    /** The list of selected projects. */
    private final List<Project> context;

    /**
     * Default constructor required by NetBeans action registration.
     * It uses the global selection context to find projects.
     */
    public AddProjectToContextAction() {
        this(new ArrayList<>(Utilities.actionsGlobalContext().lookupAll(Project.class)));
    }

    /**
     * Constructs the action with a specific list of projects.
     * @param context The list of selected projects.
     */
    public AddProjectToContextAction(List<Project> context) {
        super(NbBundle.getMessage(AddProjectToContextAction.class, "CTL_AddProjectToContextAction"));
        this.context = context;
    }

    @Override
    public void actionPerformed(ActionEvent ev) {
        // Presenter action, not called directly.
    }

    /**
     * {@inheritDoc}
     * Generates a dynamic submenu listing all active chat sessions.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        int count = context.size();
        String label = count > 1 ? "Add " + count + " projects to AI context" : "Add project to AI context";
        JMenu main = new JMenu(label);
        main.setIcon(new javax.swing.ImageIcon(org.openide.util.ImageUtilities.loadImage("icons/anahata.png")));
        
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        
        // 1. Option to create a new chat
        JMenuItem newChatItem = new JMenuItem("Create new chat...");
        newChatItem.addActionListener(e -> {
            Chat newChat = AnahataInstaller.getContainer().createNewChat();
            
            // Open the TopComponent for the new chat
            AgiTopComponent tc = new AgiTopComponent(newChat);
            tc.open();
            tc.requestActive();
            
            addProjectsToChat(newChat);
            LOG.info("Created new chat and added projects.");
        });
        main.add(newChatItem);
        main.addSeparator();

        // 2. List active chats
        if (activeChats.isEmpty()) {
            JMenuItem item = new JMenuItem("No active chats");
            item.setEnabled(false);
            main.add(item);
        } else {
            for (Chat chat : activeChats) {
                JMenuItem item = new JMenuItem(chat.getDisplayName());
                item.addActionListener(e -> addProjectsToChat(chat));
                main.add(item);
            }
        }
        
        return main;
    }

    /**
     * Enables the project context provider for all selected projects in the given chat.
     * 
     * @param chat The target chat session.
     */
    private void addProjectsToChat(Chat chat) {
        chat.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
            for (Project p : context) {
                String path = p.getProjectDirectory().getPath();
                projectsTool.setProjectProviderEnabled(path, true);
                LOG.info("Enabled project context for: " + path + " in chat: " + chat.getDisplayName());
            }
        });
    }
}
