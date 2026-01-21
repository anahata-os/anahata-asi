/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.actions;

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
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;
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
@ActionRegistration(displayName = "Add project(s) to AI context", iconBase = "icons/anahata.png", asynchronous = true, lazy = true)
@ActionReference(path = "Projects/Actions", position = 500)
public final class AddProjectToContextAction extends AbstractAction implements Presenter.Popup {

    private static final Logger LOG = Logger.getLogger(AddProjectToContextAction.class.getName());
    
    /** The list of selected projects. */
    private final List<Project> context;

    /**
     * Default constructor required by NetBeans action registration.
     * It uses the global selection context to find projects.
     * <p>
     * When this action is invoked from the Projects tab, NetBeans uses this 
     * constructor and populates the context from the current selection.
     */
    public AddProjectToContextAction() {
        this(new ArrayList<>(Utilities.actionsGlobalContext().lookupAll(Project.class)));
    }

    /**
     * Constructs the action with a specific list of projects.
     * @param context The list of selected projects.
     */
    public AddProjectToContextAction(List<Project> context) {
        super("Add project(s) to AI context");
        this.context = context;
    }

    /**
     * {@inheritDoc}
     * This method is not used directly as the action is a presenter.
     */
    @Override
    public void actionPerformed(ActionEvent ev) {
        // This action is a presenter, so this is not called directly for the menu.
    }

    /**
     * {@inheritDoc}
     * Generates a dynamic submenu listing all active chat sessions and a 
     * 'Create new chat' option.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        JMenu main = new JMenu("Add project(s) to AI context");
        List<Chat> activeChats = AnahataInstaller.getAsiConfig().getActiveChats();
        
        // 1. Option to create a new chat
        JMenuItem newChatItem = new JMenuItem("Create new chat...");
        newChatItem.addActionListener(e -> {
            // TODO: Implement new chat creation and project addition
            LOG.info("Create new chat and add projects requested.");
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
                JMenuItem item = new JMenuItem(chat.getNickname() + " (" + chat.getShortId() + ")");
                item.addActionListener(e -> {
                    for (Project p : context) {
                        String path = p.getProjectDirectory().getPath();
                        chat.getToolManager().getToolkitInstance(Projects.class).ifPresent(projectsTool -> {
                            projectsTool.setProjectProviderEnabled(path, true);
                            LOG.info("Enabled project context for: " + path + " in chat: " + chat.getNickname());
                        });
                    }
                });
                main.add(item);
            }
        }
        
        return main;
    }
}
