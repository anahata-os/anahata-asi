/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.event.ActionEvent;
import java.util.Collection;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.filesystems.FileObject;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.actions.Presenter;
import uno.anahata.asi.AgiTopComponent;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;

/**
 * A context-aware action that provides a dynamic submenu for adding selected files
 * or folders to the context of an active AI chat session.
 * <p>
 * This action implements {@link Presenter.Popup} to generate a list of active
 * chat sessions as sub-menu items, along with options for multi-chat targeting.
 * It uses {@link FilesContextActionLogic} for the actual resource management.
 * </p>
 * 
 * @author anahata
 */
@ActionID(
        category = "Tools",
        id = "uno.anahata.asi.nb.context.ContextAwareAddToContextAction"
)
@ActionRegistration(
        displayName = "Add to AI Context",
        lazy = false
)
@ActionReferences({
    @ActionReference(path = "Loaders/folder/any/Actions", position = 1350),
    @ActionReference(path = "Loaders/content/unknown/Actions", position = 1350),
    @ActionReference(path = "Loaders/text/x-java/Actions", position = 1350),
    @ActionReference(path = "Loaders/text/x-maven-pom+xml/Actions", position = 1350),
    @ActionReference(path = "Loaders/any/Actions", position = 1350),
})
public final class AddFilesToContextAction extends AbstractAction implements ContextAwareAction, Presenter.Popup {
    private static final Logger LOG = Logger.getLogger(AddFilesToContextAction.class.getName());
    
    /** The lookup context containing the selected files. */
    private final Lookup context;

    /**
     * Default constructor for registration.
     */
    public AddFilesToContextAction() {
        this(Lookup.EMPTY);
    }

    /**
     * Private constructor for context-aware instances.
     * @param context The lookup context.
     */
    private AddFilesToContextAction(Lookup context) {
        super("Add to AI Context");
        this.context = context;
    }

    /**
     * {@inheritDoc}
     * Creates a new instance of this action for the given context.
     */
    @Override
    public Action createContextAwareInstance(Lookup context) {
        return new AddFilesToContextAction(context);
    }

    /**
     * {@inheritDoc}
     * This method is not used directly as the action is a presenter.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        // This action is a presenter, so this is not called directly for the menu.
    }

    /**
     * {@inheritDoc}
     * Generates a dynamic submenu listing all active chat sessions and targeting options.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        JMenu main = new JMenu("Add to AI Context");
        main.setIcon(new ImageIcon(org.openide.util.ImageUtilities.loadImage("icons/anahata_16.png")));
        
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        Collection<? extends FileObject> files = context.lookupAll(FileObject.class);

        // 1. Option to create a new session
        JMenuItem newChatItem = new JMenuItem("Create new session...");
        newChatItem.addActionListener(e -> {
            Chat newChat = AnahataInstaller.getContainer().createNewChat();
            
            // Open the TopComponent for the new chat
            AgiTopComponent tc = new AgiTopComponent(newChat);
            tc.open();
            tc.requestActive();
            
            for (FileObject fo : files) {
                // Non-recursive by default from the menu to avoid confusion
                FilesContextActionLogic.addRecursively(fo, newChat, false);
            }
            LOG.info("Created new session and added resources.");
        });
        main.add(newChatItem);
        main.addSeparator();

        if (activeChats.isEmpty()) {
            JMenuItem item = new JMenuItem("No active sessions");
            item.setEnabled(false);
            main.add(item);
        } else {
            // 2. Add to all sessions option
            if (activeChats.size() > 1) {
                JMenuItem allItem = new JMenuItem("Add to all active sessions");
                allItem.addActionListener(e -> {
                    for (Chat chat : activeChats) {
                        for (FileObject fo : files) {
                            FilesContextActionLogic.addRecursively(fo, chat, false);
                        }
                    }
                });
                main.add(allItem);
                main.addSeparator();
            }

            // 3. List individual sessions
            for (Chat chat : activeChats) {
                JMenuItem item = new JMenuItem(chat.getDisplayName());
                item.addActionListener(e -> {
                    for (FileObject fo : files) {
                        FilesContextActionLogic.addRecursively(fo, chat, false);
                    }
                });
                main.add(item);
            }
        }
        
        return main;
    }
}
