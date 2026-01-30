/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
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
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.actions.Presenter;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;

/**
 * A context-aware action that provides a dynamic submenu for removing selected files
 * or folders from the context of an active AI chat session.
 * <p>
 * This action implements {@link Presenter.Popup} to generate a list of active
 * chat sessions as sub-menu items. It uses {@link FilesContextActionLogic} for the 
 * actual resource management.
 * </p>
 * 
 * @author anahata
 */
@ActionID(
        category = "Tools",
        id = "uno.anahata.asi.nb.context.ContextAwareRemoveFromContextAction"
)
@ActionRegistration(
        displayName = "Remove from AI Context",
        lazy = false
)
@ActionReferences({
    @ActionReference(path = "Loaders/folder/any/Actions", position = 1360),
    @ActionReference(path = "Loaders/content/unknown/Actions", position = 1360),
    @ActionReference(path = "Loaders/any/Actions", position = 1360),
})
public final class RemoveFilesFromContextAction extends AbstractAction implements ContextAwareAction, Presenter.Popup {
    private static final Logger LOG = Logger.getLogger(RemoveFilesFromContextAction.class.getName());
    
    /** The lookup context containing the selected files. */
    private final Lookup context;

    /**
     * Default constructor for registration.
     */
    public RemoveFilesFromContextAction() {
        this(Lookup.EMPTY);
    }

    /**
     * Private constructor for context-aware instances.
     * @param context The lookup context.
     */
    private RemoveFilesFromContextAction(Lookup context) {
        super("Remove from AI Context");
        this.context = context;
    }

    /**
     * {@inheritDoc}
     * Creates a new instance of this action for the given context.
     */
    @Override
    public Action createContextAwareInstance(Lookup context) {
        return new RemoveFilesFromContextAction(context);
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
     * Generates a dynamic submenu listing all active chat sessions.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        JMenu main = new JMenu("Remove from AI Context");
        Image img = ImageUtilities.loadImage("icons/delete.png");
        if (img != null) {
            main.setIcon(new ImageIcon(img.getScaledInstance(16, 16, Image.SCALE_SMOOTH)));
        }
        
        List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
        Collection<? extends FileObject> files = context.lookupAll(FileObject.class);
        
        if (activeChats.isEmpty()) {
            JMenuItem item = new JMenuItem("No active chats");
            item.setEnabled(false);
            main.add(item);
        } else {
            // 1. Remove from all chats option
            if (activeChats.size() > 1) {
                JMenuItem allItem = new JMenuItem("Remove from all active chats");
                allItem.addActionListener(e -> {
                    for (Chat chat : activeChats) {
                        for (FileObject fo : files) {
                            FilesContextActionLogic.removeRecursively(fo, chat);
                        }
                    }
                });
                main.add(allItem);
                main.addSeparator();
            }

            // 2. List individual chats
            for (Chat chat : activeChats) {
                JMenuItem item = new JMenuItem(chat.getNickname() + " (" + chat.getShortId() + ")");
                item.addActionListener(e -> {
                    for (FileObject fo : files) {
                        FilesContextActionLogic.removeRecursively(fo, chat);
                    }
                });
                main.add(item);
            }
        }
        
        return main;
    }
}
