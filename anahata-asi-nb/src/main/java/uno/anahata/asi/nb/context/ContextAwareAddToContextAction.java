/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.context;

import java.awt.event.ActionEvent;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.actions.Presenter;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;

/**
 * A context-aware action that provides a dynamic submenu for adding selected files
 * to the context of an active AI chat session.
 * <p>
 * This action implements {@link Presenter.Popup} to generate a list of active
 * chat sessions as sub-menu items.
 * 
 * @author anahata
 */
@ActionID(
        category = "Tools",
        id = "uno.anahata.asi.nb.context.ContextAwareAddToContextAction"
)
@ActionRegistration(
        displayName = "Add to AI Context",
        lazy = true
)
@ActionReferences({
    @ActionReference(path = "Loaders/folder/any/Actions", position = 1350),
    @ActionReference(path = "Loaders/text/any/Actions", position = 1350),
})
public final class ContextAwareAddToContextAction extends AbstractAction implements ContextAwareAction, Presenter.Popup {
    private static final Logger LOG = Logger.getLogger(ContextAwareAddToContextAction.class.getName());
    
    /** The lookup context containing the selected files. */
    private final Lookup context;

    /**
     * Default constructor for registration.
     */
    public ContextAwareAddToContextAction() {
        this(Lookup.EMPTY);
    }

    /**
     * Private constructor for context-aware instances.
     * @param context The lookup context.
     */
    private ContextAwareAddToContextAction(Lookup context) {
        super("Add to AI Context");
        this.context = context;
    }

    /**
     * {@inheritDoc}
     * Creates a new instance of this action for the given context.
     */
    @Override
    public Action createContextAwareInstance(Lookup context) {
        return new ContextAwareAddToContextAction(context);
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
     * If no chats are active, a disabled placeholder item is shown.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        JMenu main = new JMenu("Add to AI Context");
        List<Chat> activeChats = AnahataInstaller.getAsiConfig().getActiveChats();
        
        if (activeChats.isEmpty()) {
            JMenuItem item = new JMenuItem("No active chats");
            item.setEnabled(false);
            main.add(item);
        } else {
            for (Chat chat : activeChats) {
                JMenuItem item = new JMenuItem(chat.getNickname() + " (" + chat.getShortId() + ")");
                item.addActionListener(e -> {
                    AnahataContextAwareActionImpl impl = new AnahataContextAwareActionImpl(context, chat);
                    impl.actionPerformed(e);
                });
                main.add(item);
            }
        }
        
        return main;
    }
}