/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.Set;
import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import org.openide.filesystems.FileObject;
import org.openide.util.ImageUtilities;
import org.openide.util.actions.Presenter;
import uno.anahata.asi.AgiTopComponent;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.agi.Agi;

/**
 * A presenter for the dynamic "AI Context" context menu.
 * <p>
 * This class implements {@link Presenter.Popup} to generate a hierarchical menu 
 * that allows users to add or remove selected files from active AI sessions. 
 * It centralizes the menu logic, replacing static MIME-type based actions.
 * </p>
 * 
 * @author anahata
 */
public class AnahataContextActionPresenter extends AbstractAction implements Presenter.Popup {

    /** The set of files currently selected in the IDE. */
    private final Set<? extends FileObject> files;

    /**
     * Constructs a new presenter for the given set of files.
     * 
     * @param files The selected files.
     */
    public AnahataContextActionPresenter(Set<? extends FileObject> files) {
        super("AI Context");
        this.files = files;
    }

    /**
     * Inherited from {@link AbstractAction}. This method is not used directly 
     * as this action only serves as a popup presenter.
     * 
     * @param e The action event.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        // No direct action
    }

    /**
     * Builds and returns the dynamic "AI Context" menu item.
     * <p>
     * Implementation details:
     * 1. Lists all active sessions (Agis) from the installer.
     * 2. Provides an option to create a new session and immediately add the selection.
     * 3. Dynamically filters the "Remove" menu to only show sessions containing the selection.
     * </p>
     * 
     * @return A JMenuItem (specifically a JMenu) representing the context menu.
     */
    @Override
    public JMenuItem getPopupPresenter() {
        JMenu main = new JMenu("AI Context");
        main.setIcon(new ImageIcon(ImageUtilities.loadImage("icons/anahata_16.png")));

        List<Agi> activeAgis = AnahataInstaller.getContainer().getActiveAgis();
        
        // 1. Add Submenu
        JMenu addMenu = new JMenu("Add to Context");
        addMenu.setIcon(new ImageIcon(ImageUtilities.loadImage("icons/anahata_16.png")));
        
        JMenuItem newAgiItem = new JMenuItem("New session...");
        newAgiItem.addActionListener(e -> {
            Agi newAgi = AnahataInstaller.getContainer().createNewAgi();
            AgiTopComponent tc = new AgiTopComponent(newAgi);
            tc.open();
            tc.requestActive();
            for (FileObject fo : files) {
                FilesContextActionLogic.addRecursively(fo, newAgi, false);
            }
        });
        addMenu.add(newAgiItem);
        addMenu.addSeparator();

        for (Agi agi : activeAgis) {
            JMenuItem item = new JMenuItem(agi.getDisplayName());
            item.addActionListener(e -> {
                for (FileObject fo : files) {
                    FilesContextActionLogic.addRecursively(fo, agi, false);
                }
            });
            addMenu.add(item);
        }
        main.add(addMenu);

        // 2. Remove Submenu
        JMenu removeMenu = new JMenu("Remove from Context");
        Image delImg = ImageUtilities.loadImage("icons/delete.png");
        if (delImg != null) {
            removeMenu.setIcon(new ImageIcon(delImg.getScaledInstance(16, 16, Image.SCALE_SMOOTH)));
        }

        for (Agi agi : activeAgis) {
            boolean hasAny = false;
            for (FileObject fo : files) {
                if (FilesContextActionLogic.isInContext(fo, agi)) {
                    hasAny = true;
                    break;
                }
            }
            
            if (hasAny) {
                JMenuItem item = new JMenuItem(agi.getDisplayName());
                item.addActionListener(e -> {
                    for (FileObject fo : files) {
                        FilesContextActionLogic.removeRecursively(fo, agi, false);
                    }
                });
                removeMenu.add(item);
            }
        }
        
        if (removeMenu.getItemCount() > 0) {
            main.add(removeMenu);
        }

        return main;
    }
}
