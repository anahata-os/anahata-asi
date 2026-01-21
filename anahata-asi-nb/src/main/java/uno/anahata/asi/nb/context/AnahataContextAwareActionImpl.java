/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.context;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Lookup;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.resource.TextFileResource;

/**
 * Implementation of the context-aware action to add files to a specific AI context.
 * This class handles the actual logic of converting FileObjects to TextFileResources
 * and registering them with the selected Chat's ResourceManager.
 * <p>
 * It listens to the lookup context to enable/disable itself based on selection.
 * 
 * @author anahata
 */
public final class AnahataContextAwareActionImpl extends AbstractAction implements LookupListener {

    private static final Logger LOG = Logger.getLogger(AnahataContextAwareActionImpl.class.getName());
    
    /** The lookup result containing the selected file objects. */
    private final Lookup.Result<FileObject> result;
    
    /** The target chat session to add resources to. */
    private final Chat targetChat;

    /**
     * Constructs an implementation for a specific chat.
     * @param context The lookup context containing selected files.
     * @param targetChat The chat session to add resources to.
     */
    public AnahataContextAwareActionImpl(Lookup context, Chat targetChat) {
        super("Add to AI Context");
        this.targetChat = targetChat;
        this.result = context.lookupResult(FileObject.class);
        this.result.addLookupListener(this);
        resultChanged(null);
    }

    /**
     * {@inheritDoc}
     * Iterates over all selected FileObjects and adds them as resources to the target chat.
     * Only data files (not folders) are added.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (targetChat == null) {
            return;
        }
        
        for (FileObject fo : result.allInstances()) {
            if (fo.isData()) {
                try {
                    File file = FileUtil.toFile(fo);
                    if (file != null) {
                        String path = file.getAbsolutePath();
                        // Check if already loaded in this chat
                        if (targetChat.getResourceManager().findByPath(path).isEmpty()) {
                            TextFileResource resource = new TextFileResource(file.toPath());
                            targetChat.getResourceManager().register(resource);
                            LOG.info("Added file to context of chat '" + targetChat.getNickname() + "': " + path);
                        } else {
                            LOG.info("File already in context: " + path);
                        }
                    }
                } catch (Exception ex) {
                    LOG.log(java.util.logging.Level.SEVERE, "Error adding file to context", ex);
                }
            }
        }
    }

    /**
     * {@inheritDoc}
     * Updates the enabled state of the action based on the current selection.
     */
    @Override
    public void resultChanged(LookupEvent ev) {
        setEnabled(!result.allInstances().isEmpty());
    }
}