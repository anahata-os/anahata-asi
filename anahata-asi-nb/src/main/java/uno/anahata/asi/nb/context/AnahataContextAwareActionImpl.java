/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.nb.context;

import java.awt.event.ActionEvent;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Action;
import org.openide.filesystems.FileObject;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.LookupEvent;
import org.openide.util.LookupListener;

public final class AnahataContextAwareActionImpl extends AbstractAction implements LookupListener {

    private static final Logger LOG = Logger.getLogger(AnahataContextAwareActionImpl.class.getName());
    
    private final Lookup.Result<FileObject> result;

    AnahataContextAwareActionImpl(Lookup context) {
        super("AnahataContextAwareActionImpl");
        LOG.info("AnahataContextAwareActionImpl constructor context= " + context);
        this.result = context.lookupResult(FileObject.class);
        LOG.info("result=" + result);
        this.result.addLookupListener(this);
        resultChanged(null);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        LOG.info("actionPerformed " + e);
        for (FileObject fo : result.allInstances()) {
            LOG.info("fileObject " + fo);
            // operate on selected files
        }
    }

    @Override
    public void resultChanged(LookupEvent ev) {
        LOG.info("resultChanged " + ev);
        setEnabled(!result.allInstances().isEmpty());
        LOG.info("enabled " + isEnabled());
    }

    
}
