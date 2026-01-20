/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.nb.context;

import java.awt.event.ActionEvent;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Action;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;

@ActionID(
        category = "Tools",
        id = "uno.anahata.mavenproject9.ContextAwareAddToContextAction"
)
@ActionRegistration(
        displayName = "ContextAwareAddToContextAction",
        lazy = true
)

@ActionReferences({
    @ActionReference(path = "Loaders/folder/any/Actions", position = 1350),
    @ActionReference(path = "Loaders/text/any/Actions", position = 1350),
})

public final class ContextAwareAddToContextAction extends AbstractAction implements ContextAwareAction {
    private static final Logger LOG = Logger.getLogger(ContextAwareAddToContextAction.class.getName());
    static {
        LOG.info("ContextAwareAddToContextAction static init");
    }
    public ContextAwareAddToContextAction() {
        super("ContextAwareAddToContextAction()");
        LOG.info("public ContextAwareAddToContextAction() {");
        setEnabled(true); // base action never used directly
        
    }

    @Override
    public Action createContextAwareInstance(Lookup context) {
        LOG.info("createContextAwareInstance(Lookup context) {" + context);
        return new AnahataContextAwareActionImpl(context);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        LOG.info("public void actionPerformed(ActionEvent e) {" + e);
        
        //log.
    }
    
    
}
