package uno.anahata.asi.nb.context;

import java.awt.Image;
import java.util.logging.Logger;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectIconAnnotator;
import org.openide.filesystems.FileUtil;
import org.openide.util.ImageUtilities;
import org.openide.util.lookup.ServiceProvider;
//import uno.anahata.mavenproject9.FakeContext;

@ServiceProvider(service = ProjectIconAnnotator.class)
public class AnahataProjectAnnotator implements ProjectIconAnnotator, ChangeListener {
    private static final Logger LOG = Logger.getLogger(AnahataProjectAnnotator.class.getName());
    private static final Image BADGE;
    

    static {
        LOG.info("AnahataProjectAnnotator static init");
        Image img = ImageUtilities.loadImage("uno/anahata/mavenproject9/anahata.png");
        if (img != null) {
            BADGE = img.getScaledInstance(8, 8, Image.SCALE_SMOOTH);
        } else {
            BADGE = null;
        }
    }

    private final javax.swing.event.EventListenerList listeners = new javax.swing.event.EventListenerList();

    public AnahataProjectAnnotator() {
        LOG.info("AnahataProjectAnnotator constructor");
        //FakeContext.addChangeListener(this);
    }

    @Override
    public Image annotateIcon(Project p, Image icon, boolean opened) {
        LOG.info("AnahataProjectAnnotator.annotateIcon for project: " + p.getProjectDirectory().getName());
        /*
        if (FakeContext.contains(FileUtil.toFile(p.getProjectDirectory()))) {
            LOG.info("AnahataProjectAnnotator: Badging project " + p.getProjectDirectory().getName());
            if (BADGE != null) {
                return ImageUtilities.mergeImages(icon, BADGE, 8, 8);
            }
        }*/
        return icon;
    }

    @Override
    public void addChangeListener(ChangeListener cl) {
        LOG.info("AnahataProjectAnnotator.addChangeListener: " + cl);
        listeners.add(ChangeListener.class, cl);
    }

    @Override
    public void removeChangeListener(ChangeListener cl) {
        LOG.info("AnahataProjectAnnotator.removeChangeListener: " + cl);
        listeners.remove(ChangeListener.class, cl);
    }

    @Override
    public void stateChanged(ChangeEvent e) {
        LOG.info("AnahataProjectAnnotator.stateChanged: " + e);
        Object[] l = listeners.getListenerList();
        for (int i = l.length - 2; i >= 0; i -= 2) {
            if (l[i] == ChangeListener.class) {
                ((ChangeListener) l[i + 1]).stateChanged(e);
            }
        }
    }
}
