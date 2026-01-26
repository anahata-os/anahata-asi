package uno.anahata.asi.nb.context;

import java.awt.Image;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import org.netbeans.modules.masterfs.filebasedfs.FileBasedFileSystem;
import org.netbeans.modules.masterfs.providers.AnnotationProvider;
import org.netbeans.modules.masterfs.providers.InterceptionListener;
import org.netbeans.modules.versioning.core.VersioningAnnotationProvider;
import org.netbeans.modules.versioning.core.VersioningManager;
import org.netbeans.modules.versioning.core.util.VCSSystemProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStatusEvent;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.actions.Presenter;
import org.openide.util.lookup.ServiceProvider;

@ServiceProvider(service = AnnotationProvider.class, position = 0) 
public class AnahataAnnotationProvider extends AnnotationProvider {

    private static final Logger LOG = Logger.getLogger(AnahataAnnotationProvider.class.getName());
    private static final Image BADGE;

    static {
        LOG.info("AnahataAnnotationProvider static init");
        Image original = ImageUtilities.loadImage("icons/anahata.png");
        if (original != null) {
            BADGE = original.getScaledInstance(8, 8, Image.SCALE_SMOOTH);
        } else {
            BADGE = null;
        }
    }

    public AnahataAnnotationProvider() {
        LOG.info("AnahataAnnotationProvider constructor " + this);
    }
    
    

    @Override
    public Image annotateIcon(Image icon, int type, Set<? extends FileObject> files) {
        LOG.log(Level.INFO, "AnahataAnnotationProvider.annotateIcon called for files size: {0}", files.size());
        //FileBasedFileSystem fbds = org.netbeans.modules.masterfs.filebasedfs.FileBasedFileSystem.getInstance();
        // Start with the original icon, allowing other providers to annotate first
        
        //to get a local history
        //VCSSystemProvider.VersioningSystem localHistory = VersioningManager.getInstance().getLocalHistory(file, !fo.isFolder());
        
        // Add our badge if needed
        for (FileObject fo : files) {            
            /*
            if (FakeContext.contains(FileUtil.toFile(fo))) {
                LOG.log(Level.INFO, "AnahataFileAnnotator: Annotating icon for: {0}", fo.getName());
                if (BADGE != null) {
                    // Use top-right (8, 0) to avoid Git's bottom-right badges
                    return ImageUtilities.mergeImages(icon, BADGE, 16, 0);
                }
            }*/
        }
        return icon; 
    }

    @Override
    public String annotateName(String name, Set<? extends FileObject> files) {
        LOG.log(Level.INFO, "AnahataFileAnnotator.annotateName called for name: {0}", name);
        
        // Start with the original name, allowing other providers to annotate first
        //String name = VersioningAnnotationProvider.getDefault().annotateNameHtml(name, files);
        
        for (FileObject fo : files) {
            /*
            if (FakeContext.contains(FileUtil.toFile(fo))) {
                LOG.log(Level.INFO, "AnahataFileAnnotator: Annotating name for: {0}", fo.getName());
                return name + " [IC]";
            }*/
        }
        return name;
    }

    @Override
    public String annotateNameHtml(String name, Set<? extends FileObject> files) {
        LOG.log(Level.INFO,"AnahataFileAnnotator.annotateNameHtml called for name: {0} files {1}", new Object[]{name, files});
        
        // Start with the original name, allowing other providers to annotate first
        String annotated = VersioningAnnotationProvider.getDefault().annotateNameHtml(name, files);
        
        for (FileObject fo : files) {
            /*
            if (FakeContext.contains(FileUtil.toFile(fo))) {
                String label = " <font color='#008000'>[IC]</font>";
                if (annotated.toLowerCase().contains("<html>")) {
                    if (annotated.toLowerCase().endsWith("</html>")) {
                        return annotated.substring(0, annotated.length() - 7) + label + "</html>";
                    }
                    return annotated + label;
                }
                return "<html>" + annotated + label + "</html>";
            }*/
        }
        return annotated;
    }


    @Override
    public Action[] actions(Set<? extends FileObject> files) {
        LOG.info("------------------actions()----------------------------: " + files);
        List<Action> actions = new ArrayList<>();
        actions.add(new AnahataSubmenuAction(files));
        return actions.toArray(Action[]::new);
    }

    
    @Override
    public Lookup findExtrasFor(Set<? extends FileObject> files) {
        LOG.log(Level.INFO, "AnahataFileAnnotator.findExtrasFor called for files size: {0}", files);
        return super.findExtrasFor(files);
    }
     

    
    @Override
    public InterceptionListener getInterceptionListener() {
        LOG.info("AnahataFileAnnotator.getInterceptionListener called");
        return new InterceptionListener() {

            @Override
            public void beforeDelete(FileObject fo) {
                LOG.info("About to delete: " + fo.getPath());
            }

            @Override
            public void deleteSuccess(FileObject fo) {
                LOG.info("Successfully deleted: " + fo.getPath());
            }

            @Override
            public void deleteFailure(FileObject fo) {
                LOG.warning("Failed to delete: " + fo.getPath());
            }

            @Override public void beforeCreate(FileObject parent, String name, boolean isFolder) {}
            @Override public void createSuccess(FileObject fo) {}
            @Override public void createFailure(FileObject parent, String name, boolean isFolder) {}
        };
    }

    public class AnahataSubmenuAction extends AbstractAction implements Presenter.Popup {

        private Set<? extends FileObject> files;

        public AnahataSubmenuAction(Set<? extends FileObject> files) {
            super("AnahataSubmenuAction", new ImageIcon("icons/anahata.png"));
            this.files = files;
        }

        @Override
        public void actionPerformed(ActionEvent e) {}

        @Override
        public JMenuItem getPopupPresenter() {
            JMenu submenu = new JMenu("Anahata");
            submenu.setIcon(new ImageIcon("icons/anahata.png"));
            
            submenu.add(new JMenuItem(new AbstractAction("Add To AGI Context", submenu.getIcon()) {
                @Override
                public void actionPerformed(ActionEvent e) {
                    LOG.info("Adding: " + files);
                    //FakeContext.addFileObjects(files);
                    try {
                        FileSystem fs = files.iterator().next().getFileSystem();
                        LOG.info("Firing event on file system : " + fs);
                        AnahataAnnotationProvider.this.fireFileStatusChanged(new FileStatusEvent(fs, files, true, true));
                    } catch (Exception ex) {
                        LOG.log(Level.SEVERE, "Exception adding files to fake context ", ex);
                    }
                    
                }
            }));

            submenu.add(new JMenuItem(new AbstractAction("Remove From AGI Context") {
                @Override
                public void actionPerformed(ActionEvent e) {
                    // TODO: Implement removal
                }
            }));

            return submenu;
        }
    }

}