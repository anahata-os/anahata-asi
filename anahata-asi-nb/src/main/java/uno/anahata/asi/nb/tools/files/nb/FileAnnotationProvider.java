/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Action;
import org.netbeans.modules.masterfs.providers.AnnotationProvider;
import org.netbeans.modules.masterfs.providers.InterceptionListener;
import org.netbeans.modules.versioning.core.VersioningAnnotationProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStatusEvent;
import org.openide.filesystems.FileSystem;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;

/**
 * Provides file-level annotations (icons and names) in the NetBeans Projects tab.
 * It adds an [IC] label and a badge to files that are currently in an active AI context.
 * <p>
 * This provider delegates context status checks to {@link ContextActionLogic}.
 * </p>
 * 
 * @author anahata
 */
@ServiceProvider(service = AnnotationProvider.class, position = 0) 
public class FileAnnotationProvider extends AnnotationProvider {

    private static final Logger LOG = Logger.getLogger(FileAnnotationProvider.class.getName());
    
    /** The badge image to overlay on icons. */
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

    /**
     * Default constructor.
     */
    public FileAnnotationProvider() {
        LOG.info("AnahataAnnotationProvider instance created.");
    }

    /**
     * {@inheritDoc}
     * Annotates the icon with an Anahata badge if the file is in any active context.
     */
    @Override
    public Image annotateIcon(Image icon, int type, Set<? extends FileObject> files) {
        for (FileObject fo : files) {            
            if (isFileInAnyContext(fo)) {
                if (BADGE != null) {
                    // Use top-right (8, 0) to avoid clashing with Git's bottom-right badges
                    return ImageUtilities.mergeImages(icon, BADGE, 8, 0);
                }
            }
        }
        return icon; 
    }

    /**
     * {@inheritDoc}
     * Appends an [IC] suffix to the file name if it is in any active context.
     */
    @Override
    public String annotateName(String name, Set<? extends FileObject> files) {
        for (FileObject fo : files) {
            if (isFileInAnyContext(fo)) {
                return name + " [IC]";
            }
        }
        return name;
    }

    /**
     * {@inheritDoc}
     * Provides HTML-formatted name annotations, preserving existing versioning annotations.
     */
    @Override
    public String annotateNameHtml(String name, Set<? extends FileObject> files) {
        // Start with the original name, allowing other providers to annotate first
        String annotated = VersioningAnnotationProvider.getDefault().annotateNameHtml(name, files);
        
        for (FileObject fo : files) {
            if (isFileInAnyContext(fo)) {
                String label = " <font color='#008000'>[IC]</font>";
                if (annotated.toLowerCase().contains("<html>")) {
                    if (annotated.toLowerCase().endsWith("</html>")) {
                        return annotated.substring(0, annotated.length() - 7) + label + "</html>";
                    }
                    return annotated + label;
                }
                return "<html>" + annotated + label + "</html>";
            }
        }
        return annotated;
    }

    /**
     * Checks if the given file is currently active in any AI chat context.
     * 
     * @param fo The file object to check.
     * @return {@code true} if the file is in context, {@code false} otherwise.
     */
    private boolean isFileInAnyContext(FileObject fo) {
        for (Chat chat : AnahataInstaller.getContainer().getActiveChats()) {
            if (ContextActionLogic.isAnyInContext(fo, chat)) {
                return true;
            }
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public Action[] actions(Set<? extends FileObject> files) {
        // We use dedicated context-aware actions instead of this.
        return new Action[0];
    }

    /** {@inheritDoc} */
    @Override
    public Lookup findExtrasFor(Set<? extends FileObject> files) {
        return super.findExtrasFor(files);
    }

    /** {@inheritDoc} */
    @Override
    public InterceptionListener getInterceptionListener() {
        return null;
    }
    
    /**
     * Triggers a refresh of the file annotations by firing a FileStatusEvent.
     * 
     * @param files The set of files to refresh.
     */
    public static void fireRefresh(Set<FileObject> files) {
        if (files == null || files.isEmpty()) {
            return;
        }
        try {
            FileSystem fs = files.iterator().next().getFileSystem();
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap instanceof FileAnnotationProvider aap) {
                    aap.fireFileStatusChanged(new FileStatusEvent(fs, files, true, true));
                }
            }
        } catch (Exception ex) {
            LOG.log(Level.WARNING, "Failed to fire file status change for refresh", ex);
        }
    }
}
