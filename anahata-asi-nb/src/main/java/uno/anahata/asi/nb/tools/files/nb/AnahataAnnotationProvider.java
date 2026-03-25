/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Action;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.modules.masterfs.providers.AnnotationProvider;
import org.netbeans.modules.masterfs.providers.InterceptionListener;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStatusEvent;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.nb.AnahataInstaller;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Master Shell for Anahata context visibility in NetBeans.
 * <p>
 * This class is the IDE-facing 'Shell' that implements the AnnotationProvider API. 
 * It manages the high-salience NetBeans plumbing:
 * 1. <b>Global Re-entry Guard (BUSY):</b> Prevents recursive annotation loops during delegation.
 * 2. <b>Manual Delegation Chain:</b> Aggregates icons and names from other providers (Git, etc.) 
 *    before overlaying Anahata's session indicators.
 * 3. <b>Position 10000:</b> Ensures Anahata is the final aggregator in the chain.
 * </p>
 * <p>
 * All business logic (classification, resource counting, label building) is 
 * delegated to {@link AnahataAnnotationLogic}.
 * </p>
 * 
 * @author anahata
 */
@ServiceProvider(service = AnnotationProvider.class, position = 10000)
@Slf4j
public class AnahataAnnotationProvider extends AnnotationProvider {

    /** Logger for tracking annotation lifecycle and delegation issues. */
    private static final Logger LOG = Logger.getLogger(AnahataAnnotationProvider.class.getName());
    
    /** 
     * Global guard to prevent re-entry loops during the manual delegation chain. 
     * This is verified to prevent IDE 'flicker' and node lookup failures.
     */
    private static final ThreadLocal<Boolean> BUSY = ThreadLocal.withInitial(() -> false);
    
    /** 
     * ThreadLocal guard to prevent recursion within the icon delegation logic. 
     */
    private static final ThreadLocal<Boolean> DELEGATING_ICON = ThreadLocal.withInitial(() -> false);

    /** 
     * ThreadLocal guard to prevent recursion within the HTML delegation logic. 
     */
    private static final ThreadLocal<Boolean> DELEGATING_NAME = ThreadLocal.withInitial(() -> false);
    
    /** 
     * The scaled Anahata context badge (bell icon). 
     */
    private static final Image BADGE;

    static {
        // Load the branding icon from resources
        Image img = ImageUtilities.loadImage("icons/anahata_16.png");
        // Scaled to 8x8 for standard IDE badging proportions
        BADGE = (img != null) ? img.getScaledInstance(8, 8, Image.SCALE_SMOOTH) : null;
    }

    /**
     * Default constructor for the service loader.
     * Initializes the provider and logs the startup message.
     */
    public AnahataAnnotationProvider() {
        LOG.info("AnahataAnnotationProvider (v3.0) Master Shell initializing...");
    }

    /**
     * Annotates the icon of a file or set of files.
     * <p>
     * Implementation details:
     * 1. Preserves existing badges (Git, Errors) by delegating to other providers.
     * 2. Uses {@link AnahataAnnotationLogic} to classify the node and calculate session totals.
     * 3. Overlays the Anahata bell icon if any of the files are in an active AI context.
     * 4. Injects a dynamically built tooltip into the merged image.
     * </p>
     * 
     * @param icon The base icon to annotate.
     * @param type The type of icon (e.g., closed/opened folder).
     * @param files The set of files represented by the node.
     * @return The badged icon, or the original if no context is present.
     */
    @Override
    public Image annotateIcon(Image icon, int type, Set<? extends FileObject> files) {
        if (BUSY.get() || files == null || files.isEmpty() || BADGE == null) {
            return icon;
        }

        BUSY.set(true);
        try {
            // 1. Accumulate previous badges (Git, Errors) via manual delegation.
            Image baseIcon = delegateIcon(icon, type, files);
            if (baseIcon == null) {
                baseIcon = icon;
            }

            // 2. Classify Node Identity using the logic engine.
            FileObject fo = files.iterator().next();
            AnahataAnnotationLogic.NodeType nodeType = AnahataAnnotationLogic.classify(fo);
            
            // 3. Resolve actual physical resource (unmasking shortcuts/shadows).
            FileObject resolved = AnahataAnnotationLogic.resolve(fo);
            if (resolved == null) {
                return baseIcon;
            }

            // 4. Calculate Context Presence across active sessions.
            List<Agi> activeAgis = AnahataInstaller.getContainer().getActiveAgis();
            List<Integer> totals = AnahataAnnotationLogic.calculateSessionTotals(resolved, nodeType, activeAgis);
            
            boolean anyInContext = totals.stream().anyMatch(i -> i > 0);
            if (anyInContext) {
                // 5. Construct tooltip and assign to badge pixels.
                String tooltip = AnahataAnnotationLogic.buildTooltip(resolved, nodeType, activeAgis, totals);
                Image taggedBadge = ImageUtilities.assignToolTipToImage(BADGE, tooltip);
                
                // 6. Layering: Using (16, 0) right-aligned anchor per spec.
                return ImageUtilities.mergeImages(baseIcon, taggedBadge, 16, 0);
            }
            
            return baseIcon;
        } finally {
            BUSY.set(false);
        }
    }

    /**
     * Annotates the HTML name of a file or set of files.
     * <p>
     * Implementation details:
     * 1. Preserves existing HTML decorations (Git branch, status colors) via delegation.
     * 2. Uses {@link AnahataAnnotationLogic} to determine the specific annotation suffix (bracketed counts or session names).
     * 3. Surgically injects the suffix before the closing &lt;/html&gt; tag.
     * </p>
     * 
     * @param name The original HTML name (may be null).
     * @param files The set of files represented by the node.
     * @return The annotated HTML string.
     */
    @Override
    public String annotateNameHtml(String name, Set<? extends FileObject> files) {
        if (BUSY.get() || files == null || files.isEmpty()) {
            return null;
        }

        BUSY.set(true);
        try {
            // 1. Delegate to pick up Git [branch] and status colors.
            String baseHtml = delegateNameHtml(name, files);
            String currentHtml = (baseHtml != null) ? baseHtml : (name != null ? name : "");

            // 2. Classify node and calculate totals via logic engine.
            FileObject fo = files.iterator().next();
            AnahataAnnotationLogic.NodeType nodeType = AnahataAnnotationLogic.classify(fo);
            List<Agi> activeAgis = AnahataInstaller.getContainer().getActiveAgis();
            List<Integer> totals = AnahataAnnotationLogic.calculateSessionTotals(fo, nodeType, activeAgis);
            
            boolean anyInContext = totals.stream().anyMatch(i -> i > 0);
            if (anyInContext) {
                // 3. Dispatch to logic engine for the specific annotation string.
                String annotation = AnahataAnnotationLogic.buildNameAnnotation(nodeType, activeAgis, totals);
                
                // 4. Surgical injection before the closing </html> tag.
                if (currentHtml.toLowerCase().contains("<html>")) {
                    return currentHtml.replaceFirst("(?i)</html>", annotation + "</html>");
                } else {
                    return "<html>" + currentHtml + annotation + "</html>";
                }
            }
            
            return baseHtml;
        } finally {
            BUSY.set(false);
        }
    }

    /**
     * Provides dynamic context menu actions for the selected files.
     * <p>
     * Implementation details:
     * Returns a single {@link AnahataContextActionPresenter} which generates the dynamic 
     * "Add/Remove from AI Context" submenus. This allows all file types to be managed 
     * without specific MIME-type registrations.
     * </p>
     * 
     * @param files The set of selected files.
     * @return An array of actions to add to the context menu.
     */
    @Override
    public Action[] actions(Set<? extends FileObject> files) {
        if (files == null || files.isEmpty()) {
            return new Action[0];
        }
        return new Action[]{ new AnahataContextActionPresenter(files) };
    }

    /**
     * Safely loops through other providers to aggregate their Icon results.
     * <p>
     * Implementation details:
     * Prevents infinite recursion by checking the {@link #DELEGATING_ICON} guard.
     * Iterates through all {@link AnnotationProvider} services except this one.
     * </p>
     * 
     * @param icon The base icon to annotate.
     * @param type Icon type (BeanInfo).
     * @param files The set of target files.
     * @return The aggregated and badged image.
     */
    private Image delegateIcon(Image icon, int type, Set<? extends FileObject> files) {
        if (DELEGATING_ICON.get()) {
            return null;
        }
        DELEGATING_ICON.set(true);
        try {
            Image current = null;
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap == this) {
                    continue;
                }
                Image res = ap.annotateIcon(current != null ? current : icon, type, files);
                if (res != null) {
                    current = res;
                }
            }
            return current;
        } finally {
            DELEGATING_ICON.set(false);
        }
    }

    /**
     * Safely loops through other providers to aggregate their HTML results.
     * <p>
     * Implementation details:
     * Prevents infinite recursion by checking the {@link #DELEGATING_NAME} guard.
     * Iterates through all {@link AnnotationProvider} services except this one.
     * </p>
     * 
     * @param name The current HTML name.
     * @param files The set of target files.
     * @return The aggregated HTML string.
     */
    private String delegateNameHtml(String name, Set<? extends FileObject> files) {
        if (DELEGATING_NAME.get()) {
            return null;
        }
        DELEGATING_NAME.set(true);
        try {
            String current = null;
            for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                if (ap == this) {
                    continue;
                }
                String res = ap.annotateNameHtml(current != null ? current : name, files);
                if (res != null) {
                    current = res;
                }
            }
            return current;
        } finally {
            DELEGATING_NAME.set(false);
        }
    }

    /** 
     * {@inheritDoc}
     * <p>Implementation details: Not used. Anahata uses HTML-based name annotation 
     * for better styling and visibility of session context.</p>
     * 
     * @param name Original name.
     * @param files Target files.
     * @return null.
     */
    @Override public String annotateName(String name, Set<? extends FileObject> files) { return null; }
    
    /** 
     * {@inheritDoc}
     * <p>Implementation details: Not used by the Anahata provider as it delegates 
     * directly to standard NetBeans annotation hooks.</p>
     * @return null.
     */
    @Override public InterceptionListener getInterceptionListener() { return null; }

    /**
     * Fires a refresh event to redraw nodes for specific files across all AnnotationProviders.
     * <p>
     * Implementation details: This method triggers a broad IDE status change to 
     * force a repaint of project views. It uses {@link SwingUtilities#invokeLater} 
     * to ensure the refresh happens in the <b>next</b> event cycle.
     * </p>
     * <p>
     * <b>Multi-FileSystem Support:</b> If no specific filesystem is provided, the 
     * method gathers all filesystems currently involved in active projects and 
     * AI resources and broadcasts a broad invalidation pulse (using null set) 
     * to all of them. This is the authoritative way to handle nickname updates 
     * and resource removals.
     * </p>
     * 
     * @param fs The filesystem of the target files (can be null for global pulse).
     * @param files The set of files requiring a redraw (can be null for global pulse).
     */
    public static void fireRefresh(FileSystem fs, Set<FileObject> files) {
        log.info("fireRefresh pulse requested (Outer Thread: {})", Thread.currentThread().getName());
        
        SwingUtilities.invokeLater(() -> {
            log.info("Executing async IDE annotation refresh (EDT Triggered)");
            try {
                Set<FileSystem> fss = new HashSet<>();
                
                if (fs != null) {
                    fss.add(fs);
                } else {
                    // BROADCAST MODE: Gather all relevant filesystems
                    // 1. All open projects (ensures nicknames update even in empty sessions)
                    for (Project p : OpenProjects.getDefault().getOpenProjects()) {
                        try {
                            fss.add(p.getProjectDirectory().getFileSystem());
                        } catch (Exception ex) {
                            log.warn("Failed to resolve filesystem for project: {}", p.getProjectDirectory().getName(), ex);
                        }
                    }
                    // 2. All filesystems containing active AI resources (covers JARs, etc.)
                    for (Agi agi : AnahataInstaller.getContainer().getActiveAgis()) {
                        for (Resource res : agi.getResourceManager().getResourcesList()) {
                            if (res.getHandle() instanceof NbHandle nbh) {
                                FileObject fo = nbh.getFileObject();
                                if (fo != null && fo.isValid()) {
                                    try {
                                        fss.add(fo.getFileSystem());
                                    } catch (Exception ex) {
                                        log.warn("Failed to resolve filesystem for resource: {}", res.getName(), ex);
                                    }
                                }
                            }
                        }
                    }
                    // 3. Always include the ConfigRoot just in case
                    try {
                        fss.add(FileUtil.getConfigRoot().getFileSystem());
                    } catch (Exception ex) {
                        log.error("Critical failure: Could not resolve IDE ConfigRoot filesystem.", ex);
                    }
                }

                for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                    if (ap instanceof AnahataAnnotationProvider aap) {
                        for (FileSystem targetFs : fss) {
                            if (fs == null && (files == null || files.isEmpty())) {
                                // Broad Pulse (null set = "status of all files might be changed")
                                // This is mandatory for nickname updates and resource removals.
                                aap.fireFileStatusChanged(new FileStatusEvent(targetFs, (Set<FileObject>) null, true, true));
                            } else {
                                // Targeted Pulse for specific files
                                aap.fireFileStatusChanged(new FileStatusEvent(targetFs, files, true, true));
                            }
                        }
                    }
                }
                log.info("IDE annotation refresh sequence complete for {} filesystems.", fss.size());
            } catch (Exception ex) {
                log.error("Fatal error during IDE annotation refresh pulse.", ex);
            }
        });
    }
}
