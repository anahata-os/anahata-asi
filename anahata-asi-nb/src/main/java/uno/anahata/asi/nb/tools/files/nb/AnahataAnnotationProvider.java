/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.files.nb;

import java.awt.Image;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import javax.swing.Action;
import javax.swing.SwingUtilities;
import org.netbeans.modules.masterfs.providers.AnnotationProvider;
import org.netbeans.modules.masterfs.providers.InterceptionListener;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStatusEvent;
import org.openide.filesystems.FileSystem;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.chat.Chat;

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
public class AnahataAnnotationProvider extends AnnotationProvider {

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
     */
    public AnahataAnnotationProvider() {
        LOG.info("AnahataAnnotationProvider (v2.7) Master Shell initializing...");
    }

    /**
     * Aggregates icon badges from previous providers and overlays the Anahata bell.
     * <p>
     * Implementation uses a global BUSY guard and manual forensic delegation to 
     * ensure Git badges and other IDE markers are preserved.
     * </p>
     * 
     * {@inheritDoc}
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
            List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
            List<Integer> totals = AnahataAnnotationLogic.calculateSessionTotals(resolved, nodeType, activeChats);
            
            boolean anyInContext = totals.stream().anyMatch(i -> i > 0);
            if (anyInContext) {
                // 5. Construct tooltip and assign to badge pixels.
                String tooltip = AnahataAnnotationLogic.buildTooltip(resolved, nodeType, activeChats, totals);
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
     * Aggregates HTML labels and appends session-specific context counts or nicknames.
     * <p>
     * Implementation uses a global BUSY guard to prevent re-entrant HTML building.
     * </p>
     * 
     * {@inheritDoc}
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
            List<Chat> activeChats = AnahataInstaller.getContainer().getActiveChats();
            List<Integer> totals = AnahataAnnotationLogic.calculateSessionTotals(fo, nodeType, activeChats);
            
            boolean anyInContext = totals.stream().anyMatch(i -> i > 0);
            if (anyInContext) {
                // 3. Dispatch to logic engine for the specific annotation string.
                String annotation = AnahataAnnotationLogic.buildNameAnnotation(nodeType, activeChats, totals);
                
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
     * Safely loops through other providers to aggregate their Icon results.
     * 
     * @param icon The base icon to annotate.
     * @param type Icon type (BeanInfo).
     * @param files The set of target files.
     * @return The aggregated and aggregated image.
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

    /** {@inheritDoc} */
    @Override public String annotateName(String name, Set<? extends FileObject> files) { return null; }
    /** {@inheritDoc} */
    @Override public Action[] actions(Set<? extends FileObject> files) { return new Action[0]; }
    /** {@inheritDoc} */
    @Override public InterceptionListener getInterceptionListener() { return null; }

    /**
     * Fires a refresh event to redraw nodes for specific files across all AnnotationProviders.
     * 
     * @param fs The filesystem of the target files.
     * @param files The set of files requiring a redraw.
     */
    public static void fireRefresh(FileSystem fs, Set<FileObject> files) {
        if (files == null || files.isEmpty() || fs == null) {
            return;
        }
        SwingUtilities.invokeLater(() -> {
            try {
                for (AnnotationProvider ap : Lookup.getDefault().lookupAll(AnnotationProvider.class)) {
                    if (ap instanceof AnahataAnnotationProvider aap) {
                        aap.fireFileStatusChanged(new FileStatusEvent(fs, files, true, true));
                    }
                }
            } catch (Exception ex) {
                // Ignore refresh errors to prevent IDE instability
            }
        });
    }
}
