/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.api.project.FileOwnerQuery;
import org.netbeans.api.project.Project;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataShadow;
import uno.anahata.asi.AnahataInstaller;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * The decoupled logic engine for Anahata NetBeans annotations.
 * <p>
 * This class contains the 'Brain' of the annotation system, separating the 
 * business logic (calculating totals, building HTML, classifying nodes) from 
 * the NetBeans API plumbing. It is designed for high performance and includes 
 * instrumentation to monitor the impact on the IDE's repaint cycle.
 * </p>
 * 
 * @author anahata
 */
public class AnahataAnnotationLogic {

    /** Logger for performance tracking and classification diagnostics. */
    private static final Logger LOG = Logger.getLogger(AnahataAnnotationLogic.class.getName());

    /** 
     * Semantic classification for UI node rendering. 
     */
    public enum NodeType { 
        /** 
         * Root project node in Projects tab or project directory in Files tab. 
         */
        PROJECT, 
        /** 
         * Java package node in Projects tab. 
         */
        PACKAGE, 
        /** 
         * Standard OS folder in Files/Favorites or virtual folder in Projects. 
         */
        FOLDER, 
        /** 
         * Standard file. 
         */
        FILE 
    }

    /**
     * Performs stack-trace forensics to classify the UI node identity.
     * <p>
     * Scans the current call stack for specific NetBeans node implementation 
     * classes (PackageNode, BadgingNode) to distinguish between Projects view 
     * and Files view. This allows the system to apply different counting 
     * strategies (recursive vs. non-recursive) based on visual context.
     * </p>
     * 
     * @param fo The FileObject to classify.
     * @return The classified NodeType.
     */
    public static NodeType classify(FileObject fo) {
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        for (StackTraceElement e : stack) {
            String cn = e.getClassName();
            if (cn.contains("PackageNode")) {
                return NodeType.PACKAGE;
            }
            if (cn.contains("BadgingNode")) {
                return NodeType.PROJECT;
            }
        }
        return fo.isFolder() ? NodeType.FOLDER : NodeType.FILE;
    }

    /**
     * Resolves DataShadows (shortcuts) to their original physical files.
     * <p>
     * Unwraps {@link DataShadow} instances using the DataObject API. This 
     * ensures that context checks are performed on the ground-truth resource 
     * rather than the IDE proxy.
     * </p>
     * 
     * @param fo The potentially virtual FileObject.
     * @return The resolved physical FileObject, or the original if not a shadow.
     */
    public static FileObject resolve(FileObject fo) {
        if (fo == null) {
            return null;
        }
        try {
            DataObject dobj = DataObject.find(fo);
            if (dobj instanceof DataShadow ds) {
                return ds.getOriginal().getPrimaryFile();
            }
        } catch (Exception e) {
            // Ignore resolution errors, fallback to original
        }
        return fo;
    }

    /**
     * Calculates the context presence totals for each active session with high-resolution timing.
     * <p>
     * For projects, this method counts 'effectivelyProviding' context providers 
     * (e.g., Overview, Alerts). For packages, it performs a non-recursive 
     * resource count, while OS folders trigger a recursive search to propagate 
     * status badges.
     * </p>
     * <p>
     * This method is instrumented with System.nanoTime(). If the calculation for 
     * a single node exceeds 2ms, a performance warning is logged at the INFO level.
     * </p>
     * 
     * @param fo The file or container to count.
     * @param nodeType The classified type of the node.
     * @param activeAgis The list of active agi sessions.
     * @return A list of integers representing resources in context per session.
     */
    public static List<Integer> calculateSessionTotals(FileObject fo, NodeType nodeType, List<Agi> activeAgis) {
        long start = System.nanoTime();
        List<Integer> totals = new ArrayList<>();
        FileObject res = resolve(fo);
        
        if (res == null) {
            return totals;
        }
        
        if (nodeType == NodeType.PROJECT) {
            for (Agi agi : activeAgis) {
                totals.add(getProvidingProviders(agi, res).size());
            }
        } else {
            boolean recursive = (nodeType == NodeType.FOLDER);
            Map<Agi, Integer> counts = FilesContextActionLogic.getSessionFileCounts(res, recursive);
            for (Agi agi : activeAgis) {
                totals.add(counts.getOrDefault(agi, 0));
            }
        }
        
        long durationNs = System.nanoTime() - start;
        if (durationNs > 2_000_000) { // 2ms threshold
            LOG.log(Level.INFO, "Slow annotation calculation: {0}ms for {1} (Type: {2})", 
                    new Object[]{durationNs / 1_000_000.0, fo.getPath(), nodeType});
        }
        
        return totals;
    }

    /**
     * Builds the HTML name annotation suffix.
     * <p>
     * Dispatches to specific bracketed or parenthesized formatting based on 
     * node type. Directories use square brackets [count], while individual 
     * files use parentheses (nickname).
     * </p>
     * 
     * @param nodeType The node identity.
     * @param agis List of active agis.
     * @param totals Pre-calculated context counts.
     * @return The HTML snippet to append to the node name.
     */
    public static String buildNameAnnotation(NodeType nodeType, List<Agi> agis, List<Integer> totals) {
        return switch (nodeType) {
            case PROJECT, PACKAGE, FOLDER -> buildBracketedTotal(totals);
            case FILE -> buildFileAnnotation(agis, totals);
        };
    }

    /**
     * Builds the descriptive HTML tooltip for the Anahata badge.
     * <p>
     * Injects the Anahata icon and builds a structured list of active sessions 
     * and their respective contributions (providers for projects, resource 
     * counts for folders).
     * </p>
     * 
     * @param fo The target node's FileObject.
     * @param nodeType The classified identity.
     * @param activeAgis List of active sessions.
     * @param totals Pre-calculated context counts.
     * @return The formatted HTML tooltip string.
     */
    public static String buildTooltip(FileObject fo, NodeType nodeType, List<Agi> activeAgis, List<Integer> totals) {
        StringBuilder sb = new StringBuilder();
        
        sb.append("<img src=\"").append(AnahataAnnotationLogic.class.getResource("/icons/anahata_16.png")).append("\" width=\"16\" height=\"16\"> ");
        sb.append("<b>In context in:</b><br>");
        
        if (nodeType == NodeType.PROJECT) {
            for (Agi agi : activeAgis) {
                List<String> providers = getProvidingProviders(agi, fo);
                if (!providers.isEmpty()) {
                    sb.append("&nbsp;&nbsp;&bull;&nbsp;<b>").append(agi.getDisplayName()).append("</b><br>");
                    sb.append("&nbsp;&nbsp;&nbsp;&nbsp;Providers: ").append(String.join(", ", providers)).append("<br>");
                }
            }
        } else {
            boolean showCount = (nodeType != NodeType.FILE);
            for (int i = 0; i < activeAgis.size(); i++) {
                if (totals.get(i) > 0) {
                    sb.append("&nbsp;&nbsp;&bull;&nbsp;").append(activeAgis.get(i).getDisplayName());
                    if (showCount) {
                        sb.append(": ").append(totals.get(i)).append(" resources");
                    }
                    sb.append("<br>");
                }
            }
        }
        return sb.toString();
    }

    /**
     * Formats context counts into greyed-out square brackets.
     * <p>
     * Appends a series of bracketed totals (e.g., [3][1]) styled with a 
     * neutral grey color to minimize visual noise.
     * </p>
     * 
     * @param totals The session-specific counts.
     * @return The formatted bracketed HTML string.
     */
    private static String buildBracketedTotal(List<Integer> totals) {
        StringBuilder sb = new StringBuilder(" <font color='#707070'>");
        for (Integer sum : totals) {
            if (sum > 0) {
                sb.append("[").append(sum).append("]");
            }
        }
        return sb.append("</font>").toString();
    }

    /**
     * Formats file name annotations using parentheses.
     * <p>
     * If the file is in a single session, it shows the session's display name. 
     * If in multiple sessions, it shows the total session count.
     * </p>
     * 
     * @param agis Active agi list.
     * @param totals Session counts.
     * @return The formatted parenthesized HTML string.
     */
    private static String buildFileAnnotation(List<Agi> agis, List<Integer> totals) {
        StringBuilder sb = new StringBuilder(" <font color='#707070'>");
        long sessionsCount = totals.stream().filter(i -> i > 0).count();
        if (sessionsCount == 1) {
            for (int i = 0; i < totals.size(); i++) {
                if (totals.get(i) > 0) {
                    sb.append("(").append(agis.get(i).getDisplayName()).append(")");
                    break;
                }
            }
        } else if (sessionsCount > 1) {
            sb.append("(").append(sessionsCount).append(")");
        }
        return sb.append("</font>").toString();
    }

    /**
     * Locates the names of all active providers for a project.
     * <p>
     * Uses {@link FileOwnerQuery} to find the project root and queries the 
     * Projects toolkit for the flattened hierarchy of 'effectively providing' 
     * providers.
     * </p>
     * 
     * @param agi The session to check.
     * @param fo Any FileObject belonging to the project.
     * @return A list of names for all active providers.
     */
    private static List<String> getProvidingProviders(Agi agi, FileObject fo) {
        List<String> names = new ArrayList<>();
        Project p = FileOwnerQuery.getOwner(fo);
        if (p == null) {
            return names;
        }

        String path = Projects.getCanonicalPath(p.getProjectDirectory());
        agi.getToolManager().getToolkitInstance(Projects.class).ifPresent(tool -> {
            tool.getProjectProvider(path).ifPresent(pcp -> {
                names.addAll(flattenProvidingNames(pcp));
            });
        });
        return names;
    }

    /**
     * Flattens a provider hierarchy and extracts names of active nodes.
     * <p>
     * Performs a depth-first search of the children providers, checking the 
     * {@link ContextProvider#isEffectivelyProviding()} status for each node.
     * </p>
     * 
     * @param root The starting context provider.
     * @return List of matching provider names.
     */
    private static List<String> flattenProvidingNames(ContextProvider root) {
        List<String> list = new ArrayList<>();
        if (root.isEffectivelyProviding()) {
            list.add(root.getName());
        }
        for (ContextProvider child : root.getChildrenProviders()) {
            list.addAll(flattenProvidingNames(child));
        }
        return list;
    }
}
