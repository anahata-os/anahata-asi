/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.files.nb;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
 * the NetBeans API plumbing.
 * </p>
 * 
 * @author anahata
 */
public class AnahataAnnotationLogic {

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
     * Implementation details:
     * Scans the current call stack for specific NetBeans node implementation classes 
     * (PackageNode, BadgingNode) to distinguish between Projects view and Files view.
     * </p>
     * 
     * @param fo The FileObject to classify.
     * @return The classified NodeType.
     */
    public static NodeType classify(FileObject fo) {
        StackTraceElement[] stack = Thread.currentThread().getStackTrace();
        for (StackTraceElement e : stack) {
            String cn = e.getClassName();
            // PackageNode: specific to Java/Projects view
            if (cn.contains("PackageNode")) {
                return NodeType.PACKAGE;
            }
            // BadgingNode: the proven project filter node detection
            if (cn.contains("BadgingNode")) {
                return NodeType.PROJECT;
            }
        }
        // Fallback to directory/file check if stack trace is ambiguous
        return fo.isFolder() ? NodeType.FOLDER : NodeType.FILE;
    }

    /**
     * Resolves DataShadows (shortcuts) to their original physical files.
     * <p>
     * Implementation details:
     * Unwraps {@link DataShadow} instances using the DataObject API. 
     * Ensures context checks are performed on the ground truth resource.
     * </p>
     * 
     * @param fo The potentially virtual FileObject.
     * @return The resolved physical FileObject.
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
     * Calculates the context presence totals for each active session.
     * <p>
     * Implementation details:
     * - Projects: Counts active providers via the Projects toolkit.
     * - Packages: Performs a non-recursive resource count.
     * - Folders: Performs a recursive resource count for tree propagation.
     * </p>
     * 
     * @param fo The file or container.
     * @param nodeType The classified type.
     * @param activeAgis List of active sessions.
     * @return A list of integers matching the activeAgis list indices.
     */
    public static List<Integer> calculateSessionTotals(FileObject fo, NodeType nodeType, List<Agi> activeAgis) {
        List<Integer> totals = new ArrayList<>();
        FileObject res = resolve(fo);
        if (res == null) {
            return totals;
        }
        
        if (nodeType == NodeType.PROJECT) {
            // Projects count 'effectivelyProviding' context providers (flattened hierarchy)
            for (Agi agi : activeAgis) {
                totals.add(getProvidingProviders(agi, res).size());
            }
        } else {
            // Packages: non-recursive sum. Folders: recursive sum.
            boolean recursive = (nodeType == NodeType.FOLDER);
            Map<Agi, Integer> counts = FilesContextActionLogic.getSessionFileCounts(res, recursive);
            for (Agi agi : activeAgis) {
                totals.add(counts.getOrDefault(agi, 0));
            }
        }
        return totals;
    }

    /**
     * Builds the HTML name annotation suffix.
     * <p>
     * Implementation details:
     * Dispatches to specific bracketed or parenthesized formatting based on node type.
     * </p>
     * 
     * @param nodeType The node identity.
     * @param agis List of active agis.
     * @param totals Pre-calculated context counts.
     * @return The HTML snippet to append.
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
     * Implementation details:
     * Injects the Anahata icon and builds a structured list of active sessions 
     * and providers (for projects) or resource counts (for containers).
     * </p>
     * 
     * @param fo The target node's FileObject.
     * @param nodeType The classified identity.
     * @param activeAgis List of active sessions.
     * @param totals Pre-calculated context counts.
     * @return The formatted HTML tooltip.
     */
    public static String buildTooltip(FileObject fo, NodeType nodeType, List<Agi> activeAgis, List<Integer> totals) {
        StringBuilder sb = new StringBuilder();
        
        // Add Anahata icon to the tooltip per spec
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
            // For files we don't show counts, for containers we do.
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
     * Formats context counts into greyed-out square brackets (e.g., [3][1]).
     * <p>
     * Implementation details:
     * Appends a series of bracketed totals styled with a neutral grey color.
     * </p>
     * 
     * @param totals The session-specific counts.
     * @return The bracketed HTML string.
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
     * Implementation details:
     * If in a single session, shows the session nickname. In multiple sessions, 
     * shows the total session count.
     * </p>
     * 
     * @param agis Active agi list.
     * @param totals Session counts.
     * @return The parenthesized HTML string.
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
     * Implementation details:
     * Uses {@link FileOwnerQuery} to find the project root from the node's FileObject. 
     * Queries the toolkit for the flattened hierarchy of 'effectively providing' providers.
     * </p>
     * 
     * @param agi The session to check.
     * @param fo Any FileObject belonging to the project.
     * @return A list of provider names.
     */
    private static List<String> getProvidingProviders(Agi agi, FileObject fo) {
        List<String> names = new ArrayList<>();
        
        // MISSION CRITICAL: Resolve the project owner. In the Logical View, 'fo' is usually 
        // a kid of the root, not the root itself.
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
     * Flattens a provider hierarchy and extracts names of nodes that are actually providing context.
     * <p>
     * Implementation details:
     * Performs a depth-first search of the children providers, checking the 
     * {@link ContextProvider#isEffectivelyProviding()} status.
     * </p>
     * 
     * @param root The starting context provider.
     * @return List of matching names.
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
