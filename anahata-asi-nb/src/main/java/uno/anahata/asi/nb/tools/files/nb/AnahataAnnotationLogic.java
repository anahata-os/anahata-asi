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
import uno.anahata.asi.chat.Chat;
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
     * This logic is identical to the verified Mock version to ensure 100% 
     * reliable detection of Logical View (Project/Package) nodes.
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
     * Resolves DataShadows (shortcuts) to their original files.
     * <p>
     * This ensures that context lookups happen on the physical resource 
     * even if the node being annotated is a virtual shortcut.
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
     * Logic varies by node type:
     * <ul>
     *   <li><b>Projects:</b> Total 'effectively providing' providers.</li>
     *   <li><b>Packages:</b> Sum of resources in that package ONLY (non-recursive).</li>
     *   <li><b>Folders:</b> Recursive resource count for tree visibility.</li>
     * </ul>
     * </p>
     * 
     * @param fo The file or container.
     * @param nodeType The classified type.
     * @param activeChats List of active sessions.
     * @return A list of integers matching the activeChats list indices.
     */
    public static List<Integer> calculateSessionTotals(FileObject fo, NodeType nodeType, List<Chat> activeChats) {
        List<Integer> totals = new ArrayList<>();
        FileObject res = resolve(fo);
        if (res == null) {
            return totals;
        }
        
        if (nodeType == NodeType.PROJECT) {
            // Projects count 'effectivelyProviding' context providers (flattened hierarchy)
            for (Chat chat : activeChats) {
                totals.add(getProvidingProviders(chat, res).size());
            }
        } else {
            // Packages: non-recursive sum. Folders: recursive sum.
            boolean recursive = (nodeType == NodeType.FOLDER);
            Map<Chat, Integer> counts = FilesContextActionLogic.getSessionFileCounts(res, recursive);
            for (Chat chat : activeChats) {
                totals.add(counts.getOrDefault(chat, 0));
            }
        }
        return totals;
    }

    /**
     * Builds the HTML name annotation suffix.
     * <p>
     * Format per spec:
     * <ul>
     *   <li><b>Containers:</b> [sum1][sum2]...</li>
     *   <li><b>Files:</b> (displayName) or (count)</li>
     * </ul>
     * </p>
     * 
     * @param nodeType The node identity.
     * @param chats List of active chats.
     * @param totals Pre-calculated context counts.
     * @return The HTML snippet to append.
     */
    public static String buildNameAnnotation(NodeType nodeType, List<Chat> chats, List<Integer> totals) {
        return switch (nodeType) {
            case PROJECT, PACKAGE, FOLDER -> buildBracketedTotal(totals);
            case FILE -> buildFileAnnotation(chats, totals);
        };
    }

    /**
     * Builds the descriptive HTML tooltip for the Anahata badge.
     * 
     * @param fo The target node's FileObject.
     * @param nodeType The classified identity.
     * @param activeChats List of active sessions.
     * @param totals Pre-calculated context counts.
     * @return The formatted HTML tooltip.
     */
    public static String buildTooltip(FileObject fo, NodeType nodeType, List<Chat> activeChats, List<Integer> totals) {
        StringBuilder sb = new StringBuilder();
        
        // Add Anahata icon to the tooltip per spec
        sb.append("<img src=\"").append(AnahataAnnotationLogic.class.getResource("/icons/anahata_16.png")).append("\" width=\"16\" height=\"16\"> ");
        sb.append("<b>In context in:</b><br>");
        
        if (nodeType == NodeType.PROJECT) {
            for (Chat chat : activeChats) {
                List<String> providers = getProvidingProviders(chat, fo);
                if (!providers.isEmpty()) {
                    sb.append("&nbsp;&nbsp;&bull;&nbsp;<b>").append(chat.getDisplayName()).append("</b><br>");
                    sb.append("&nbsp;&nbsp;&nbsp;&nbsp;Providers: ").append(String.join(", ", providers)).append("<br>");
                }
            }
        } else {
            // For files we don't show counts, for containers we do.
            boolean showCount = (nodeType != NodeType.FILE);
            for (int i = 0; i < activeChats.size(); i++) {
                if (totals.get(i) > 0) {
                    sb.append("&nbsp;&nbsp;&bull;&nbsp;").append(activeChats.get(i).getDisplayName());
                    if (showCount) {
                        sb.append(": ").append(totals.get(i)).append(" resources");
                    }
                    sb.append("<br>");
                }
            }
        }
        return sb.toString();
    }

    // --- Private Detail Helpers ---

    /**
     * Formats context counts into greyed-out square brackets (e.g., [3][1]).
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
     * Formats file name annotations according to the spec: (displayName) or (count).
     * 
     * @param chats Active chat list.
     * @param totals Session counts.
     * @return The parenthesized HTML string.
     */
    private static String buildFileAnnotation(List<Chat> chats, List<Integer> totals) {
        StringBuilder sb = new StringBuilder(" <font color='#707070'>");
        long sessionsCount = totals.stream().filter(i -> i > 0).count();
        if (sessionsCount == 1) {
            for (int i = 0; i < totals.size(); i++) {
                if (totals.get(i) > 0) {
                    sb.append("(").append(chats.get(i).getDisplayName()).append(")");
                    break;
                }
            }
        } else if (sessionsCount > 1) {
            sb.append("(").append(sessionsCount).append(")");
        }
        return sb.append("</font>").toString();
    }

    /**
     * Locates the names of all providers for a project that are currently 'effectively providing'.
     * <p>
     * Implementation uses FileOwnerQuery to resolve the project root from any FileObject 
     * (crucial for Logical View nodes where the set of files often contains children of the root).
     * </p>
     * 
     * @param chat The session to check.
     * @param fo Any FileObject belonging to the project.
     * @return A list of provider names.
     */
    private static List<String> getProvidingProviders(Chat chat, FileObject fo) {
        List<String> names = new ArrayList<>();
        
        // MISSION CRITICAL: Resolve the project owner. In the Logical View, 'fo' is usually 
        // a kid of the root, not the root itself.
        Project p = FileOwnerQuery.getOwner(fo);
        if (p == null) {
            return names;
        }

        String path = Projects.getCanonicalPath(p.getProjectDirectory());
        chat.getToolManager().getToolkitInstance(Projects.class).ifPresent(tool -> {
            tool.getProjectProvider(path).ifPresent(pcp -> {
                names.addAll(flattenProvidingNames(pcp));
            });
        });
        return names;
    }

    /**
     * Flattens a provider hierarchy and extracts names of effectively-providing nodes.
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
