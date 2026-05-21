package uno.anahata.asi.toolkit.java.classpath;

import java.io.File;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.Getter;

/**
 * A token-efficient classpath printer that encapsulates both raw and pretty versions.
 * <p>
 * It manages its own list of {@link JarHandler}s to recover metadata from within the JARs,
 * allowing for a rich, environment-agnostic representation of the classpath.
 * </p>
 * <p>
 * This class follows a reactive pattern: calling {@link #setRaw(String)} automatically 
 * triggers a re-build of the pretty-printed representation via the {@link #update()} method.
 * </p>
 * 
 * @author anahata
 */
public class VeryPrettyClassPathPrinter {
    
    /**
     * The original, unformatted classpath string as provided by the system.
     */
    @Getter
    private String raw;
    
    /**
     * The formatted, token-optimized version of the classpath.
     * This is the version intended for AI context consumption.
     */
    @Getter
    private String pretty;
    
    /**
     * The list of specialized handlers used to extract metadata (IDs, versions, vendors) 
     * from within the JARs.
     */
    @Getter
    private final List<JarHandler> handlers = new ArrayList<>();

    /**
     * Constructs a new printer with default handlers (Maven, Selenium, Default).
     */
    public VeryPrettyClassPathPrinter() {
        // Initialize default handlers
        this.handlers.add(new MavenJarHandler());
        this.handlers.add(new SeleniumJarHandler());
        this.handlers.add(new DefaultJarHandler());
    }

    /**
     * Sets the raw classpath string. If the new value differs from the current one, 
     * it automatically triggers an {@link #update()}.
     * 
     * @param raw The new raw classpath string.
     */
    public void setRaw(String raw) {
        if (!Objects.equals(this.raw, raw)) {
            this.raw = raw;
            update();
        }
    }

    /**
     * Adds a new JAR handler to the printer. The new handler is added at the 
     * beginning of the list to give it priority over existing handlers.
     * <p>
     * Note: You should call {@link #update()} after adding a handler if you 
     * want the changes to be reflected immediately in the {@link #pretty} string.
     * </p>
     * 
     * @param handler The handler to add.
     */
    public void addHandler(JarHandler handler) {
        this.handlers.add(0, handler);
    }

    /**
     * Re-builds the pretty-printed string using the current set of handlers and 
     * the current raw classpath.
     * <p>
     * This method builds a collapsed directory tree and then groups JARs by 
     * lexical prefix within each node to minimize token usage.
     * </p>
     * 
     * @return The updated pretty string.
     */
    public String update() {
        if (this.raw == null || this.raw.isEmpty()) {
            this.pretty = "Classpath is empty.";
            return this.pretty;
        }
        
        String[] entries = this.raw.split(File.pathSeparator);
        Map<String, List<JarMetadata>> dirMap = new TreeMap<>();
        
        for (String entry : entries) {
            File f = new File(entry);
            if (!f.exists() || !f.getName().endsWith(".jar")) {
                continue;
            }
            
            String dir = f.getParent();
            JarMetadata meta = inspect(f);
            if (meta != null) {
                dirMap.computeIfAbsent(dir, (String k) -> new ArrayList<>()).add(meta);
            }
        }

        String commonRoot = findCommonRoot(dirMap.keySet());
        
        DirectoryNode rootNode = new DirectoryNode("");
        for (Map.Entry<String, List<JarMetadata>> entry : dirMap.entrySet()) {
            String path = entry.getKey();
            if (commonRoot != null && path.startsWith(commonRoot)) {
                path = path.substring(commonRoot.length());
            }
            
            DirectoryNode current = rootNode;
            if (!path.isEmpty()) {
                String[] segments = path.split(Pattern.quote(File.separator));
                for (String segment : segments) {
                    if (segment.isEmpty()) continue;
                    current = current.children.computeIfAbsent(segment, DirectoryNode::new);
                }
            }
            current.jars.addAll(entry.getValue());
        }

        collapse(rootNode);

        StringBuilder sb = new StringBuilder();
        if (commonRoot != null && !commonRoot.isEmpty()) {
            sb.append("ROOT: ").append(commonRoot).append("\n\n");
        }
        
        printNode(rootNode, "", sb);
        
        this.pretty = sb.toString().trim();
        return this.pretty;
    }

    private void collapse(DirectoryNode node) {
        List<String> childKeys = new ArrayList<>(node.children.keySet());
        for (String key : childKeys) {
            DirectoryNode child = node.children.get(key);
            collapse(child);
            
            if (child.jars.isEmpty() && child.children.size() == 1) {
                String grandchildKey = child.children.keySet().iterator().next();
                DirectoryNode grandchild = child.children.get(grandchildKey);
                
                node.children.remove(key);
                String combinedName = child.name + File.separator + grandchild.name;
                grandchild.name = combinedName;
                node.children.put(combinedName, grandchild);
            }
        }
    }

    private void printNode(DirectoryNode node, String indent, StringBuilder sb) {
        if (!node.name.isEmpty()) {
            sb.append(indent).append("📂 ").append(node.name).append("/\n");
            String newIndent = indent + "  ";
            if (!node.jars.isEmpty()) {
                sb.append(formatLexicalGroups(node.jars, newIndent));
            }
            for (DirectoryNode child : node.children.values()) {
                printNode(child, newIndent, sb);
            }
        } else {
            for (DirectoryNode child : node.children.values()) {
                printNode(child, indent, sb);
            }
        }
    }

    private String findCommonRoot(Set<String> paths) {
        if (paths == null || paths.isEmpty()) return null;
        String common = null;
        for (String p : paths) {
            if (common == null) {
                common = p;
            } else {
                int lastMatch = -1;
                for (int i = 0; i < Math.min(common.length(), p.length()); i++) {
                    if (common.charAt(i) != p.charAt(i)) break;
                    if (common.charAt(i) == File.separatorChar) lastMatch = i;
                }
                if (lastMatch == -1) return "";
                common = common.substring(0, lastMatch + 1);
            }
        }
        return common;
    }

    /**
     * Inspects a single JAR file using all registered handlers.
     * <p>
     * It uses a "Best Effort" strategy, collecting metadata from all handlers
     * and picking the one that provides the most information (version, vendor, etc.).
     * </p>
     * 
     * @param jarFile The file to inspect.
     * @return The extracted metadata, or null if no handler could process it.
     */
    private JarMetadata inspect(File jarFile) {
        JarMetadata best = null;
        for (JarHandler handler : handlers) {
            if (handler.canHandle(jarFile)) {
                JarMetadata meta = handler.extractMetadata(jarFile);
                if (meta != null && meta.isBetterThan(best)) {
                    best = meta;
                }
            }
        }
        return best;
    }

    /**
     * Formats a list of JAR metadata into lexical groups for a specific directory.
     * It factors out common versions and prefixes to minimize token usage.
     * <p>
     * This implementation uses a "Majority Version" rule: it promotes the most frequent
     * version to the group level to save tokens.
     * </p>
     * 
     * @param jars The list of JARs in the directory.
     * @param indent The current indentation string.
     * @return A formatted string listing the JARs by prefix and version.
     */
    private String formatLexicalGroups(List<JarMetadata> jars, String indent) {
        if (jars == null || jars.isEmpty()) return "";

        Map<String, List<JarMetadata>> groups = new TreeMap<>();
        
        // Contextual Prefix Discovery: Longest shared segment-prefix
        for (JarMetadata j : jars) {
            String bestPrefix = getFirstSegment(j.getId());
            for (JarMetadata other : jars) {
                if (j == other) continue;
                String common = getCommonPrefix(j.getId(), other.getId());
                if (common != null && common.length() > bestPrefix.length()) {
                    bestPrefix = common;
                }
            }
            groups.computeIfAbsent(bestPrefix, (String k) -> new ArrayList<>()).add(j);
        }

        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, List<JarMetadata>> entry : groups.entrySet()) {
            String prefix = entry.getKey();
            List<JarMetadata> members = entry.getValue();
            
            // Single-member optimization: No core suffix, just the ID
            if (members.size() == 1) {
                JarMetadata m = members.get(0);
                sb.append(indent).append("📄 ").append(m.getId());
                if (m.getVersion() != null) {
                    sb.append(" v").append(m.getVersion());
                }
                sb.append("\n");
                continue;
            }

            // Version Majority Logic
            Map<String, Integer> versionCounts = new HashMap<>();
            for (JarMetadata m : members) {
                if (m.getVersion() != null) {
                    versionCounts.put(m.getVersion(), versionCounts.getOrDefault(m.getVersion(), 0) + 1);
                }
            }
            
            String majorityVer = null;
            int maxCount = 0;
            for (Map.Entry<String, Integer> verEntry : versionCounts.entrySet()) {
                if (verEntry.getValue() > maxCount) {
                    maxCount = verEntry.getValue();
                    majorityVer = verEntry.getKey();
                }
            }
            
            // Only promote if it's actually a significant majority (or only one version exists)
            if (versionCounts.size() > 1 && maxCount <= members.size() / 2) {
                majorityVer = null; 
            }

            final String finalMajorityVer = majorityVer;

            sb.append(indent).append("📄 ").append(prefix);
            if (finalMajorityVer != null) {
                sb.append(" v").append(finalMajorityVer);
            }

            List<String> items = members.stream().map(m -> {
                String id = m.getId();
                String suffix;
                if (id.equals(prefix)) {
                    suffix = "core";
                } else if (id.startsWith(prefix + "-") || id.startsWith(prefix + ".")) {
                    suffix = id.substring(prefix.length() + 1);
                } else {
                    suffix = id;
                }
                
                // Override version if different from majority
                if (m.getVersion() != null && !m.getVersion().equals(finalMajorityVer)) {
                    suffix += " v" + m.getVersion();
                }
                return suffix;
            }).distinct().sorted().collect(Collectors.toList());

            if (items.size() > 1 || !items.get(0).equals("core")) {
                sb.append(": ").append(String.join(", ", items));
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    private String getFirstSegment(String id) {
        int idx = id.indexOf('-');
        int dotIdx = id.indexOf('.');
        if (idx == -1) idx = dotIdx;
        else if (dotIdx != -1) idx = Math.min(idx, dotIdx);
        
        return (idx != -1) ? id.substring(0, idx) : id;
    }

    private String getCommonPrefix(String id1, String id2) {
        String[] s1 = id1.split("(?<=[-.])|(?=[-.])");
        String[] s2 = id2.split("(?<=[-.])|(?=[-.])");
        
        StringBuilder prefix = new StringBuilder();
        int i = 0;
        while (i < s1.length && i < s2.length && s1[i].equals(s2[i])) {
            prefix.append(s1[i]);
            i++;
        }
        
        String result = prefix.toString();
        // Trim trailing separator if it's the last character
        if (result.endsWith("-") || result.endsWith(".")) {
            result = result.substring(0, result.length() - 1);
        }
        return result.isEmpty() ? null : result;
    }
    
    /**
     * {@inheritDoc}
     * <p>Returns the pretty-printed version of the classpath.</p>
     */
    @Override
    public String toString() {
        return pretty;
    }

    private static class DirectoryNode {
        String name;
        final List<JarMetadata> jars = new ArrayList<>();
        final Map<String, DirectoryNode> children = new TreeMap<>();

        DirectoryNode(String name) {
            this.name = name;
        }
    }
}
