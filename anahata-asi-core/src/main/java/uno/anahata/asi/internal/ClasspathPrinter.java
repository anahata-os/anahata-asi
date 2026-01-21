/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.ai.tools.spi;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * A utility class for formatting complex Java classpaths into a readable,
 * hierarchical tree structure.
 * <p>
 * This is especially useful for debugging "hot-reload" issues or inspecting
 * the environment of a running JVM.
 * </p>
 */
public class ClasspathPrinter {

    static class TreeNode {
        String name;
        Map<String, TreeNode> children = new TreeMap<>();
        List<String> jars = new ArrayList<>();

        TreeNode(String name) {
            this.name = name;
        }

        int getTotalJarCount() {
            int count = jars.size();
            for (TreeNode child : children.values()) {
                count += child.getTotalJarCount();
            }
            return count;
        }
    }

    /**
     * Prints the given classpath string as a hierarchical tree, grouping JARs by
     * their parent directory and showing counts.
     *
     * @param classpath The classpath string to format.
     * @return A formatted, tree-like string representation of the classpath.
     */
    public static String prettyPrint(String classpath) {
        if (classpath == null || classpath.isEmpty()) {
            return "Classpath is empty.";
        }

        TreeNode root = new TreeNode("/");
        String[] entries = classpath.split(File.pathSeparator);

        for (String entry : entries) {
            File file = new File(entry);
            if (file.exists() && file.isFile() && entry.endsWith(".jar")) {
                String[] pathParts = file.getParent().split(java.util.regex.Pattern.quote(File.separator));
                TreeNode currentNode = root;
                for (String part : pathParts) {
                    if (part.isEmpty()) continue;
                    currentNode = currentNode.children.computeIfAbsent(part, TreeNode::new);
                }
                currentNode.jars.add(file.getName());
            } else if (file.exists() && file.isDirectory()) {
                 String[] pathParts = file.getAbsolutePath().split(java.util.regex.Pattern.quote(File.separator));
                TreeNode currentNode = root;
                for (String part : pathParts) {
                    if (part.isEmpty()) continue;
                    currentNode = currentNode.children.computeIfAbsent(part, TreeNode::new);
                }
            }
        }

        StringBuilder sb = new StringBuilder("Classpath Tree:\n");
        printTree(sb, root, "");
        return sb.toString();
    }

    private static void printTree(StringBuilder sb, TreeNode node, String prefix) {
        if (!node.name.equals("/") || !node.children.isEmpty()) {
             int totalJars = node.getTotalJarCount();
             if (totalJars > 0) {
                sb.append(prefix).append("└─ ").append(node.name).append(" (").append(totalJars).append(" jars)").append("\n");
             } else {
                sb.append(prefix).append("└─ ").append(node.name).append("\n");
             }
        }

        List<TreeNode> children = new ArrayList<>(node.children.values());
        for (int i = 0; i < children.size(); i++) {
            TreeNode child = children.get(i);
            boolean isLast = i == children.size() - 1;
            printTree(sb, child, prefix + (isLast ? "    " : "│   "));
        }
    }
}