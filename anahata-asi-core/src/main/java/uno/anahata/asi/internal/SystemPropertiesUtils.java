/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.internal;

import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

/**
 * Utility class for formatting and hierarchical rendering of JVM system properties.
 *
 * @author anahata
 */
public final class SystemPropertiesUtils {

    /**
     * Private constructor to prevent instantiation of utility class.
     */
    private SystemPropertiesUtils() {
    }

    /**
     * Represents a node in the hierarchical tree of system properties.
     */
    public static class SystemPropertyNode {

        /**
         * The segment name of this node (e.g., "java").
         */
        private final String segment;

        /**
         * The full dot-separated path to this node (e.g., "java.vendor").
         */
        private final String fullPath;

        /**
         * The value of the property, if this is a leaf node.
         */
        private Object value;

        /**
         * The children of this node, keyed by their segment name.
         */
        private final Map<String, SystemPropertyNode> children = new TreeMap<>();

        /**
         * Constructs a new node.
         *
         * @param segment the segment name.
         * @param fullPath the full path.
         */
        public SystemPropertyNode(String segment, String fullPath) {
            this.segment = segment;
            this.fullPath = fullPath;
        }

        /**
         * Checks if this node is a leaf (has no children).
         *
         * @return {@code true} if it has no children.
         */
        public boolean isLeaf() {
            return children.isEmpty();
        }

        /**
         * Gets the segment name.
         *
         * @return the segment name.
         */
        public String getSegment() {
            return segment;
        }

        /**
         * Gets the full path.
         *
         * @return the full path.
         */
        public String getFullPath() {
            return fullPath;
        }

        /**
         * Gets the property value.
         *
         * @return the value.
         */
        public Object getValue() {
            return value;
        }

        /**
         * Sets the property value.
         *
         * @param value the new value.
         */
        public void setValue(Object value) {
            this.value = value;
        }

        /**
         * Gets the child nodes.
         *
         * @return the children map.
         */
        public Map<String, SystemPropertyNode> getChildren() {
            return children;
        }
    }

    /**
     * Generates a token-efficient, hierarchical representation of all JVM
     * system properties (excluding the classpath itself).
     *
     * @return a formatted string of system properties.
     */
    public static String getSystemProperties() {
        Properties props = System.getProperties();
        SystemPropertyNode root = new SystemPropertyNode("", "");

        for (Object keyObj : props.keySet()) {
            String key = (String) keyObj;
            if (key.startsWith("java.class.path")) {
                continue;
            }

            String[] parts = key.split("\\.");
            SystemPropertyNode current = root;
            StringBuilder pathAcc = new StringBuilder();
            for (String part : parts) {
                if (pathAcc.length() > 0) {
                    pathAcc.append(".");
                }
                pathAcc.append(part);
                current = current.children.computeIfAbsent(part, k -> new SystemPropertyNode(k, pathAcc.toString()));
            }
            current.value = props.get(key);
        }

        StringBuilder sb = new StringBuilder();
        for (SystemPropertyNode child : root.children.values()) {
            renderSysProp(sb, child, 0);
        }
        return sb.toString();
    }

    /**
     * Recursively renders a system property node and its children into a formatted string.
     *
     * @param sb the StringBuilder to append to.
     * @param node the node to render.
     * @param indent the current indentation level.
     */
    private static void renderSysProp(StringBuilder sb, SystemPropertyNode node, int indent) {
        String tabs = "  ".repeat(indent);

        SystemPropertyNode current = node;
        String displayLabel = current.segment;
        while (current.children.size() == 1 && current.value == null) {
            SystemPropertyNode next = current.children.values().iterator().next();
            displayLabel += "." + next.segment;
            current = next;
        }

        if (current.isLeaf()) {
            sb.append(tabs).append("- `").append(displayLabel).append("`: ")
                    .append(TextUtils.formatValue(current.value)).append("\n");
        } else {
            sb.append(tabs).append("**").append(current.fullPath).append("**:\n");

            if (current.value != null) {
                sb.append(tabs).append("  - `value`: ").append(TextUtils.formatValue(current.value)).append("\n");
            }

            for (SystemPropertyNode child : current.children.values()) {
                renderSysProp(sb, child, indent + 1);
            }
        }
    }
}
