/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.ide;

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.swing.JEditorPane;
import javax.swing.text.JTextComponent;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.model.resource.TextViewport;
import uno.anahata.asi.model.resource.TextViewportSettings;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Static utility for interacting with the NetBeans Output Window and its tabs.
 * 
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class NetBeansOutput {

    private static final String OUTPUT_TAB_CLASS = "org.netbeans.core.output2.OutputTab";

    /**
     * Gathers information about all open output tabs and returns a Markdown report with tails.
     * This method performs a single EDT run to gather all data.
     * 
     * @return A Markdown string.
     * @throws Exception if gathering fails.
     */
    public static String getMarkdownReport() throws Exception {
        long start = System.currentTimeMillis();
        final StringBuilder sb = new StringBuilder("### Open Output Tabs\n");
        
        SwingUtils.runInEDTAndWait(() -> {
            TopComponent outputTC = WindowManager.getDefault().findTopComponent("output");
            if (outputTC == null) {
                sb.append("Output window is not open.");
                return;
            }

            List<OutputTabInfo> tabs = new ArrayList<>();
            findOutputTabsRecursive(outputTC, tabs);

            if (tabs.isEmpty()) {
                sb.append("No output tabs are currently open.");
            } else {
                for (OutputTabInfo tab : tabs) {
                    String cleanName = tab.getDisplayName().replaceAll("<[^>]*>", "");
                    sb.append(String.format("#### %s (ID: %d, Lines: %d, Running: %s)\n", 
                            cleanName, tab.getId(), tab.getTotalLines(), tab.isRunning()));
                    
                    // Get tail content directly while on EDT
                    findTextComponentById(tab.getId()).ifPresent(textComp -> {
                        String text = textComp.getText();
                        if (text != null && !text.isBlank()) {
                            TextViewport viewport = new TextViewport();
                            viewport.setSettings(TextViewportSettings.builder()
                                    .tail(true)
                                    .tailLines(20)
                                    .columnWidth(512)
                                    .build());
                            viewport.process(text);
                            sb.append("```text\n").append(viewport.getProcessedText()).append("\n```\n");
                        }
                    });
                }
            }
        });
        log.info("Output Tabs Markdown report generated in {}ms (including EDT wait)", System.currentTimeMillis() - start);
        return sb.toString();
    }

    /**
     * Lists all open output tabs.
     * 
     * @return A list of OutputTabInfo.
     * @throws Exception if gathering fails.
     */
    public static List<OutputTabInfo> listOutputTabs() throws Exception {
        long start = System.currentTimeMillis();
        final List<OutputTabInfo> tabInfos = new ArrayList<>();
        SwingUtils.runInEDTAndWait(() -> {
            TopComponent outputTC = WindowManager.getDefault().findTopComponent("output");
            if (outputTC != null) {
                findOutputTabsRecursive(outputTC, tabInfos);
            }
        });
        log.info("Gathered info for {} output tabs in {}ms (including EDT wait)", tabInfos.size(), System.currentTimeMillis() - start);
        return tabInfos;
    }

    /**
     * Retrieves the full text content of a specific output tab.
     * 
     * @param id The tab identifier.
     * @return The text content.
     * @throws Exception if the tab is not found or content cannot be read.
     */
    public static String getTabContent(long id) throws Exception {
        final StringBuilder sb = new StringBuilder();
        SwingUtils.runInEDTAndWait(() -> {
            findTextComponentById(id).ifPresent(textComp -> sb.append(textComp.getText()));
        });
        
        if (sb.length() == 0) {
            throw new Exception("Output tab not found or empty: " + id);
        }
        return sb.toString();
    }

    private static void findOutputTabsRecursive(Component component, List<OutputTabInfo> tabInfos) {
        if (component == null) return;

        if (component.getClass().getName().equals(OUTPUT_TAB_CLASS)) {
            String title = component.getName();
            boolean isRunning = title != null && title.contains("<b>");

            findTextComponent(component).ifPresent(textComponent -> {
                String text = textComponent.getText();
                int contentSize = text != null ? text.length() : 0;
                int totalLines = text != null ? (int) text.lines().count() : 0;
                long id = System.identityHashCode(textComponent);
                tabInfos.add(new OutputTabInfo(id, title, contentSize, totalLines, isRunning));
            });
        }

        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                findOutputTabsRecursive(child, tabInfos);
            }
        }
    }

    private static Optional<JTextComponent> findTextComponent(Component comp) {
        if (comp instanceof JEditorPane) return Optional.of((JEditorPane) comp);
        if (comp instanceof Container container) {
            for (Component child : container.getComponents()) {
                Optional<JTextComponent> found = findTextComponent(child);
                if (found.isPresent()) return found;
            }
        }
        return Optional.empty();
    }

    private static Optional<JTextComponent> findTextComponentById(long id) {
        TopComponent outputTC = WindowManager.getDefault().findTopComponent("output");
        return outputTC != null ? findTextComponentRecursive(outputTC, id) : Optional.empty();
    }
    
    private static Optional<JTextComponent> findTextComponentRecursive(Component component, long targetId) {
        if (component == null) return Optional.empty();
        if (component instanceof JTextComponent && System.identityHashCode(component) == targetId) {
            return Optional.of((JTextComponent) component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                Optional<JTextComponent> found = findTextComponentRecursive(child, targetId);
                if (found.isPresent()) return found;
            }
        }
        return Optional.empty();
    }
}
