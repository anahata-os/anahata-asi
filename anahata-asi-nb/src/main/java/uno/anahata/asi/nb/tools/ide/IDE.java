/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import java.awt.Component;
import java.awt.Container;
import java.awt.EventQueue;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.Action;
import javax.swing.JEditorPane;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.model.resource.TextViewport;
import uno.anahata.asi.model.resource.TextViewportSettings;
import uno.anahata.asi.nb.tools.ide.context.OpenTopComponentsContextProvider;
import uno.anahata.asi.nb.tools.ide.context.OutputTabsContextProvider;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.toolkit.files.Files;

/**
 * Provides tools for interacting with the NetBeans IDE.
 * This includes managing open windows (TopComponents) and the Output Window.
 */
@Slf4j
@AiToolkit("A toolkit for interacting with the NetBeans IDE.")
public class IDE extends AnahataToolkit {

    private static final String OUTPUT_TAB_CLASS = "org.netbeans.core.output2.OutputTab";

    public IDE() {
        childrenProviders.add(new OpenTopComponentsContextProvider(this));
        childrenProviders.add(new OutputTabsContextProvider(this));
    }

    /**
     * Monitors the IDE log file (messages.log) by loading it into the context with 'tail' enabled.
     * 
     * @return The TextFileResource representing the log file.
     * @throws Exception if the log file cannot be found or loaded.
     */
    @AiTool("Monitors the IDE log file (messages.log) by loading it into the context with 'tail' enabled. This is the most efficient way to keep an eye on IDE errors.")
    public TextFileResource monitorLogs() throws Exception {
        String userDir = System.getProperty("netbeans.user");
        if (userDir == null) {
            throw new Exception("System property 'netbeans.user' is not set.");
        }
        File logFile = new File(userDir, "var/log/messages.log");
        if (!logFile.exists()) {
            throw new Exception("IDE log file not found at: " + logFile.getAbsolutePath());
        }

        Files filesToolkit = getToolManager().getToolkitInstance(Files.class)
                .orElseThrow(() -> new IllegalStateException("Files toolkit not found"));
        
        TextViewportSettings settings = TextViewportSettings.builder()
                .tail(true)
                .tailLines(100)
                .showLineNumbers(true)
                .build();
        
        return filesToolkit.loadTextFileWithSettings(logFile.getAbsolutePath(), settings);
    }

    // --- TOPCOMPONENTS TOOLS ---

    /**
     * Gets a detailed list of all open IDE windows as a structured list of objects.
     * @return a list of TopComponentInfo objects.
     * @throws Exception if an error occurs.
     */
    @AiTool("Gets a detailed list of all open IDE windows as a structured list of objects.")
    public List<TopComponentInfo> getOpenTopComponentsOverview() throws Exception {
        return gatherTopComponentInfo();
    }

    /**
     * Gets a detailed list of all open IDE windows, formatted as a Markdown table.
     * @return a Markdown table string.
     * @throws Exception if an error occurs.
     */
    @AiTool("Gets a detailed list of all open IDE windows, formatted as a Markdown table.")
    public String getOpenTopComponentsMarkdown() throws Exception {
        List<TopComponentInfo> infos = gatherTopComponentInfo();
        if (infos.isEmpty()) {
            return "No TopComponents are currently open.";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("| Id | Name | Selected | Mode | Activated Nodes | Size | Path | Tooltip | ClassName |\n");
        sb.append("|---|---|---|---|---|---|---|---|---|\n");

        for (TopComponentInfo info : infos) {
            String bestName = info.getHtmlDisplayName() != null ? info.getHtmlDisplayName() : 
                              info.getDisplayName() != null ? info.getDisplayName() : info.getName();

            sb.append(String.format("| %s | %s | %s | %s | %s | %d | %s | %s | %s |\n",
                    info.getId() != null ? info.getId().replace("|", "\\|") : "N/A",
                    bestName != null ? bestName.replace("|", "\\|") : "N/A",
                    info.isSelected() ? "Y" : "N",
                    info.getMode() != null ? info.getMode().replace("|", "\\|") : "N/A",
                    info.getActivatedNodes() != null ? info.getActivatedNodes().replace("|", "\\|") : "N/A",
                    info.getSizeInBytes(),
                    info.getPrimaryFilePath() != null ? info.getPrimaryFilePath().replace("|", "\\|") : "N/A",
                    info.getTooltip() != null ? info.getTooltip().replace("|", "\\|") : "N/A",
                    info.getClassName() != null ? info.getClassName().replace("|", "\\|") : "N/A"
            ));
        }
        return sb.toString();
    }

    /**
     * Gets a detailed list of all open IDE windows, formatted as a simple string.
     * @return a detailed string representation of open windows.
     * @throws Exception if an error occurs.
     */
    @AiTool("Gets a detailed list of all open IDE windows, formatted as a simple string.")
    public String getOpenTopComponentsDetailedString() throws Exception {
        List<TopComponentInfo> infos = gatherTopComponentInfo();
        if (infos.isEmpty()) {
            return "No TopComponents are currently open.";
        }

        StringBuilder sb = new StringBuilder();
        for (TopComponentInfo info : infos) {
            sb.append("--------------------------------------------------\n");
            sb.append(String.format("ID: %s\n", info.getId() != null ? info.getId() : "N/A"));
            sb.append(String.format("Name: %s\n", info.getName()));
            sb.append(String.format("Selected: %s\n", info.isSelected() ? "Y" : "N"));
            sb.append(String.format("DisplayName: %s\n", info.getDisplayName()));
            sb.append(String.format("HtmlDisplayName: %s\n", info.getHtmlDisplayName()));
            sb.append(String.format("Tooltip: %s\n", info.getTooltip()));
            sb.append(String.format("ClassName: %s\n", info.getClassName()));
            sb.append(String.format("Mode: %s\n", info.getMode()));
            sb.append(String.format("Activated Nodes: %s\n", info.getActivatedNodes()));
            sb.append(String.format("Supported Actions: %s\n", info.getSupportedActions()));
            sb.append(String.format("File Path: %s\n", info.getFilePath()));
            sb.append(String.format("Primary File Path: %s\n", info.getPrimaryFilePath()));
            sb.append(String.format("Size (bytes): %d\n", info.getSizeInBytes()));
            sb.append("--------------------------------------------------\n\n");
        }
        return sb.toString();
    }

    // --- OUTPUT TOOLS ---

    /**
     * Lists all tabs in the NetBeans Output Window, returning their display names, total lines, and running status.
     * @return a list of OutputTabInfo objects.
     * @throws Exception if an error occurs.
     */
    @AiTool("Lists all tabs in the NetBeans Output Window, returning their display names, total lines, and running status.")
    public List<OutputTabInfo> listOutputTabs() throws Exception {
        final List<OutputTabInfo> tabInfos = new ArrayList<>();
        SwingUtils.runInEDTAndWait(() -> {
            TopComponent outputTC = WindowManager.getDefault().findTopComponent("output");
            if (outputTC == null) {
                log.warn("Output TopComponent not found.");
                return;
            }
            findOutputTabsRecursive(outputTC, tabInfos);
        });
        return tabInfos;
    }

    /**
     * Retrieves the paginated and filtered text content of a specific tab in the NetBeans Output Window.
     * @param id The unique ID of the tab to read.
     * @param startIndex The starting line number (0-based) for pagination.
     * @param pageSize The number of lines to return.
     * @param grepPattern A regex pattern to filter lines.
     * @param maxLineLength The maximum length of each line.
     * @return the filtered text content.
     * @throws Exception if an error occurs.
     */
    @AiTool("Retrieves the paginated and filtered text content of a specific tab in the NetBeans Output Window, with line length truncation.")
    public String getOutputTabContent(
            @AiToolParam("The unique ID of the tab to read.") long id,
            @AiToolParam("The starting line number (0-based) for pagination.") int startIndex,
            @AiToolParam("The number of lines to return.") int pageSize,
            @AiToolParam("A regex pattern to filter lines. Can be null or empty to return all lines.") String grepPattern,
            @AiToolParam("The maximum length of each line. Lines longer than this will be truncated. Set to 0 for no limit.") int maxLineLength) throws Exception {

        final StringBuilder resultBuilder = new StringBuilder();
        SwingUtils.runInEDTAndWait(() -> {
            Optional<JTextComponent> targetComp = findTextComponentById(id);
            if (targetComp.isPresent()) {
                String text = targetComp.get().getText();
                
                // Use TextViewport for processing
                TextViewport viewport = new TextViewport();
                viewport.setSettings(TextViewportSettings.builder()
                        .startChar(0) 
                        .pageSizeInChars(Integer.MAX_VALUE)
                        .grepPattern(grepPattern)
                        .columnWidth(maxLineLength)
                        .build());
                viewport.process(text);
                
                String processed = viewport.getProcessedText();
                List<String> lines = processed.lines().collect(Collectors.toList());
                int start = Math.min(startIndex, lines.size());
                int end = Math.min(start + pageSize, lines.size());
                resultBuilder.append(String.join("\n", lines.subList(start, end)));
            } else {
                resultBuilder.append("Error: No tab found with ID '").append(id).append("'");
            }
        });
        return resultBuilder.toString();
    }

    /**
     * Retrieves the tail of the content of a specific tab in the NetBeans Output Window.
     * @param id The unique ID of the tab to read.
     * @param tailLines The number of lines to return from the end.
     * @param maxLineLength The maximum length of each line.
     * @return the tail of the text content.
     * @throws Exception if an error occurs.
     */
    @AiTool("Retrieves the tail of the content of a specific tab in the NetBeans Output Window.")
    public String getOutputTabTail(
            @AiToolParam("The unique ID of the tab to read.") long id,
            @AiToolParam("The number of lines to return from the end.") int tailLines,
            @AiToolParam("The maximum length of each line. Lines longer than this will be truncated. Set to 0 for no limit.") int maxLineLength) throws Exception {

        final StringBuilder resultBuilder = new StringBuilder();
        SwingUtils.runInEDTAndWait(() -> {
            Optional<JTextComponent> targetComp = findTextComponentById(id);
            if (targetComp.isPresent()) {
                String text = targetComp.get().getText();
                TextViewport viewport = new TextViewport();
                viewport.setSettings(TextViewportSettings.builder()
                        .tail(true)
                        .tailLines(tailLines)
                        .columnWidth(maxLineLength)
                        .build());
                viewport.process(text);
                resultBuilder.append(viewport.getProcessedText());
            } else {
                resultBuilder.append("Error: No tab found with ID '").append(id).append("'");
            }
        });
        return resultBuilder.toString();
    }

    // --- PRIVATE HELPERS ---

    private List<TopComponentInfo> gatherTopComponentInfo() throws InterruptedException, InvocationTargetException {
        if (EventQueue.isDispatchThread()) {
            return gatherInfoOnEDT();
        } else {
            final List<TopComponentInfo> results = new ArrayList<>();
            EventQueue.invokeAndWait(() -> {
                results.addAll(gatherInfoOnEDT());
            });
            return results;
        }
    }

    private List<TopComponentInfo> gatherInfoOnEDT() {
        List<TopComponentInfo> results = new ArrayList<>();
        Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
        if (opened.isEmpty()) {
            return Collections.emptyList();
        }
        TopComponent activated = WindowManager.getDefault().getRegistry().getActivated();

        for (TopComponent tc : opened) {
            String id = WindowManager.getDefault().findTopComponentID(tc);
            String name = tc.getName();
            String displayName = tc.getDisplayName();
            String htmlDisplayName = tc.getHtmlDisplayName();
            String tooltip = tc.getToolTipText();
            String className = tc.getClass().getName();
            boolean isActivated = (tc == activated);
            Mode mode = WindowManager.getDefault().findMode(tc);
            String modeName = (mode != null) ? mode.getName() : "N/A";

            String activatedNodes = "N/A";
            Node[] nodes = tc.getActivatedNodes();
            if (nodes != null && nodes.length > 0) {
                activatedNodes = Arrays.stream(nodes)
                        .map(Node::getDisplayName)
                        .collect(Collectors.joining(", "));
            }

            String supportedActions = "N/A";
            Action[] actions = tc.getActions();
            if (actions != null && actions.length > 0) {
                supportedActions = Arrays.stream(actions)
                        .filter(a -> a != null && a.getValue(Action.NAME) != null)
                        .map(action -> action.getValue(Action.NAME).toString())
                        .collect(Collectors.joining(", "));
            }

            String filePath = "N/A";
            FileObject fileObject = tc.getLookup().lookup(FileObject.class);
            if (fileObject != null) {
                filePath = fileObject.getPath();
            }

            String primaryFilePath = "N/A";
            long sizeInBytes = -1;
            DataObject dataObject = tc.getLookup().lookup(DataObject.class);
            if (dataObject != null) {
                FileObject primaryFile = dataObject.getPrimaryFile();
                if (primaryFile != null) {
                    sizeInBytes = primaryFile.getSize();
                    try {
                        File f = FileUtil.toFile(primaryFile);
                        if (f != null) {
                            primaryFilePath = f.getAbsolutePath();
                        } else {
                            URL url = primaryFile.getURL();
                            primaryFilePath = url.toExternalForm();
                        }
                    } catch (FileStateInvalidException e) {
                        primaryFilePath = "Error getting path: " + e.getMessage();
                    }
                }
            }

            results.add(new TopComponentInfo(id, name, isActivated, displayName, htmlDisplayName, tooltip, className, modeName, activatedNodes, supportedActions, filePath, primaryFilePath, sizeInBytes));
        }
        return results;
    }

    private void findOutputTabsRecursive(Component component, List<OutputTabInfo> tabInfos) {
        if (component == null) {
            return;
        }

        if (component.getClass().getName().equals(OUTPUT_TAB_CLASS)) {
            String title = component.getName();
            boolean isRunning = title != null && title.contains("<b>");

            findTextComponent(component).ifPresent(textComponent -> {
                String text = textComponent.getText();
                int contentSize = text.length();
                int totalLines = (int) text.lines().count();
                long id = System.identityHashCode(textComponent);
                tabInfos.add(new OutputTabInfo(id, title, contentSize, totalLines, isRunning));
            });
        }

        if (component instanceof Container) {
            Container container = (Container) component;
            for (Component child : container.getComponents()) {
                findOutputTabsRecursive(child, tabInfos);
            }
        }
    }

    private Optional<JTextComponent> findTextComponent(Component comp) {
        if (comp instanceof JEditorPane) {
            return Optional.of((JEditorPane) comp);
        }
        if (comp instanceof Container) {
            for (Component child : ((Container) comp).getComponents()) {
                Optional<JTextComponent> found = findTextComponent(child);
                if (found.isPresent()) {
                    return found;
                }
            }
        }
        return Optional.empty();
    }

    private Optional<JTextComponent> findTextComponentById(long id) {
        TopComponent outputTC = WindowManager.getDefault().findTopComponent("output");
        if (outputTC != null) {
            return findTextComponentRecursive(outputTC, id);
        }
        return Optional.empty();
    }
    
    private Optional<JTextComponent> findTextComponentRecursive(Component component, long targetId) {
        if (component == null) {
            return Optional.empty();
        }
        
        if (component instanceof JTextComponent && System.identityHashCode(component) == targetId) {
            return Optional.of((JTextComponent) component);
        }

        if (component instanceof Container) {
            Container container = (Container) component;
            for (Component child : container.getComponents()) {
                Optional<JTextComponent> found = findTextComponentRecursive(child, targetId);
                if (found.isPresent()) {
                    return found;
                }
            }
        }
        return Optional.empty();
    }
}
