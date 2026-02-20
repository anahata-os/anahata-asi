/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import java.io.File;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.toolkit.files.TextViewportSettings;
import uno.anahata.asi.nb.tools.ide.context.OpenTopComponentsContextProvider;
import uno.anahata.asi.nb.tools.ide.context.OutputTabsContextProvider;
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

    /**
     * Constructs a new IDE toolkit and initializes its child context providers.
     */
    public IDE() {
        OpenTopComponentsContextProvider otc = new OpenTopComponentsContextProvider();
        otc.setParent(this);
        childrenProviders.add(otc);

        OutputTabsContextProvider ot = new OutputTabsContextProvider();
        ot.setParent(this);
        childrenProviders.add(ot);
    }

    /**
     * Monitors the IDE log file (messages.log) by loading it into the context.
     * 
     * @param grepPattern Optional regex pattern to filter log lines.
     * @param tailLines Number of recent lines to include (defaults to 100).
     * @return The TextFileResource representing the log file.
     * @throws Exception if the log file cannot be found or loaded.
     */
    @AiTool("Monitors the IDE log file (messages.log) by loading it into the context with 'tail' enabled and optional grepping.")
    public TextFileResource monitorLogs(
            @AiToolParam("Optional regex pattern to filter log lines (e.g. 'ERROR' or a specific logger name).") String grepPattern,
            @AiToolParam("Number of lines to tail from the end of the file or matching results.") Integer tailLines) throws Exception {
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
                .tailLines(tailLines != null ? tailLines : 100)
                .grepPattern(grepPattern)
                .showLineNumbers(false)
                .build();
        
        return filesToolkit.loadTextFileWithSettings(logFile.getAbsolutePath(), settings);
    }

    /**
     * Lists all open IDE windows (TopComponents).
     * @return a list of TopComponentInfo objects.
     * @throws Exception if an error occurs.
     */
    @AiTool("Lists all open IDE windows (TopComponents).")
    public List<TopComponentInfo> listTopComponents() throws Exception {
        return NetBeansTopComponents.listTopComponents();
    }

    /**
     * Gets a Markdown table of all open IDE windows.
     * @return a Markdown table string.
     * @throws Exception if an error occurs.
     */
    @AiTool("Gets a Markdown table of all open IDE windows.")
    public String getTopComponentsMarkdown() throws Exception {
        return NetBeansTopComponents.getMarkdownReport();
    }

    /**
     * Lists all tabs in the NetBeans Output Window.
     * @return a list of OutputTabInfo objects.
     * @throws Exception if an error occurs.
     */
    @AiTool("Lists all tabs in the NetBeans Output Window.")
    public List<OutputTabInfo> listOutputTabs() throws Exception {
        return NetBeansOutput.listOutputTabs();
    }

    /**
     * Gets a Markdown report of all open output tabs, including tails.
     * @return a Markdown string.
     * @throws Exception if an error occurs.
     */
    @AiTool("Gets a Markdown report of all open output tabs, including tails.")
    public String getOutputTabsMarkdown() throws Exception {
        return NetBeansOutput.getMarkdownReport();
    }
}
