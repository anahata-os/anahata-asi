/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.File;
import java.util.List;
import javax.swing.Action;
import lombok.extern.slf4j.Slf4j;
import org.openide.awt.Actions;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.toolkit.files.TextViewportSettings;
import uno.anahata.asi.nb.tools.ide.context.OpenTopComponentsContextProvider;
import uno.anahata.asi.nb.tools.ide.context.OutputTabsContextProvider;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * Provides tools for interacting with the NetBeans IDE.
 * This includes managing open windows (TopComponents), the Output Window, 
 * and navigating the project structure.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for interacting with the NetBeans IDE.")
public class IDE extends AnahataToolkit {

    /**
     * Defines the supported targets for programmatic selection in the IDE.
     */
    public enum SelectInTarget {
        @Schema(description = "The logical Projects view (Ctrl+Shift+1).")
        PROJECTS,
        @Schema(description = "The physical Files view (Ctrl+Shift+2).")
        FILES,
        @Schema(description = "The Favorites view (Ctrl+Shift+3).")
        FAVORITES
    }

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
     * @throws Exception if the log file cannot be found or loaded.
     */
    @AiTool(value = "Monitors the IDE log file (messages.log) by loading it into the context with 'tail' enabled and optional grepping.", maxDepth = 12)
    public void monitorLogs(
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

        NbFiles filesToolkit = getToolkit(NbFiles.class);
        
        TextViewportSettings settings = TextViewportSettings.builder()
                .tail(true)
                .tailLines(tailLines != null ? tailLines : 100)
                .grepPattern(grepPattern)
                .showLineNumbers(false)
                .build();
        
        filesToolkit.loadTextFileInternal(logFile.getAbsolutePath(), settings);
    }

    /**
     * Selects and highlights a file or folder in a specific IDE view.
     * 
     * @param path   The absolute path to select.
     * @param target The target view (PROJECTS, FILES, or FAVORITES).
     * @throws Exception if selection fails.
     */
    @AiTool("Selects and highlights the specified file or folder in the selected IDE view.")
    public void selectIn(
            @AiToolParam(value = "The absolute path to the file or folder.", rendererId = "path") String path,
            @AiToolParam("The target view to select in.") SelectInTarget target) throws Exception {
        selectInStatic(path, target);
    }

    /**
     * Static implementation of the universal selection logic.
     * It uses the 'ContextAwareAction' pattern to fuse the target object into the action lookup,
     * ensuring the IDE navigates correctly regardless of current focus.
     * 
     * @param path   The path to select.
     * @param target The target view.
     * @throws Exception if the action cannot be invoked.
     */
    public static void selectInStatic(String path, SelectInTarget target) throws Exception {
        FileObject fo = FileUtil.toFileObject(new File(path));

        if (fo == null) {
            throw new IllegalArgumentException("Target not found: " + path);
        }

        DataObject dobj = DataObject.find(fo);
        
        String actionId;
        switch (target) {
            case PROJECTS -> actionId = "org.netbeans.modules.project.ui.SelectInProjects";
            case FILES -> actionId = "org.netbeans.modules.project.ui.SelectInFiles";
            case FAVORITES -> actionId = "org.netbeans.modules.favorites.Select";
            default -> throw new IllegalArgumentException("Unknown selection target: " + target);
        }

        Action a = Actions.forID("Window/SelectDocumentNode", actionId);
        
        if (a instanceof ContextAwareAction caa) {
            // Create a fused lookup containing everything the target action might expect
            Lookup context = Lookups.fixed(fo, dobj, dobj.getNodeDelegate());
            Action delegate = caa.createContextAwareInstance(context);
            
            // NetBeans UI actions MUST run on the Event Dispatch Thread
            uno.anahata.asi.swing.internal.SwingUtils.runInEDT(() -> {
                delegate.actionPerformed(new java.awt.event.ActionEvent(
                    dobj.getNodeDelegate(), 
                    java.awt.event.ActionEvent.ACTION_PERFORMED, 
                    "select"
                ));
            });
        } else {
            throw new Exception("Selection action '" + actionId + "' not found or is not ContextAware.");
        }
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
     * Gets a Markdown report of all open output tabs, including tails.
     * @return a Markdown string.
     * @throws Exception if an error occurs.
     */
    @AiTool("Gets a Markdown report of all open output tabs, including tails.")
    public String getOutputTabsMarkdown() throws Exception {
        return NetBeansOutput.getMarkdownReport();
    }
}
