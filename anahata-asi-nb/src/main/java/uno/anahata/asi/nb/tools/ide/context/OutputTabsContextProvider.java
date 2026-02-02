/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide.context;

import java.util.List;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.ide.IDE;
import uno.anahata.asi.nb.tools.ide.OutputTabInfo;

/**
 * Provides a list of all open tabs in the NetBeans Output Window,
 * including a tail of the content for each tab.
 */
public class OutputTabsContextProvider extends BasicContextProvider {

    private final IDE ideToolkit;

    public OutputTabsContextProvider(IDE ideToolkit) {
        super("netbeans-open-output-tabs", "Open Output Tabs", "A list of all open tabs in the NetBeans Output Window with a tail of their content.");
        this.ideToolkit = ideToolkit;
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        List<OutputTabInfo> outputTabs = ideToolkit.listOutputTabs();
        StringBuilder sb = new StringBuilder("### Open Output Tabs\n");
        if (outputTabs.isEmpty()) {
            sb.append("No output tabs are currently open.");
        } else {
            for (OutputTabInfo tab : outputTabs) {
                String cleanName = tab.getDisplayName().replaceAll("<[^>]*>", "");
                sb.append(String.format("#### %s (ID: %d, Lines: %d, Running: %s)\n", 
                        cleanName, 
                        tab.getId(), 
                        tab.getTotalLines(), 
                        tab.isRunning()));
                
                int tailLines = 20;
                try {
                    String content = ideToolkit.getOutputTabTail(tab.getId(), tailLines, 512);
                    if (content != null && !content.isBlank()) {
                        sb.append("```text\n");
                        if (tab.getTotalLines() > tailLines) {
                            sb.append("... [skipping ").append(tab.getTotalLines() - tailLines).append(" lines] ...\n");
                        }
                        sb.append(content).append("\n");
                        sb.append("```\n");
                    } else {
                        sb.append("  *(Empty or content not available)*\n");
                    }
                } catch (Exception e) {
                    sb.append("  *(Error retrieving content: ").append(e.getMessage()).append(")*\n");
                }
                sb.append("\n");
            }
        }
        ragMessage.addTextPart(sb.toString());
    }
}
