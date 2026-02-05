/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.ide;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.swing.Action;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * Static utility for gathering information about open TopComponents (windows) in NetBeans.
 * 
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class NetBeansTopComponents {

    /**
     * Gathers information about all open TopComponents and returns a Markdown table report.
     * This method performs a single EDT run to ensure consistency and performance.
     * 
     * @return A Markdown table string.
     * @throws Exception if gathering fails.
     */
    public static String getMarkdownReport() throws Exception {
        long start = System.currentTimeMillis();
        final List<TopComponentInfo> infos = listTopComponents();

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
                    escape(info.getId()),
                    escape(bestName),
                    info.isSelected() ? "Y" : "N",
                    escape(info.getMode()),
                    escape(info.getActivatedNodes()),
                    info.getSizeInBytes(),
                    escape(info.getPrimaryFilePath()),
                    escape(info.getTooltip()),
                    escape(info.getClassName())
            ));
        }
        log.info("TopComponents Markdown report generated in {}ms", System.currentTimeMillis() - start);
        return sb.toString();
    }

    /**
     * Gathers raw info objects for the @AiTool method.
     * 
     * @return A list of TopComponentInfo.
     * @throws Exception if gathering fails.
     */
    public static List<TopComponentInfo> listTopComponents() throws Exception {
        long start = System.currentTimeMillis();
        final List<TopComponentInfo> results = new ArrayList<>();
        
        SwingUtils.runInEDTAndWait(() -> {
            Set<TopComponent> opened = WindowManager.getDefault().getRegistry().getOpened();
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
                        File f = FileUtil.toFile(primaryFile);
                        if (f != null) {
                            primaryFilePath = f.getAbsolutePath();
                        } else {
                            try {
                                URL url = primaryFile.getURL();
                                primaryFilePath = url.toExternalForm();
                            } catch (Exception e) {
                                primaryFilePath = "URL Error: " + e.getMessage();
                            }
                        }
                    }
                }

                results.add(new TopComponentInfo(id, name, isActivated, displayName, htmlDisplayName, tooltip, className, modeName, activatedNodes, supportedActions, filePath, primaryFilePath, sizeInBytes));
            }
        });
        
        log.info("Gathered info for {} TopComponents in {}ms (including EDT wait)", results.size(), System.currentTimeMillis() - start);
        return results;
    }

    private static String escape(String s) {
        return s != null ? s.replace("|", "\\|").replace("\n", " ") : "N/A";
    }
}
