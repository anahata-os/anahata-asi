/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.awt.BorderLayout;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.diff.DiffController;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.resource.FileTextReplacements;
import uno.anahata.asi.model.resource.TextReplacement;
import uno.anahata.asi.toolkit.files.Files;

/**
 * The main panel for cherry-picking surgical changes across multiple files.
 * It integrates the NetBeans Diff API with a custom selection UI.
 * 
 * @author anahata
 */
@Slf4j
public class CherryPickDiffPanel extends JPanel {
    private final JTabbedPane tabbedPane = new JTabbedPane();
    private final List<FileDiffPanel> filePanels = new ArrayList<>();

    public CherryPickDiffPanel(List<FileTextReplacements> fileReplacements) {
        setLayout(new BorderLayout());
        add(tabbedPane, BorderLayout.CENTER);
        for (FileTextReplacements fr : fileReplacements) {
            FileDiffPanel panel = new FileDiffPanel(fr);
            filePanels.add(panel);
            tabbedPane.addTab(new File(fr.getPath()).getName(), panel);
        }
    }

    public List<FileTextReplacements> getAcceptedReplacements() {
        return filePanels.stream()
                .map(FileDiffPanel::getAcceptedReplacements)
                .filter(fr -> !fr.getReplacements().isEmpty())
                .collect(Collectors.toList());
    }
    
    public String getAggregatedComments() {
        return filePanels.stream()
                .map(p -> {
                    String comments = p.listPanel.getAggregatedComments();
                    if (comments.isEmpty()) return "";
                    return "File: " + p.original.getPath() + "\n" + comments;
                })
                .filter(s -> !s.isEmpty())
                .collect(Collectors.joining("\n\n"));
    }

    private static class FileDiffPanel extends JPanel {
        private final FileTextReplacements original;
        private final ReplacementListPanel listPanel;
        private DiffController controller;
        private final JPanel diffContainer = new JPanel(new BorderLayout());

        public FileDiffPanel(FileTextReplacements fr) {
            this.original = fr;
            setLayout(new BorderLayout());
            
            listPanel = new ReplacementListPanel(fr.getReplacements(), this::updateDiff);
            
            JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, diffContainer, listPanel);
            split.setDividerLocation(0.75);
            split.setResizeWeight(0.75);
            add(split, BorderLayout.CENTER);
            
            updateDiff();
        }

        private void updateDiff() {
            try {
                FileObject fo = FileUtil.toFileObject(new File(original.getPath()));
                if (fo == null) return;
                
                String baseContent = fo.asText();
                List<TextReplacement> selected = listPanel.getSelectedReplacements();
                
                // Use a dummy Files instance to access the protected performReplacements logic
                String modifiedContent = new Files() {
                    public String apply(String c, List<TextReplacement> r) throws Exception {
                        return performReplacements(c, r);
                    }
                }.apply(baseContent, selected);

                DiffStreamSource baseSource = new DiffStreamSource(fo.getName(), "Current", baseContent, fo.getMIMEType());
                DiffStreamSource modSource = new DiffStreamSource(fo.getName(), "Proposed", modifiedContent, fo.getMIMEType());

                if (controller == null) {
                    controller = DiffController.createEnhanced(baseSource, modSource);
                    diffContainer.removeAll();
                    diffContainer.add(controller.getJComponent(), BorderLayout.CENTER);
                } else {
                    controller = DiffController.createEnhanced(controller, baseSource, modSource);
                }
                diffContainer.revalidate();
                diffContainer.repaint();
            } catch (Exception ex) {
                log.error("Failed to update diff for: " + original.getPath(), ex);
            }
        }

        public FileTextReplacements getAcceptedReplacements() {
            return FileTextReplacements.builder()
                    .path(original.getPath())
                    .lastModified(original.getLastModified())
                    .replacements(listPanel.getSelectedReplacements())
                    .build();
        }
    }
}
