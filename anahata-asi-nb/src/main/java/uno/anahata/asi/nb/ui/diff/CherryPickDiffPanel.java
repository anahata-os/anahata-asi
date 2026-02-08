/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.diff;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Image;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.Box;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.diff.DiffController;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.resource.FileTextReplacements;
import uno.anahata.asi.model.resource.TextReplacement;
import uno.anahata.asi.nb.tools.NbCoding;
import uno.anahata.asi.swing.internal.UICapture;
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
    private final DefaultListModel<byte[]> framesModel = new DefaultListModel<>();
    private final JList<byte[]> framesList = new JList<>(framesModel);
    private final NbCoding toolkit;

    public CherryPickDiffPanel(List<FileTextReplacements> fileReplacements, NbCoding toolkit) {
        this.toolkit = toolkit;
        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(1100, 800));
        
        // --- Header with Capture Actions ---
        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT));
        JButton captureBtn = new JButton("Capture Screen");
        captureBtn.addActionListener(e -> {
            try {
                java.awt.Window w = SwingUtilities.getWindowAncestor(this);
                if (w != null) {
                    byte[] png = UICapture.captureComponent(w);
                    framesModel.addElement(png);
                }
            } catch (Exception ex) {
                log.error("Capture failed", ex);
            }
        });
        
        JButton removeFrameBtn = new JButton("Remove Selected Frame");
        removeFrameBtn.addActionListener(e -> {
            int idx = framesList.getSelectedIndex();
            if (idx != -1) framesModel.remove(idx);
        });
        
        header.add(captureBtn);
        header.add(removeFrameBtn);
        header.add(new JLabel(" (Frames will be attached to the tool response)"));
        
        // --- Frames Preview ---
        framesList.setLayoutOrientation(JList.HORIZONTAL_WRAP);
        framesList.setVisibleRowCount(1);
        framesList.setCellRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                byte[] data = (byte[]) value;
                ImageIcon icon = new ImageIcon(data);
                Image img = icon.getImage().getScaledInstance(80, 60, Image.SCALE_SMOOTH);
                label.setIcon(new ImageIcon(img));
                label.setText("");
                return label;
            }
        });
        
        JScrollPane framesScroll = new JScrollPane(framesList);
        framesScroll.setPreferredSize(new Dimension(0, 80));
        
        JPanel topContainer = new JPanel(new BorderLayout());
        topContainer.add(header, BorderLayout.NORTH);
        topContainer.add(framesScroll, BorderLayout.CENTER);
        
        add(topContainer, BorderLayout.NORTH);
        add(tabbedPane, BorderLayout.CENTER);
        
        for (FileTextReplacements fr : fileReplacements) {
            FileDiffPanel panel = new FileDiffPanel(fr);
            filePanels.add(panel);
            tabbedPane.addTab(new File(fr.getPath()).getName(), panel);
        }
    }

    public List<byte[]> getCapturedFrames() {
        List<byte[]> frames = new ArrayList<>();
        for (int i = 0; i < framesModel.size(); i++) {
            frames.add(framesModel.get(i));
        }
        return frames;
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
