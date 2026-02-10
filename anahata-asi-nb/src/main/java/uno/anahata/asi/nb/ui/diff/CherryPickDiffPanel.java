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
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.diff.DiffController;
import org.openide.DialogDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.resource.FileTextReplacements;
import uno.anahata.asi.model.resource.TextReplacement;
import uno.anahata.asi.nb.tools.NbCoding;
import uno.anahata.asi.swing.icons.IconUtils;
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
    @Getter
    private final JButton nextBtn = new JButton("Next");
    private final JButton prevBtn = new JButton("Previous");
    private final JButton okBtn = new JButton("OK", IconUtils.getIcon("google.png", 16, 16)); // Using google icon as a temporary 'OK' placeholder
    private final JButton cancelBtn = new JButton("Cancel", IconUtils.getIcon("delete.png", 16, 16));
    private DialogDescriptor descriptor;
    private final JTabbedPane tabbedPane = new JTabbedPane();
    private final List<FileDiffPanel> filePanels = new ArrayList<>();
    private final NbCoding toolkit;

    /**
     * Constructs a new CherryPickDiffPanel.
     * 
     * @param fileReplacements The list of proposed changes per file.
     * @param toolkit The parent NbCoding toolkit instance.
     */
    public CherryPickDiffPanel(List<FileTextReplacements> fileReplacements, NbCoding toolkit) {
        this.toolkit = toolkit;
        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(1200, 850));
        
        if (fileReplacements != null && !fileReplacements.isEmpty()) {
            for (FileTextReplacements fr : fileReplacements) {
                FileDiffPanel p = new FileDiffPanel(fr);
                filePanels.add(p);
                tabbedPane.addTab(new File(fr.getPath()).getName(), p);
            }
            add(tabbedPane, BorderLayout.CENTER);
        } else {
            add(new JLabel("No changes to review.", JLabel.CENTER), BorderLayout.CENTER);
        }
        
        prevBtn.addActionListener(e -> {
            int current = tabbedPane.getSelectedIndex();
            if (current > 0) {
                tabbedPane.setSelectedIndex(current - 1);
            }
        });
        
        nextBtn.addActionListener(e -> {
            int current = tabbedPane.getSelectedIndex();
            if (current < tabbedPane.getTabCount() - 1) {
                tabbedPane.setSelectedIndex(current + 1);
            }
        });
        
        tabbedPane.addChangeListener(e -> updateButtons());
    }

    /**
     * Configures the panel for use within a DialogDescriptor.
     * 
     * @param dd The dialog descriptor to configure.
     */
    public void setupDialog(DialogDescriptor dd) {
        this.descriptor = dd;
        okBtn.addActionListener(e -> descriptor.setValue(okBtn));
        cancelBtn.addActionListener(e -> descriptor.setValue(DialogDescriptor.CANCEL_OPTION));
        updateButtons();
    }

    /**
     * Updates the text and behavior of the wizard buttons based on the current tab.
     */
    private void updateButtons() {
        if (descriptor == null) return;
        int current = tabbedPane.getSelectedIndex();
        int count = tabbedPane.getTabCount();
        
        prevBtn.setEnabled(current > 0);
        nextBtn.setEnabled(current < count - 1);
        
        descriptor.setOptions(new Object[]{cancelBtn, prevBtn, nextBtn, okBtn});
    }

    public JButton getOkBtn() {
        return okBtn;
    }

    /**
     * Collects all screenshots captured by the user across all file cards.
     * 
     * @return A list of PNG byte arrays.
     */
    public List<byte[]> getScreenshots() {
        List<byte[]> screenshots = new ArrayList<>();
        for (FileDiffPanel p : filePanels) {
            screenshots.addAll(p.listPanel.getScreenshots());
        }
        return screenshots;
    }

    /**
     * Returns the list of replacements that were selected (checked) by the user.
     * 
     * @return The list of accepted changes.
     */
    public List<FileTextReplacements> getAcceptedReplacements() {
        return filePanels.stream()
                .map(FileDiffPanel::getAcceptedReplacements)
                .filter(fr -> !fr.getReplacements().isEmpty())
                .collect(Collectors.toList());
    }
    
    /**
     * Aggregates all user feedback comments from all file panels.
     * 
     * @return A formatted string of feedback.
     */
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

    /**
     * A sub-panel for reviewing changes to a single file.
     */
    private static class FileDiffPanel extends JPanel {
        private final FileTextReplacements original;
        private final ReplacementListPanel listPanel;
        private DiffController controller;
        private final JPanel diffContainer = new JPanel(new BorderLayout());

        /**
         * Constructs a FileDiffPanel for the given file replacements.
         * @param fr the replacements for a single file.
         */
        public FileDiffPanel(FileTextReplacements fr) {
            this.original = fr;
            setLayout(new BorderLayout());
            
            listPanel = new ReplacementListPanel(fr.getReplacements(), this::updateDiff);
            
            diffContainer.setMinimumSize(new Dimension(400, 400));
            
            JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, diffContainer, listPanel);
            split.setDividerLocation(0.75); // Give more space to the code diff by default
            split.setResizeWeight(1.0); // The diff container gets all extra space when resizing
            add(split, BorderLayout.CENTER);
            
            updateDiff();
        }

        /**
         * Refreshes the diff view based on the current selection in the list panel.
         */
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

                DiffController next = (controller == null)
                        ? DiffController.createEnhanced(baseSource, modSource)
                        : DiffController.createEnhanced(controller, baseSource, modSource);

                if (next != controller) {
                    controller = next;
                    diffContainer.removeAll();
                    diffContainer.add(controller.getJComponent(), BorderLayout.CENTER);
                }
                
                diffContainer.revalidate();
                diffContainer.repaint();
            } catch (Exception ex) {
                log.error("Failed to update diff for: " + original.getPath(), ex);
            }
        }

        /**
         * Gets the accepted replacements for this specific file.
         * @return a FileTextReplacements object with filtered replacements.
         */
        public FileTextReplacements getAcceptedReplacements() {
            return FileTextReplacements.builder()
                    .path(original.getPath())
                    .lastModified(original.getLastModified())
                    .replacements(listPanel.getSelectedReplacements())
                    .build();
        }
    }
}
