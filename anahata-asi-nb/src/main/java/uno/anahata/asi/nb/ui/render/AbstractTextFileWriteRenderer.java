/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.io.File;
import java.util.List;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JLayer;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.diff.DiffController;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.nb.tools.ide.Editor;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.ParameterRenderer;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.toolkit.files.AbstractTextFileWrite;
import uno.anahata.asi.toolkit.files.LineComment;

/**
 * Base class for rendering file write operations (full updates or surgical replacements)
 * using the NetBeans native diff viewer decorated with agentic annotations.
 * 
 * <p>This renderer implements a sophisticated "Decorator Pattern" to overcome the limitations
 * of the NetBeans Diff module, which typically isolates its sidebars from the standard
 * annotation system. By using a {@link JLayer} with a custom {@link DiffAnnotationsLayerUI},
 * we are able to paint comic-style AI comment bubbles directly over the diff visualizer
 * without modifying its internal component hierarchy.</p>
 * 
 * <p>Key Features:</p>
 * <ul>
 *   <li><b>Historical Diff Persistence</b>: Automatically captures the file state before
 *   execution and persists it in the tool call DTO. This ensures that even after a 
 *   NetBeans restart, the history shows what was changed rather than an empty diff.</li>
 *   <li><b>Boundary-Aware Scrolling</b>: Solves the "Scroll Trap" issue where nested 
 *   NetBeans scroll panes consume all wheel events. It automatically re-dispatches 
 *   scrolling to the parent conversation when boundaries are hit.</li>
 *   <li><b>JLayer Decoration</b>: Safely overlays AI content without disrupting the 
 *   internal component tree of the NetBeans Diff module.</li>
 *   <li><b>Comic-Style Annotations</b>: Renders AI comments as translucent bubbles 
 *   anchored to specific line numbers in the modified side of the diff.</li>
 * </ul>
 * 
 * <p>Research Credits - This implementation was made possible by dissecting the following 
 * NetBeans internal components to understand the coordinate mapping and layout management 
 * of the Diff Viewer:</p>
 * <ul>
 *   <li>{@code org.netbeans.modules.editor.NbEditorUtilities}</li>
 *   <li>{@code org.openide.text.NbDocument}</li>
 *   <li>{@code org.openide.text.Line}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.DiffViewImpl}</li>
 *   <li>{@code org.netbeans.api.diff.StreamSource}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.LinesComponent}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.DEditorPane}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.editable.LineNumbersActionsBar}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.editable.EditableDiffView}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.editable.DiffViewManager}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.editable.DiffContentPanel}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.editable.DiffSplitPaneDivider}</li>
 *   <li>{@code org.netbeans.modules.diff.builtin.visualizer.editable.DecoratedEditorPane}</li>
 * </ul>
 * 
 * @param <T> The type of the file write DTO, extending AbstractTextFileWrite.
 * @author anahata
 */
@Slf4j
public abstract class AbstractTextFileWriteRenderer<T extends AbstractTextFileWrite> implements ParameterRenderer<T> {

    /** The agi panel where this renderer is hosted. */
    protected AgiPanel agiPanel;
    
    /** The tool call associated with this parameter. */
    protected AbstractToolCall<?, ?> call;
    
    /** The name of the parameter being rendered. */
    protected String paramName;
    
    /** The current state of the file write update. */
    protected T update;

    /** 
     * Container panel with a height cap to prevent "blank line heaps" from 
     * corrupting the conversation layout.
     */
    protected final JPanel container = new JPanel(new BorderLayout()) {
        @Override
        public Dimension getPreferredSize() {
            Dimension d = super.getPreferredSize();
            // Cap height at 800px, allow growth up to that point.
            return new Dimension(d.width, Math.min(d.height, 800));
        }
    };

    /** 
     * The NetBeans DiffController instance responsible for managing the two-pane
     * visualizer and synchronized scrolling.
     */
    private DiffController controller;
    
    /** 
     * The document containing the proposed modifications. This document is editable, 
     * allowing the user to refine the AI's proposal before execution.
     */
    private Document modDoc;
    
    /** 
     * The last rendered update DTO. Used to verify if an incoming update requires 
     * a full visualizer re-render.
     */
    private T lastRenderedUpdate;

    /** 
     * The last rendered status of the tool call response. Required to detect
     * transitions from proposal to application.
     */
    private ToolExecutionStatus lastRenderedStatus;
    
    /** 
     * The UI layer responsible for painting AI annotations (bubbles and icons) 
     * over the diff visualizer.
     */
    private DiffAnnotationsLayerUI layerUI;
    
    /** 
     * The JLayer container wrapping the NetBeans diff visualizer. This is the 
     * entry point for the decoration logic.
     */
    private JLayer<JComponent> jlayer;

    /** No-arg constructor for factory instantiation. */
    protected AbstractTextFileWriteRenderer() {}

    /** {@inheritDoc} */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, T value) {
        this.agiPanel = agiPanel;
        this.call = call;
        this.paramName = paramName;
        this.update = value;
    }

    /** {@inheritDoc} */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /** {@inheritDoc} */
    @Override
    public void updateContent(T value) {
        this.update = value;
    }

    /**
     * Subclasses must provide the full proposed content based on the current disk state.
     * 
     * @param currentContent The current text content of the file.
     * @return The proposed new content.
     * @throws Exception if calculation fails.
     */
    protected abstract String calculateProposedContent(String currentContent) throws Exception;

    /**
     * Subclasses can provide line-level comments to be displayed in the diff gutter.
     * 
     * @param currentContent The current text content of the file.
     * @return A list of {@link LineComment} objects.
     */
    protected abstract List<LineComment> getLineComments(String currentContent);

    /**
     * Subclasses must be able to create a new DTO instance with updated content 
     * when the user manually edits the proposed document in the UI.
     * 
     * @param newContent The edited content from the document.
     * @return A new DTO instance of type T.
     */
    protected abstract T createUpdatedDto(String newContent);

    /**
     * Returns the index of the tab that should be selected by default when the 
     * visualizer is first rendered.
     * 
     * @return 0 for Graphical, 1 for Textual.
     */
    protected int getInitialTabIndex() {
        return 0;
    }

    /** 
     * {@inheritDoc} 
     * 
     * <p>The main rendering loop. Performs stability checks, initializes the NetBeans 
     * Diff system, and installs the agentic decoration layer.</p>
     */
    @Override
    public boolean render() {
        if (update == null) {
            return false;
        }

        ToolExecutionStatus status = call.getResponse().getStatus();

        // 1. Stability check: if the incoming update AND status are the same as what we just rendered
        if (Objects.equals(update, lastRenderedUpdate) && status == lastRenderedStatus) {
            return true;
        }

        try {
            File file = new File(update.getPath());
            FileObject fo = FileUtil.toFileObject(file);

            boolean isPending = status == ToolExecutionStatus.PENDING;

            // --- Historical Persistence Logic ---
            String baseContent;
            if (update.getOriginalContent() != null) {
                // Use the persisted original content for history
                baseContent = update.getOriginalContent();
            } else {
                // First render: capture current content from disk/IDE
                baseContent = (fo != null) ? fo.asText() : "";
                if (isPending) {
                    // Store it in the DTO. Kryo will serialize the change in the args map.
                    update.setOriginalContent(baseContent);
                }
            }

            String currentOnDisk = (fo != null) ? fo.asText() : "";
            String proposedContent = calculateProposedContent(currentOnDisk);

            // 2. Secondary stability check: content + status
            if (modDoc != null) {
                String docText = modDoc.getText(0, modDoc.getLength());
                if (docText.equals(proposedContent) && status == lastRenderedStatus) {
                    lastRenderedUpdate = update;
                    return true;
                }
            }

            String name = (fo != null) ? fo.getName() : "new_file";
            String mime = (fo != null) ? fo.getMIMEType() : "text/plain";

            EditorKit kit = MimeLookup.getLookup(mime).lookup(EditorKit.class);
            if (kit == null && !"text/plain".equals(mime)) {
                log.info("No EditorKit found for MIME type: {}. Falling back to text/plain", mime);
                kit = MimeLookup.getLookup("text/plain").lookup(EditorKit.class);
            }

            if (kit == null) {
                throw new IllegalStateException("Could not find an EditorKit for " + mime + " or text/plain");
            }

            Document baseDoc = kit.createDefaultDocument();
            baseDoc.insertString(0, baseContent, null);

            this.modDoc = kit.createDefaultDocument();
            modDoc.insertString(0, proposedContent, null);

            // 3. Sync user edits back to the tool call's modifiedArgs (only if pending)
            if (isPending) {
                modDoc.addDocumentListener(new DocumentListener() {
                    private void sync() {
                        try {
                            String text = modDoc.getText(0, modDoc.getLength());
                            T edited = createUpdatedDto(text);
                            lastRenderedUpdate = edited;
                            call.setModifiedArgument(paramName, edited);
                        } catch (BadLocationException ex) {
                            log.error("Failed to sync edited document content", ex);
                        }
                    }
                    @Override public void insertUpdate(DocumentEvent e) { sync(); }
                    @Override public void removeUpdate(DocumentEvent e) { sync(); }
                    @Override public void changedUpdate(DocumentEvent e) { sync(); }
                });
            }

            String leftTitle = isPending ? "Current" : "Original";
            String rightTitle = isPending ? "Proposed" : "Applied";

            DiffStreamSource baseSource = new DiffStreamSource(name, leftTitle, baseContent, mime);
            baseSource.setDocument(baseDoc);

            DiffStreamSource modSource = new DiffStreamSource(name, rightTitle, proposedContent, mime);
            modSource.setDocument(modDoc);
            modSource.setEditable(isPending); 

            DiffController next = (controller == null)
                    ? DiffController.createEnhanced(baseSource, modSource)
                    : DiffController.createEnhanced(controller, baseSource, modSource);

            // Trigger a UI rebuild if the controller instance changed OR if the status changed.
            if (next != controller || status != lastRenderedStatus) {
                controller = next;
                JComponent visualizer = controller.getJComponent();
                applyVisualizerSettings(visualizer);

                // Create the header panel with the file hyperlink and toggle
                List<LineComment> comments = getLineComments(currentOnDisk);
                
                JCheckBox toggle = new JCheckBox("Show AI Comments", true);
                JPanel headerPanel = createHeaderPanel(update.getPath(), comments, toggle, status);

                // Create the UI Layer for agentic annotations
                layerUI = new DiffAnnotationsLayerUI(comments);
                jlayer = new JLayer<>(visualizer, layerUI);

                // Handle tab changes to show/hide bubbles
                JTabbedPane tabs = SwingUtils.findComponent(visualizer, JTabbedPane.class);
                if (tabs != null) {
                    tabs.setSelectedIndex(getInitialTabIndex());
                    tabs.addChangeListener(new ChangeListener() {
                        @Override
                        public void stateChanged(ChangeEvent e) {
                            if (layerUI != null) {
                                // Only show bubbles on the Graphical tab (index 0)
                                layerUI.setShowingComments(toggle.isSelected() && tabs.getSelectedIndex() == 0);
                                jlayer.repaint();
                            }
                        }
                    });
                }

                container.removeAll();
                container.add(headerPanel, BorderLayout.NORTH);
                container.add(jlayer, BorderLayout.CENTER);
                
                // Finalize layout after the component is realized
                jlayer.addHierarchyListener(new HierarchyListener() {
                    @Override
                    public void hierarchyChanged(HierarchyEvent e) {
                        if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0 && jlayer.isShowing()) {
                            SwingUtils.runInEDT(() -> fixSplitPane(jlayer));
                            jlayer.removeHierarchyListener(this);
                        }
                    }
                });

                container.revalidate();
                container.repaint();
            }

            lastRenderedUpdate = update;
            lastRenderedStatus = status;
            return true;
        } catch (Exception e) {
            log.error("Failed to render diff", e);
            renderError("Error loading diff:\n" + e.getMessage());
            return false;
        }
    }

    /**
     * Renders a multi-line error message when diff generation fails.
     * 
     * @param message The detailed error message.
     */
    private void renderError(String message) {
        JTextArea errorArea = new JTextArea(message);
        errorArea.setEditable(false);
        errorArea.setLineWrap(true);
        errorArea.setWrapStyleWord(true);
        errorArea.setBackground(UIManager.getColor("Panel.background"));
        errorArea.setForeground(Color.RED);
        errorArea.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

        container.removeAll();
        container.add(errorArea, BorderLayout.CENTER);
        container.revalidate();
        container.repaint();
    }

    /**
     * Creates the top header panel containing the file link and annotation toggle.
     * 
     * @param path The absolute path to the file.
     * @param comments The list of comments for line jumping.
     * @param toggle The checkbox used to toggle AI comments.
     * @param status The current execution status of the tool.
     * @return The populated header {@link JPanel}.
     */
    private JPanel createHeaderPanel(String path, List<LineComment> comments, JCheckBox toggle, ToolExecutionStatus status) {
        JPanel panel = new JPanel(new BorderLayout());
        JPanel topRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 5));
        
        File f = new File(path);
        JButton link = new JButton("<html><a href='#'>" + f.getName() + "</a></html>");
        link.setToolTipText(path);
        link.setBorderPainted(false);
        link.setOpaque(false);
        link.setBackground(new Color(0, 0, 0, 0));
        link.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        link.addActionListener(e -> {
            try {
                int jumpLine = (comments != null && !comments.isEmpty()) ? comments.get(0).getLineNumber() : 1;
                agiPanel.getAgi().getToolkit(Editor.class).ifPresent(t -> {
                    try {
                        t.openFile(path, jumpLine);
                    } catch (Exception ex) {
                        log.error("Failed to open file", ex);
                    }
                });
            } catch (Exception ex) {
                log.error("Failed to open file via hyperlink", ex);
            }
        });

        toggle.addActionListener(e -> {
            if (layerUI != null) {
                layerUI.setShowingComments(toggle.isSelected());
                jlayer.repaint();
            }
        });

        String labelText;
        switch (status) {
            case PENDING: labelText = "Proposed Changes:"; break;
            case EXECUTED: labelText = "Applied Changes:"; break;
            case DECLINED: labelText = "Changes (Declined):"; break;
            case FAILED: labelText = "Changes (Failed):"; break;
            default: labelText = "Changes (" + status + "):"; break;
        }
        JLabel statusLabel = new JLabel(labelText);
        statusLabel.setFont(statusLabel.getFont().deriveFont(java.awt.Font.BOLD));
        
        topRow.add(statusLabel);
        topRow.add(link);
        topRow.add(Box.createHorizontalStrut(20));
        topRow.add(toggle);
        
        panel.add(topRow, BorderLayout.NORTH);
        
        if (comments != null && !comments.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            for (LineComment lc : comments) {
                sb.append("Line ").append(lc.getLineNumber()).append(": ").append(lc.getComment()).append("\n");
            }
            
            JTextArea area = new JTextArea(sb.toString().trim());
            area.setEditable(false);
            area.setLineWrap(true);
            area.setWrapStyleWord(true);
            area.setFont(new java.awt.Font("SansSerif", java.awt.Font.ITALIC, 11));
            area.setForeground(Color.GRAY);
            area.setOpaque(false);
            area.setBackground(new Color(0, 0, 0, 0));
            area.setBorder(BorderFactory.createEmptyBorder(0, 15, 5, 10));
            
            panel.add(area, BorderLayout.CENTER);
        }
        
        return panel;
    }

    /**
     * Forces the internal JSplitPane of the NetBeans diff viewer to a 50/50 split
     * by recursively searching for the split pane component.
     * 
     * @param c The component to start the search from.
     */
    private void fixSplitPane(Component c) {
        if (c instanceof JSplitPane sp) {
            sp.setDividerLocation(0.5);
            sp.setResizeWeight(0.5);
        } else if (c instanceof Container cont) {
            for (Component child : cont.getComponents()) {
                fixSplitPane(child);
            }
        }
    }

    /**
     * Injects custom client properties into the NetBeans component tree.
     * Specifically, it enables the "diff_merger" system. The unified scrolling 
     * logic is now handled by the JLayer's LayerUI.
     * 
     * @param c The component to start the configuration from.
     */
    private void applyVisualizerSettings(Component c) {
        if (c instanceof JComponent jc) {
            jc.putClientProperty("diff_merger", Boolean.TRUE);
            jc.putClientProperty("showMergeButtons", Boolean.TRUE);
        }
        if (c instanceof Container cont) {
            for (Component child : cont.getComponents()) {
                applyVisualizerSettings(child);
            }
        }
    }
}
