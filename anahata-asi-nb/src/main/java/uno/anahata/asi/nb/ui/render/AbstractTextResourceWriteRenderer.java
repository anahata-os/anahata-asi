/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.util.List;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JLayer;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import lombok.extern.slf4j.Slf4j;
import net.miginfocom.swing.MigLayout;
import org.netbeans.api.diff.DiffController;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.util.ImageUtilities;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRenderer;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;
import uno.anahata.asi.toolkit.resources.text.LineComment;

/**
 * Base class for rendering file write operations (full updates or surgical replacements)
 * using the NetBeans native diff viewer decorated with agentic annotations.
 * 
 * <p>This renderer implements a sophisticated Decorator Pattern to overcome the limitations
 * of the NetBeans Diff module, which typically isolates its sidebars from the standard
 * annotation system. By using a {@link JLayer} with a custom {@link DiffAnnotationsLayerUI},
 * we are able to paint comic-style AI comment bubbles directly over the diff visualizer
 * without modifying its internal component hierarchy.</p>
 * 
 * <p>Key Features:</p>
 * <ul>
 *   <li><b>Immediate Pre-Flight Validation</b>: Delegates to the authoritative 
 *   {@link AbstractTextResourceWrite#validate} logic to reject hallucinated or stale proposals 
 *   as soon as they are rendered.</li>
 *   <li><b>Historical Diff Persistence</b>: Automatically captures the file state before
 *   execution and persists it in the tool call DTO.</li>
 *   <li><b>JLayer Decoration</b>: Safely overlays AI content without disrupting the 
 *   internal component tree of the NetBeans Diff module.</li>
 * </ul>
 * 
 * <p>Research Credits - This implementation was made possible by dissecting the following 
 * NetBeans internal components to understand the coordinate mapping and layout management 
 * of the Diff Viewer, Do not delete this list:</p>
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
 * @param <T> The type of the file write DTO, extending AbstractTextResourceWrite.
 * @author anahata
 */
@Slf4j
public abstract class AbstractTextResourceWriteRenderer<T extends AbstractTextResourceWrite> implements ParameterRenderer<T> {

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
    protected final JPanel container = new JPanel(new BorderLayout());

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

    /**
     * The internal tab switcher of the NetBeans visualizer.
     */
    private JTabbedPane tabs;

    /**
     * The tab index recommended based on diff size (0=Graphical, 1=Textual).
     */
    private int recommendedTabIndex = 0;

    /** No-arg constructor for factory instantiation. */
    protected AbstractTextResourceWriteRenderer() {}

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
     * Performs an immediate validation of the file write using the 
     * authoritative logic in the Files toolkit. If validation fails, the 
     * tool call is rejected immediately with a diff of the failed intent.
     * 
     * @return true if valid, false if rejected.
     */
    private boolean validatePreFlight() {
        if (call.getResponse().getStatus() != ToolExecutionStatus.PENDING) {
            return true; 
        }

        try {
            update.validate(agiPanel.getAgi());
            
            String diff = update.getUnifiedDiff();
            
            // Clear previous validation logs and log the current intent
            call.getResponse().getLogs().clear();
            call.getResponse().getLogs().add("Proposed Intent:\n" + diff);
            
            // Smart Tab selection: prefer textual for small changes
            int lines = diff.split("\\R").length;
            this.recommendedTabIndex = (lines <= 30) ? 1 : 0;

            return true;
        } catch (Exception e) {
            log.warn("Pre-flight validation failed for resource {}: {}", update.getResourceUuid(), e.getMessage());
            
            String diff = "";
            try {
                // If validation failed, it might be due to captureOriginalContent or logic.
                // We try to generate whatever diff we can for context.
                if (update.getOriginalContent() != null) {
                    diff = update.getUnifiedDiff();
                }
            } catch (Exception ex) {
                log.error("Failed to generate intent diff for failed validation", ex);
            }
            
            call.getResponse().fail("Validation Failed: " + e.getMessage(), diff);
            return false;
        }
    }

    /**
     * Subclasses can provide line-level comments to be displayed in the diff gutter.
     * 
     * @return A list of {@link LineComment} objects.
     */
    protected abstract List<LineComment> getLineComments();

    /** 
     * Subclasses can provide a specialized panel to display the raw 'Surgical Intent' 
     * (e.g., a list of insertions, replacements, and deletions) at the top of the diff viewer.
     * 
     * @return The intent component, or null.
     */
    protected JComponent createIntentPanel() {
        return null;
    }

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
        return recommendedTabIndex;
    }

    /** 
     * {@inheritDoc} 
     * 
     * <p>The main rendering loop. Performs pre-flight validation, initializes 
     * the NetBeans Diff system, and installs the agentic decoration layer.</p>
     */
    @Override
    public boolean render() {
        if (update == null) {
            return false;
        }

        // 1. Validation check
        if (!validatePreFlight()) {
            return true;
        }
        
        Resource resource = agiPanel.getAgi().getResourceManager().get(update.getResourceUuid());
        ToolExecutionStatus status = call.getResponse().getStatus();

        // 2. Stability check: if the incoming update AND status are the same as what we just rendered
        if (Objects.equals(update, lastRenderedUpdate) && status == lastRenderedStatus) {
            return true;
        }

        try {
            boolean isPending = status == ToolExecutionStatus.PENDING;
            boolean isExecuted = status == ToolExecutionStatus.EXECUTED;

            String baseContent = update.getOriginalContent();
            String proposedContent;
            
            try {
                proposedContent = update.calculateResultingContent();
            } catch (Exception e) {
                log.error("Failed to calculate resulting content", e);
                proposedContent = baseContent; // Fallback
            }

            // 3. Secondary stability check: content + status
            if (modDoc != null) {
                String docText = modDoc.getText(0, modDoc.getLength());
                if (docText.equals(proposedContent) && status == lastRenderedStatus) {
                    lastRenderedUpdate = update;
                    return true;
                }
            }

            String name = resource.getName();
            String mime = resource.getMimeType();

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
            modDoc.putProperty("AsiRole", "Proposed");
            modDoc.insertString(0, proposedContent, null);

            // 4. Sync user edits back to the tool call's modifiedArgs (only if pending)
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

            String leftTitle = isPending ? "Current on Disk" : "Original State";
            String rightTitle;
            if (isPending) {
                rightTitle = "Proposed Change";
            } else if (isExecuted) {
                rightTitle = "Applied Changes";
            } else {
                rightTitle = "Proposed Intent (Rejected/Failed)";
            }

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
                applyVisualizerSettings(visualizer, false);

                // Create the header panel with the file hyperlink and toggle
                List<LineComment> comments = getLineComments();
                
                JCheckBox toggle = new JCheckBox("Show AI Comments", true);
                JPanel headerPanel = createHeaderPanel(resource, comments, toggle, status);

                // Create the UI Layer for agentic annotations
                layerUI = new DiffAnnotationsLayerUI(comments);
                jlayer = new JLayer<>(visualizer, layerUI);
                
                // Wrap the JLayer in a panel that enforces a smart height policy.
                // Textual tab (patch view) expands fully; Graphical tab is capped.
                JPanel diffWrapper = new JPanel(new BorderLayout()) {
                    @Override
                    public Dimension getPreferredSize() {
                        Dimension d = super.getPreferredSize();
                        // Index 1 is usually the 'Textual' tab in the NetBeans Diff Viewer
                        if (tabs != null && tabs.getSelectedIndex() == 1) {
                            Component textualTab = tabs.getComponentAt(1);
                            log.info("Found textualTab " + textualTab);
                            if (textualTab instanceof Container cont) {
                                JEditorPane pane = SwingUtils.findComponent(cont, JEditorPane.class);
                                log.info("Found editor pane " + pane);
                                if (pane != null) {
                                    int lineCount = pane.getDocument().getDefaultRootElement().getElementCount();
                                    int lineHeight = pane.getFontMetrics(pane.getFont()).getHeight();
                                    // Height = lines * height + margin for headers/tabs
                                    return new Dimension(d.width, (lineCount * lineHeight) + 120);
                                }
                            }
                        }
                        return new Dimension(d.width, Math.min(d.height, 800));
                    }
                };
                diffWrapper.add(jlayer, BorderLayout.CENTER);

                // Handle tab changes to show/hide bubbles and adjust height
                this.tabs = SwingUtils.findComponent(visualizer, JTabbedPane.class);
                if (tabs != null) {
                    tabs.setSelectedIndex(getInitialTabIndex());
                    tabs.addChangeListener(new ChangeListener() {
                        @Override
                        public void stateChanged(ChangeEvent e) {
                            if (layerUI != null) {
                                // Only show bubbles on the Graphical tab (index 0)
                                layerUI.setShowingComments(toggle.isSelected() && tabs.getSelectedIndex() == 0);
                                applyVisualizerSettings(jlayer, false);
                                
                                // Ripple the layout change up to the conversation panel
                                container.revalidate();
                                container.repaint();
                                jlayer.repaint();
                            }
                        }
                    });
                }

                container.removeAll();
                container.add(headerPanel, BorderLayout.NORTH);
                container.add(diffWrapper, BorderLayout.CENTER);
                
                // Finalize layout after the component is realized
                jlayer.addHierarchyListener(new HierarchyListener() {
                    @Override
                    public void hierarchyChanged(HierarchyEvent e) {
                        if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0 && jlayer.isShowing()) {
                            SwingUtils.runInEDT(() -> {
                                fixSplitPane(jlayer);
                                applyVisualizerSettings(jlayer, false);
                            });
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
     * @param resource The managed resource.
     * @param comments The list of comments for line jumping.
     * @param toggle The checkbox used to toggle AI comments.
     * @param status The current execution status of the tool.
     * @return The populated header {@link JPanel}.
     */
    private JPanel createHeaderPanel(Resource resource, List<LineComment> comments, JCheckBox toggle, ToolExecutionStatus status) {
        JPanel panel = new JPanel(new BorderLayout());
        JPanel topRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 5));
        topRow.setOpaque(false);
        
        JLabel htmlDisplayName = new JLabel(resource.getHtmlDisplayName());
        htmlDisplayName.setToolTipText(resource.getHandle().getUri().toString());
        htmlDisplayName.setOpaque(false);
        
        if (resource.getHandle() instanceof NbHandle nbh) {
            FileObject fo = nbh.getFileObject();
            if (fo != null) {
                try {
                    DataObject dobj = DataObject.find(fo);
                    Image img = dobj.getNodeDelegate().getIcon(java.beans.BeanInfo.ICON_COLOR_16x16);
                    if (img != null) {
                        htmlDisplayName.setIcon(ImageUtilities.image2Icon(img));
                    }
                } catch (Exception e) {
                    log.warn("Failed to resolve live icon for resource: {} ({})", resource, e.getMessage());
                }
            }
        }

        toggle.setOpaque(false);
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
        topRow.add(htmlDisplayName);
        ResourceUiRegistry.getInstance().getResourceUI().populateActions(topRow, resource, agiPanel);
        topRow.add(Box.createHorizontalStrut(20));
        topRow.add(toggle);
        
        panel.add(topRow, BorderLayout.NORTH);
        
        // --- SURGICAL DASHBOARD (Split Layout) ---
        JPanel dashboard = new JPanel(new MigLayout("fillx, insets 0 15 5 10", "[grow][grow]", "[]"));
        dashboard.setOpaque(false);

        // 1. Left: Surgical Intent Panel
        JComponent intentPanel = createIntentPanel();
        if (intentPanel != null) {
            intentPanel.setOpaque(false);
            dashboard.add(intentPanel, "growx, aligny top");
        } else {
             dashboard.add(new JPanel(), "growx"); // Spacer if no intent
        }

        // 2. Right: Line Comments Panel (Aligned Right)
        if (comments != null && !comments.isEmpty()) {
            StringBuilder sb = new StringBuilder("<html><div style='text-align: right;'>");
            for (LineComment lc : comments) {
                sb.append("<i style='color: #888888; font-size: 9pt;'>Proposed Line ").append(lc.getLineNumber()).append(":</i> ")
                  .append("<span style='color: #666666; font-size: 9pt;'>").append(lc.getComment()).append("</span><br>");
            }
            sb.append("</div></html>");
            
            JLabel commentsLabel = new JLabel(sb.toString());
            commentsLabel.setVerticalAlignment(JLabel.TOP);
            dashboard.add(commentsLabel, "growx, aligny top, alignx right");
        }
        
        if (dashboard.getComponentCount() > 0) {
            panel.add(dashboard, BorderLayout.CENTER);
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
     * This method recursively scans the visualizer to enable the merger UI 
     * and sets the editability of the proposed side editor based on its 
     * role property or physical position in a split pane.
     * 
     * @param c The component to start the configuration from.
     * @param isProposedSide Whether the component is on the proposed side of a split.
     */
    private void applyVisualizerSettings(Component c, boolean isProposedSide) {
        if (c instanceof JEditorPane pane) {
            // Priority 1: Check document property (AsiRole)
            Object role = pane.getDocument().getProperty("AsiRole");
            boolean proposed = "Proposed".equals(role) || isProposedSide;

            if (proposed) {
                // Proposed side is editable only in PENDING state
                pane.setEditable(call.getResponse().getStatus() == ToolExecutionStatus.PENDING);
            } else {
                // Base side is never editable in this renderer
                pane.setEditable(false);
            }
        }

        if (c instanceof JComponent jc) {
            // Enable the internal NetBeans diff merger logic
            jc.putClientProperty("diff_merger", Boolean.TRUE);
            jc.putClientProperty("showMergeButtons", Boolean.TRUE);
        }

        if (c instanceof JSplitPane sp) {
            // Side-by-side diff: Left is Original, Right is Proposed
            applyVisualizerSettings(sp.getLeftComponent(), false);
            applyVisualizerSettings(sp.getRightComponent(), true);
        } else if (c instanceof Container cont) {
            for (Component child : cont.getComponents()) {
                applyVisualizerSettings(child, isProposedSide);
            }
        }
    }
}
