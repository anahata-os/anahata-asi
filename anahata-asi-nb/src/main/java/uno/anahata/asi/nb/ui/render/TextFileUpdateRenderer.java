/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.io.File;
import java.util.Objects;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.diff.DiffController;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.netbeans.modules.editor.NbEditorUtilities;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.text.Annotation;
import org.openide.text.Line;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.chat.ChatPanel;
import uno.anahata.asi.swing.chat.render.ParameterRenderer;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.toolkit.files.LineComment;
import uno.anahata.asi.toolkit.files.FullTextFileUpdate;
import uno.anahata.asi.nb.ui.diff.DiffStreamSource;

/**
 * A rich renderer for {@link FullTextFileUpdate} tool parameters that embeds a 
 * NetBeans-native diff viewer with support for the Merger UI (red crosses/arrows)
 * and native gutter annotations for AI comments.
 * 
 * @author anahata
 */
@Slf4j
public class TextFileUpdateRenderer implements ParameterRenderer<FullTextFileUpdate> {

    private ChatPanel chatPanel;
    private AbstractToolCall<?, ?> call;
    private String paramName;
    private FullTextFileUpdate update;
    
    /** 
     * Container panel with a height cap to prevent "blank line heaps" from 
     * corrupting the conversation layout.
     */
    private final JPanel container = new JPanel(new BorderLayout()) {
        @Override
        public Dimension getPreferredSize() {
            Dimension d = super.getPreferredSize();
            // Cap height at 800px, allow growth up to that point.
            // This prevents "Scroll Past End" whitespace from blowing up the conversation view.
            return new Dimension(d.width, Math.min(d.height, 800));
        }
    };
    
    private DiffController controller;
    private Document modDoc;
    private FullTextFileUpdate lastRenderedUpdate;

    /** No-arg constructor for factory instantiation. */
    public TextFileUpdateRenderer() {}

    @Override
    public void init(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, FullTextFileUpdate value) {
        this.chatPanel = chatPanel;
        this.call = call;
        this.paramName = paramName;
        this.update = value;
    }

    @Override
    public JComponent getComponent() {
        return container;
    }

    @Override
    public void updateContent(FullTextFileUpdate value) {
        this.update = value;
    }

    @Override
    public boolean render() {
        if (update == null) {
            return false;
        }
        
        // 1. Stability check: if the incoming update is the same as what we just rendered
        // (including user edits synced back), skip recreation to preserve cursor/scroll.
        if (Objects.equals(update, lastRenderedUpdate)) {
            return true;
        }

        try {
            File file = new File(update.getPath());
            FileObject fo = FileUtil.toFileObject(file);
            
            String currentContent = (fo != null) ? fo.asText() : "";
            String newContent = update.getNewContent();
            
            // 2. Secondary stability check: if we already have a document and its content matches the update, 
            // then the update probably came FROM this document via the sync listener.
            if (modDoc != null) {
                String docText = modDoc.getText(0, modDoc.getLength());
                if (docText.equals(newContent)) {
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
            baseDoc.insertString(0, currentContent, null);
            
            this.modDoc = kit.createDefaultDocument();
            modDoc.insertString(0, newContent, null);
            
            // 3. Sync user edits back to the tool call's modifiedArgs
            modDoc.addDocumentListener(new DocumentListener() {
                private void sync() {
                    try {
                        String text = modDoc.getText(0, modDoc.getLength());
                        // Create a new update object with the edited content
                        FullTextFileUpdate edited = new FullTextFileUpdate(
                                update.getPath(),
                                update.getLastModified(),                                
                                text,
                                update.getLineComments());
                        
                        // Update cache to prevent re-rendering this specific change
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

            DiffStreamSource baseSource = new DiffStreamSource(name, "Current", currentContent, mime);
            baseSource.setDocument(baseDoc);
            
            DiffStreamSource modSource = new DiffStreamSource(name, "Proposed", newContent, mime);
            modSource.setDocument(modDoc);
            modSource.setEditable(true); // Trigger Merger UI

            DiffController next = (controller == null) 
                    ? DiffController.createEnhanced(baseSource, modSource)
                    : DiffController.createEnhanced(controller, baseSource, modSource);

            if (next != controller) {
                controller = next;
                JComponent visualizer = controller.getJComponent();
                applyVisualizerSettings(visualizer);
                
                container.removeAll();
                container.add(visualizer, BorderLayout.CENTER);
                
                // Attach Annotations
                SwingUtilities.invokeLater(() -> attachAnnotations(modDoc));
                
                container.revalidate();
                container.repaint();
            }
            
            lastRenderedUpdate = update;
            return true;
        } catch (Exception e) {
            log.error("Failed to render TextFileUpdate diff", e);
            container.removeAll();
            container.add(new JLabel("Error loading diff: " + e.getMessage()));
            return false;
        }
    }

    private void applyVisualizerSettings(Component c) {
        if (c instanceof JComponent jc) {
            jc.putClientProperty("diff_merger", Boolean.TRUE);
            jc.putClientProperty("showMergeButtons", Boolean.TRUE);
        }
        
        // Find internal scroll panes and redispatch mouse wheel events to the chat scroll pane.
        // This allows the user to scroll through the conversation even when over the diff viewer.
        if (c instanceof JScrollPane sp) {
            sp.addMouseWheelListener(e -> SwingUtils.redispatchMouseWheelEvent(sp, e));
        }
        
        if (c instanceof Container cont) {
            for (Component child : cont.getComponents()) {
                applyVisualizerSettings(child);
            }
        }
    }

    private void attachAnnotations(Document doc) {
        if (update.getLineComments() != null) {
            for (LineComment lc : update.getLineComments()) {
                try {
                    Line line = NbEditorUtilities.getLine(doc, lc.getLineNumber() - 1, false);
                    if (line != null) {
                        new AiAnnotation(lc.getComment()).attach(line);
                    }
                } catch (Exception ex) {
                    log.warn("Failed to attach annotation to line {}", lc.getLineNumber(), ex);
                }
            }
        }
    }

    private static class AiAnnotation extends Annotation {
        private final String text;
        public AiAnnotation(String text) { this.text = text; }
        @Override public String getAnnotationType() { return "org-netbeans-modules-editor-annotations-info"; }
        @Override public String getShortDescription() { return text; }
    }
}
