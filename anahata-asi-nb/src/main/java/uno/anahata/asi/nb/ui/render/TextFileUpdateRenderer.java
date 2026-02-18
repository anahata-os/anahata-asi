/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.io.File;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
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
import uno.anahata.asi.toolkit.files.LineComment;
import uno.anahata.asi.toolkit.files.TextFileUpdate;
import uno.anahata.asi.nb.ui.diff.DiffStreamSource;

/**
 * A rich renderer for {@link TextFileUpdate} tool parameters that embeds a 
 * NetBeans-native diff viewer with support for the Merger UI (red crosses/arrows)
 * and native gutter annotations for AI comments.
 * 
 * @author anahata
 */
@Slf4j
public class TextFileUpdateRenderer implements ParameterRenderer<TextFileUpdate> {

    private final ChatPanel chatPanel;
    private final AbstractToolCall<?, ?> call;
    private final String paramName;
    private TextFileUpdate update;
    
    private final JPanel container = new JPanel(new BorderLayout());
    private DiffController controller;

    public TextFileUpdateRenderer(ChatPanel chatPanel, AbstractToolCall<?, ?> call, String paramName, TextFileUpdate value) {
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
    public void update(TextFileUpdate value) {
        this.update = value;
    }

    @Override
    public boolean render() {
        if (update == null) {
            return false;
        }

        try {
            File file = new File(update.getPath());
            FileObject fo = FileUtil.toFileObject(file);
            
            String currentContent = (fo != null) ? fo.asText() : "";
            String newContent = update.getNewContent();
            String name = (fo != null) ? fo.getName() : "new_file";
            String mime = (fo != null) ? fo.getMIMEType() : "text/plain";

            EditorKit kit = MimeLookup.getLookup(mime).lookup(EditorKit.class);
            Document baseDoc = kit.createDefaultDocument();
            baseDoc.insertString(0, currentContent, null);
            
            Document modDoc = kit.createDefaultDocument();
            modDoc.insertString(0, newContent, null);

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
                applyMergerProperties(visualizer);
                
                container.removeAll();
                container.add(visualizer, BorderLayout.CENTER);
                
                // Attach Annotations
                SwingUtilities.invokeLater(() -> attachAnnotations(modDoc));
                
                container.revalidate();
                container.repaint();
            }
            return true;
        } catch (Exception e) {
            log.error("Failed to render TextFileUpdate diff", e);
            container.removeAll();
            container.add(new JLabel("Error loading diff: " + e.getMessage()));
            return false;
        }
    }

    private void applyMergerProperties(Component c) {
        if (c instanceof JComponent jc) {
            jc.putClientProperty("diff_merger", Boolean.TRUE);
            jc.putClientProperty("showMergeButtons", Boolean.TRUE);
        }
        if (c instanceof Container cont) {
            for (Component child : cont.getComponents()) {
                applyMergerProperties(child);
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
