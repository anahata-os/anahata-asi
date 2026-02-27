/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.nb.tools.ide.Editor;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.ParameterRenderer;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.toolkit.files.FullTextFileCreate;

/**
 * A rich renderer for the {@link FullTextFileCreate} DTO.
 * It provides syntax highlighting using NetBeans editor kits and allows 
 * the user to edit the proposed content before creation.
 * 
 * @author anahata
 */
@Slf4j
public class FullTextFileCreateRenderer implements ParameterRenderer<FullTextFileCreate> {

    private AgiPanel agiPanel;
    private AbstractToolCall<?, ?> call;
    private String paramName;
    private FullTextFileCreate create;

    private final JPanel container = new JPanel(new BorderLayout()) {
        @Override
        public Dimension getPreferredSize() {
            Dimension d = super.getPreferredSize();
            return new Dimension(d.width, Math.min(d.height, 800));
        }
    };

    private FullTextFileCreate lastRenderedUpdate;
    private ToolExecutionStatus lastRenderedStatus;
    private Document modDoc;

    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, FullTextFileCreate value) {
        this.agiPanel = agiPanel;
        this.call = call;
        this.paramName = paramName;
        this.create = value;
    }

    @Override
    public JComponent getComponent() {
        return container;
    }

    @Override
    public void updateContent(FullTextFileCreate value) {
        this.create = value;
    }

    @Override
    public boolean render() {
        if (create == null) return false;

        ToolExecutionStatus status = call.getResponse().getStatus();
        if (create.equals(lastRenderedUpdate) && status == lastRenderedStatus) {
            return true;
        }

        try {
            boolean isPending = status == ToolExecutionStatus.PENDING;
            String content = create.getContent();

            if (modDoc != null) {
                String docText = modDoc.getText(0, modDoc.getLength());
                if (docText.equals(content) && status == lastRenderedStatus) {
                    lastRenderedUpdate = create;
                    return true;
                }
            }

            String mime = FileUtil.getMIMEType(create.getPath());
            if (mime == null) mime = "text/plain";

            EditorKit kit = MimeLookup.getLookup(mime).lookup(EditorKit.class);
            if (kit == null) kit = MimeLookup.getLookup("text/plain").lookup(EditorKit.class);

            this.modDoc = kit.createDefaultDocument();
            modDoc.insertString(0, content, null);

            if (isPending) {
                modDoc.addDocumentListener(new DocumentListener() {
                    private void sync() {
                        try {
                            String text = modDoc.getText(0, modDoc.getLength());
                            FullTextFileCreate edited = FullTextFileCreate.builder()
                                    .path(create.getPath())
                                    .content(text)
                                    .build();
                            lastRenderedUpdate = edited;
                            call.setModifiedArgument(paramName, edited);
                        } catch (BadLocationException ex) {
                            log.error("Failed to sync edited creation content", ex);
                        }
                    }
                    @Override public void insertUpdate(DocumentEvent e) { sync(); }
                    @Override public void removeUpdate(DocumentEvent e) { sync(); }
                    @Override public void changedUpdate(DocumentEvent e) { sync(); }
                });
            }

            container.removeAll();
            
            // Header
            JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 5));
            JLabel label = new JLabel(isPending ? "Proposed New File:" : "Created File:");
            label.setFont(label.getFont().deriveFont(java.awt.Font.BOLD));
            header.add(label);
            
            JButton link = new JButton("<html><a href='#'>" + new File(create.getPath()).getName() + "</a></html>");
            link.setToolTipText(create.getPath());
            link.setBorderPainted(false);
            link.setOpaque(false);
            link.setBackground(new Color(0, 0, 0, 0));
            link.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            link.addActionListener(e -> {
                 agiPanel.getAgi().getToolkit(Editor.class).ifPresent(t -> {
                    try {
                        t.openFile(create.getPath(), 1);
                    } catch (Exception ex) {
                        log.error("Failed to open file", ex);
                    }
                });
            });
            header.add(link);
            container.add(header, BorderLayout.NORTH);

            // Editor
            javax.swing.JEditorPane editor = new javax.swing.JEditorPane();
            editor.setEditorKit(kit);
            editor.setDocument(modDoc);
            editor.setEditable(isPending);
            
            JScrollPane scroll = new JScrollPane(editor);
            container.add(scroll, BorderLayout.CENTER);

            lastRenderedUpdate = create;
            lastRenderedStatus = status;
            container.revalidate();
            container.repaint();
            return true;

        } catch (Exception e) {
            log.error("Failed to render file creation", e);
            return false;
        }
    }
}
