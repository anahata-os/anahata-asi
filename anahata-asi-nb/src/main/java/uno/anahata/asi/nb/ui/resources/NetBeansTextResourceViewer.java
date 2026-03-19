/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.resources;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Insets;
import java.io.IOException;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.text.Document;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.NbEditorUI;
import org.netbeans.modules.parsing.api.Source;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.FileSystem;
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.StringHandle;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * A NetBeans-native text resource viewer that provides 100% IDE fidelity.
 * <p>
 * This viewer implementation uses the <b>"Total Adoption"</b> strategy: it 
 * requests the official IDE-assembled frame (extComp) and hosts it within 
 * a stable wrapper.
 * </p>
 * <p>
 * <b>Architectural Stability:</b> By adopting the official NetBeans components 
 * (including sidebars and status bar) synchronously, we ensure 100% fidelity 
 * regarding line numbers, folds, and error marks without hierarchy conflicts.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NetBeansTextResourceViewer extends AbstractTextResourceViewer {

    /** The actual NetBeans editor pane. */
    private JEditorPane editor;
    /** The official IDE scroller. */
    private JScrollPane mainScroller;
    /** Wrapper for the assembled IDE frame. */
    private JPanel wrapper = null;

    /**
     * Constructs a new NetBeansTextResourceViewer.
     * 
     * @param agiPanel The parent panel.
     * @param resource The resource to render.
     */
    public NetBeansTextResourceViewer(AgiPanel agiPanel, Resource resource) {
        super(agiPanel, resource);
        // FORCE SINGULARITY: We stay on the editor card to avoid parentage 
        // conflicts and redundant IDE frame assembly.
        setPreviewAsEditor(true);
        // RE-SIGNAL: Explicitly trigger the card swap now that we've locked in singularity mode.
        setEditing(false);
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    protected JComponent createPreviewComponent() {
        return new JPanel();
    }
    
    /**
     * Lazy getter for the visible wrapper panel.
     * @return The wrapper panel.
     */
    private synchronized JPanel getWrapper() {
        if (wrapper == null) {
            wrapper = new JPanel(new BorderLayout());
            wrapper.setOpaque(false); // Respect theme
        }
        return wrapper;
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    protected JComponent createEditorComponent() {
        if (editor == null) {
            initEditor();
        }
        return getWrapper();
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Authoritatively toggles the editability of the 
     * adopted NetBeans editor pane.</p>
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        if (editor != null) {
            editor.setEditable(editable);
        }
    }

    /**
     * Initializes the native editor and adopts the official IDE-assembled frame.
     */
    private void initEditor() {
        try {
            // 1. Authoritative MIME Sensing (Soberanía del Entorno)
            String mime = "text/plain";
            FileObject efmFo = null; // Efemeral FileObject for snippets
            
            if (resource.getHandle() instanceof NbHandle nbh) {
                FileObject fo = nbh.getFileObject();
                if (fo != null) {
                    mime = fo.getMIMEType();
                }
            } else if (resource.getHandle() instanceof StringHandle) {
                // Sonda de Memoria: le preguntamos a NetBeans qué kit usaría para este nombre
                try {
                    FileSystem memFS = FileUtil.createMemoryFileSystem();
                    efmFo = memFS.getRoot().createData(resource.getName());
                    mime = efmFo.getMIMEType();
                } catch (IOException ex) {
                    log.warn("Mime probe failed for snippet: {}. Defaulting to text/plain.", resource.getName());
                }
            }

            // 2. High-Fidelity Pane Setup
            this.editor = new JEditorPane();            
            editor.setContentType(mime);
            editor.setOpaque(true);
            editor.setEditable(false);
            
            // TRIM FIX: Disable NetBeans-specific empty space behaviors for snippets
            editor.putClientProperty("scroll-past-end", Boolean.FALSE);
            editor.setMargin(new Insets(0, 0, 0, 0));
            
            // 3. Document Identity Binding (Total Adoption)
            Document doc = editor.getDocument();
            doc.putProperty("mimeType", mime);
            
            FileObject finalFo = (efmFo != null) ? efmFo : 
                                 (resource.getHandle() instanceof NbHandle nbh ? nbh.getFileObject() : null);
            
            if (finalFo != null) {
                try {
                    DataObject dobj = DataObject.find(finalFo);
                    // This is the key for full high-fidelity: bind the document to an IDE DataObject
                    doc.putProperty(Document.StreamDescriptionProperty, dobj);
                } catch (Exception ex) {
                    log.warn("Failed to find DataObject for: {}", resource.getName());
                }
            }

            // 4. Early Parser Activation
            Source.create(doc);

            // 5. TOTAL ADOPTION: Request the official NetBeans frame
            EditorUI eui = Utilities.getEditorUI(editor);
            if (eui != null) {
                // Ensure the UI doesn't reserve space for "virtual space" at the bottom
                eui.updateTextMargin();
                
                JComponent extComp = eui.getExtComponent();
                // Adopt the official IDE scroller for scroll behavior management
                this.mainScroller = SwingUtils.findComponent(extComp, JScrollPane.class);
                
                getWrapper().removeAll();
                getWrapper().add(extComp, BorderLayout.CENTER);
            } else {
                log.warn("EditorUI missing for: {}. Falling back to basic scroller.", resource.getName());
                this.mainScroller = new JScrollPane(editor);
                getWrapper().removeAll();
                getWrapper().add(mainScroller, BorderLayout.CENTER);
            }
            
            configureScrollBehavior();
            getWrapper().revalidate();
            getWrapper().repaint();

        } catch (Exception e) {
            log.error("Failed to init NetBeans high-fidelity viewer", e);
        }
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public JScrollPane getScrollPane() {
        return mainScroller;
    }

    /** {@inheritDoc} */
    @Override
    public Dimension getPreferredSize() {
        Dimension ps = super.getPreferredSize();
        if (!verticalScrollEnabled && editor != null) {
            try {
                EditorUI eui = Utilities.getEditorUI(editor);
                if (eui != null) {
                    Document doc = editor.getDocument();
                    int lineCount = doc.getDefaultRootElement().getElementCount();
                    int lineHeight = eui.getLineHeight();
                    int textHeight = lineCount * lineHeight;
                    
                    int h = textHeight;
                    if (controlStrip.isVisible()) {
                        h += controlStrip.getPreferredSize().height;
                    }
                    
                    if (mainScroller != null) {
                        // Authoritative Width Sensing: detect if horizontal scrollbar is needed
                        Dimension viewPS = editor.getPreferredSize();
                        int availableWidth = getWidth();
                        if (availableWidth <= 0 && getParent() != null) {
                            availableWidth = getParent().getWidth();
                        }
                        if (availableWidth > 0 && viewPS.width > availableWidth) {
                            h += mainScroller.getHorizontalScrollBar().getPreferredSize().height;
                        }
                    }
                    
                    Insets insets = getInsets();
                    h += insets.top + insets.bottom + 5; // Respecting the 5px safety buffer
                    
                    return new Dimension(ps.width, h);
                }
            } catch (Exception e) {
                log.warn("Precision height calculation failed for NetBeans viewer, falling back to base.");
            }
        }
        return ps;
    }

    /** {@inheritDoc} */
    @Override
    protected void onEditorActivated() {
        syncWithResource();
    }

    /** {@inheritDoc} */
    @Override
    protected void onPreviewActivated() {
        if (editor != null) {
            editor.setEditable(false);
        }
        syncWithResource();
    }

    /** {@inheritDoc} */
    @Override
    public String getEditorContent() {
        return (editor != null) ? editor.getText() : null;
    }

    /** {@inheritDoc} */
    @Override
    protected void updatePreviewContent(String content) {
        if (editor != null) {
            String newText = (content != null) ? content : "";
            if (!editor.getText().equals(newText)) {
                int caret = editor.getCaretPosition();
                editor.setText(newText);
                try {
                    editor.setCaretPosition(Math.min(caret, newText.length()));
                } catch (Exception ex) {
                    editor.setCaretPosition(0);
                }
                
                // Breathing re-layout for snippets
                if (!verticalScrollEnabled) {
                    revalidate();
                    repaint();
                }
            }
        }
    }
}
