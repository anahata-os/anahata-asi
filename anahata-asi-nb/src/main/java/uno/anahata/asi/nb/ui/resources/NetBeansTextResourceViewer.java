/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.resources;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Insets;
import java.io.IOException;
import java.lang.ref.Cleaner;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.text.Document;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.lib2.EditorApiPackageAccessor;
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
 * requests the official IDE-assembled frame (extComp) and hosts it within a
 * stable wrapper.
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

    /**
     * The actual NetBeans editor pane.
     */
    private JEditorPane editor;
    /**
     * The official IDE scroller.
     */
    private JScrollPane mainScroller;
    /**
     * Wrapper for the assembled IDE frame.
     */
    private JPanel wrapper = null;

    /**
     * Shared MemoryFileSystem for all virtual snippets to prevent GC leaks and
     * AST Indexer fragmentation.
     */
    private static FileSystem sharedMemFS;

    private static final Cleaner CLEANER = Cleaner.create();

    /**
     * Lazily gets the shared MemoryFileSystem.
     */
    private static synchronized FileSystem getSharedMemFS() throws IOException {
        if (sharedMemFS == null) {
            sharedMemFS = FileUtil.createMemoryFileSystem();
        }
        return sharedMemFS;
    }

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
     *
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
     * <p>
     * Implementation details: Authoritatively toggles the editability of the
     * adopted NetBeans editor pane.</p>
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        if (editor != null) {
            editor.setEditable(editable);
        }
    }

    /**
     * Initializes the native editor and adopts the official IDE-assembled
     * frame.
     */
    private void initEditor() {
        try {
            // 1. Authoritative MIME Sensing (Soberanía del Entorno)
            String mime = "text/plain";
            FileObject efmFo = null; // Efemeral FileObject for snippets
            FileObject finalFo = null;

            if (resource.getHandle() instanceof NbHandle nbh) {
                finalFo = nbh.getFileObject();
                if (finalFo != null) {
                    mime = finalFo.getMIMEType();
                }
            } else if (resource.getHandle() instanceof uno.anahata.asi.agi.resource.handle.PathHandle ph) {
                finalFo = FileUtil.toFileObject(new java.io.File(ph.getPath()));
                if (finalFo != null) {
                    mime = finalFo.getMIMEType();
                }
            } else if (resource.getHandle() instanceof StringHandle sh) {
                try {
                    FileSystem memFS = getSharedMemFS();
                    FileObject folder = memFS.getRoot().createFolder(java.util.UUID.randomUUID().toString());
                    efmFo = folder.createData(resource.getName());
                    finalFo = efmFo;
                    mime = efmFo.getMIMEType();

                    if (sh.getContextPath() != null) {
                        finalFo.setAttribute("anahata.contextPath", sh.getContextPath());
                    }

                    // Propagate custom attributes (e.g. anahata.customClasspath)
                    if (sh.getAttributes() != null) {
                        for (java.util.Map.Entry<String, Object> entry : sh.getAttributes().entrySet()) {
                            finalFo.setAttribute(entry.getKey(), entry.getValue());
                        }
                    }

                    // Need to write the content to the MemoryFileSystem file object so EditorCookie reads it!
                    try (java.io.OutputStream os = finalFo.getOutputStream()) {
                        os.write(sh.getContent().getBytes(java.nio.charset.StandardCharsets.UTF_8));
                    }
                } catch (IOException ex) {
                    log.warn("Mime probe failed for snippet: {}. Defaulting to text/plain.", resource.getName());
                }
            }

            // 2. High-Fidelity Pane Setup
            this.editor = new JEditorPane();
            editor.setContentType(mime);
            editor.setEditorKit(org.openide.text.CloneableEditorSupport.getEditorKit(mime));

            if (finalFo != null) {
                try {
                    DataObject dobj = DataObject.find(finalFo);
                    org.openide.cookies.EditorCookie ec = dobj.getLookup().lookup(org.openide.cookies.EditorCookie.class);
                    if (ec != null) {
                        Document doc = ec.openDocument();
                        editor.setDocument(doc);
                    } else {
                        log.warn("No EditorCookie for {}", resource.getName());
                    }
                } catch (Exception ex) {
                    log.warn("Failed to find DataObject for: {}", resource.getName());
                }
            }

            if (editor.getDocument() == null) {
                editor.setDocument(editor.getEditorKit().createDefaultDocument());
            }

            // Explicitly attach an UndoManager for Ctrl+Z support since we are outside a TopComponent
            javax.swing.undo.UndoManager undoManager = new javax.swing.undo.UndoManager();
            editor.getDocument().addUndoableEditListener(undoManager);
            editor.getInputMap(javax.swing.JComponent.WHEN_FOCUSED).put(javax.swing.KeyStroke.getKeyStroke("control Z"), "Undo");
            editor.getActionMap().put("Undo", new javax.swing.AbstractAction() {
                @Override
                public void actionPerformed(java.awt.event.ActionEvent e) {
                    if (undoManager.canUndo()) {
                        undoManager.undo();
                    }
                }
            });
            editor.getInputMap(javax.swing.JComponent.WHEN_FOCUSED).put(javax.swing.KeyStroke.getKeyStroke("control Y"), "Redo");
            editor.getInputMap(javax.swing.JComponent.WHEN_FOCUSED).put(javax.swing.KeyStroke.getKeyStroke("control shift Z"), "Redo");
            editor.getActionMap().put("Redo", new javax.swing.AbstractAction() {
                @Override
                public void actionPerformed(java.awt.event.ActionEvent e) {
                    if (undoManager.canRedo()) {
                        undoManager.redo();
                    }
                }
            });

            editor.setOpaque(true);
            editor.setEditable(false);

            editor.getDocument().addDocumentListener(new uno.anahata.asi.swing.internal.AnyChangeDocumentListener(() -> {
                if (!verticalScrollEnabled) {
                    SwingUtilities.invokeLater(() -> {
                        revalidate();
                        repaint();
                    });
                }
            }));

            if (finalFo != null) {
                final FileObject fileToDelete = finalFo;
                CLEANER.register(this, () -> {
                    try {
                        if (fileToDelete.isValid()) {
                            log.info("Deleting in memory file object {}", fileToDelete);
                            fileToDelete.delete();
                        } else {
                            log.warn("Did not delete in memory file object {} as it was not valid", fileToDelete);
                        }
                    } catch (Exception e) {
                        log.error("Deleting in memory file object " + fileToDelete, e);
                    }
                });
            }

            // TRIM FIX: Disable NetBeans-specific empty space behaviors for snippets
            editor.putClientProperty("scroll-past-end", Boolean.FALSE);
            editor.setMargin(new Insets(0, 0, 0, 0));

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

            // --- REGISTRY & DIAGNOSTICS ---
            log.info("Editor height before registration: " + editor.getHeight());

            editor.addAncestorListener(new javax.swing.event.AncestorListener() {
                @Override
                public void ancestorAdded(javax.swing.event.AncestorEvent event) {
                    log.info("ancestorAdded for editor: {}", editor);
                }
                @Override
                public void ancestorRemoved(javax.swing.event.AncestorEvent event) {
                    log.info("ancestorRemoved for editor: {}", editor);
                }
                @Override
                public void ancestorMoved(javax.swing.event.AncestorEvent event) {}
            });

            editor.addFocusListener(new java.awt.event.FocusAdapter() {
                @Override
                public void focusGained(java.awt.event.FocusEvent e) {
                    if (!"text/plain".equals(editor.getContentType())) {
                        log.info("Focus gained calling ensureRegistered() for editor: {}", editor);
                        ensureRegistered();
                    } else {
                        log.info("Skipping EditorRegistry registration for plain text snippet: {}", resource.getName());
                    }
                }
            });

            configureScrollBehavior();
            log.info("editor.getHeight() Before revalidate " + editor.getHeight() );
            getWrapper().revalidate();            
            getWrapper().repaint();
            log.info("editor.getHeight() after revalidate " + editor.getHeight() );

        } catch (Exception e) {
            log.error("Failed to init NetBeans high-fidelity viewer", e);
        }
    }

    private void ensureRegistered() {
        log.info("ensureRegistered() editor.getHeight() " + editor.getHeight() );
        try {
            log.info("registering " + editor);
            SwingUtilities.invokeLater(()->EditorApiPackageAccessor.get().register(editor));
            log.info("registered " + editor);
        } catch (Throwable t) {
            log.warn("Failed to register editor with EditorRegistry", t);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JScrollPane getScrollPane() {
        return mainScroller;
    }

    private transient int lastLoggedH = -1;

    /**
     * {@inheritDoc}
     */
    @Override
    public Dimension getPreferredSize() {
        Dimension ps = super.getPreferredSize();
        if (!verticalScrollEnabled && editor != null) {
            try {
                int len = editor.getDocument().getLength();
                java.awt.Rectangle r = editor.modelToView(len);
                int modelToViewHeight = (r != null) ? (r.y + r.height) : 0;

                int editorPrefHeight = editor.getPreferredSize().height;

                java.awt.FontMetrics fm = editor.getFontMetrics(editor.getFont());
                int lineCount = editor.getDocument().getDefaultRootElement().getElementCount();
                int fmHeight = lineCount * fm.getHeight();

                int h = modelToViewHeight;
                if (h == 0) {
                    h = editorPrefHeight;
                }

                int controlStripH = (controlStrip != null && controlStrip.isVisible()) ? controlStrip.getPreferredSize().height : 0;
                int scrollBarH = (mainScroller != null && mainScroller.getHorizontalScrollBar() != null && mainScroller.getHorizontalScrollBar().isVisible()) ? mainScroller.getHorizontalScrollBar().getPreferredSize().height : 0;
                Insets insets = getInsets();
                int insetsH = insets.top + insets.bottom + 5;

                int finalH = h + controlStripH + scrollBarH + insetsH;

                if (finalH != lastLoggedH) {
                    log.info("Height metrics for {}: modelToView={}, editorPref={}, fontMetrics={}, lines={}, controlStrip={}, scrollBar={}, insets={}, finalH={}",
                            resource.getName(), modelToViewHeight, editorPrefHeight, fmHeight, lineCount, controlStripH, scrollBarH, insetsH, finalH);
                    lastLoggedH = finalH;
                }

                return new Dimension(ps.width, finalH);
            } catch (Exception e) {
                log.warn("Precision height calculation failed for NetBeans viewer.", e);
            }
        }
        return ps;
    }

    /*
    @Override
    protected void configureScrollBehavior() {
        super.configureScrollBehavior();
        if (!verticalScrollEnabled && editor != null && editor.getClientProperty("atrv.wheel.forwarder") == null) {
            editor.addMouseWheelListener(e -> {
                if (!verticalScrollEnabled) {
                    uno.anahata.asi.swing.internal.SwingUtils.redispatchMouseWheelEvent(editor, e);
                    e.consume();
                }
            });
            editor.putClientProperty("atrv.wheel.forwarder", Boolean.TRUE);
        }
    }
     */
    /**
     * {@inheritDoc}
     */
    @Override
    protected void onEditorActivated() {
        syncWithResource();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void onPreviewActivated() {
        if (editor != null) {
            editor.setEditable(false);
        }
        syncWithResource();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getEditorContent() {
        return (editor != null) ? editor.getText() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void updatePreviewContent(String content) {
        if (editor != null) {
            String newText = (content != null) ? content : "";
            if (!editor.getText().equals(newText)) {
                int caret = editor.getCaretPosition();
                try {
                    Document doc = editor.getDocument();
                    if (doc instanceof javax.swing.text.AbstractDocument) {
                        ((javax.swing.text.AbstractDocument) doc).replace(0, doc.getLength(), newText, null);
                    } else {
                        doc.remove(0, doc.getLength());
                        doc.insertString(0, newText, null);
                    }
                } catch (Exception ex) {
                    log.error("Failed to update document cleanly, falling back to setText", ex);
                    editor.setText(newText);
                }
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
