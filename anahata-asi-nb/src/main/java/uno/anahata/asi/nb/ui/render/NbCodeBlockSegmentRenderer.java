/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.nb.ui.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.View;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.editor.fold.FoldHierarchy;
import org.netbeans.editor.AnnotationDesc;
import org.netbeans.editor.Annotations;
import org.netbeans.editor.BaseDocument;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.editor.NbEditorUI;
import org.netbeans.modules.parsing.api.Source;
import org.openide.filesystems.FileObject;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.render.AbstractCodeBlockSegmentRenderer;

/**
 * A NetBeans-specific code block renderer providing 100% IDE fidelity, including
 * semantic code folding, gutter annotations, and syntax highlighting.
 * <p>
 * This renderer leverages the official {@link NbEditorUI} assembly line to bypass
 * internal IDE visibility vetos and ensures the NetBeans Parsing API is active
 * for the snippet to enable semantic features like code folds.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NbCodeBlockSegmentRenderer extends AbstractCodeBlockSegmentRenderer {

    /** The NetBeans EditorKit used for syntax highlighting and IDE integration. */
    protected final EditorKit kit;
    
    /** 
     * An optional FileObject used to back the document for full semantic 
     * features (errors, folds). 
     */
    @Setter
    protected FileObject fileObject;
    
    /** Flag to ensure the high-fidelity onboarding happens exactly once. */
    private boolean onboarded = false;

    /**
     * Constructs a new NbCodeBlockSegmentRenderer.
     *
     * @param agiPanel The parent AgiPanel providing the IDE context.
     * @param initialContent The initial code snippet text.
     * @param language The language identifier (e.g., "java", "json").
     * @param kit The pre-resolved NetBeans EditorKit for the language.
     */
    public NbCodeBlockSegmentRenderer(AgiPanel agiPanel, String initialContent, String language, EditorKit kit) {
        super(agiPanel, initialContent, language);
        this.kit = kit;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a {@link JEditorPane} and configures it to trigger NetBeans' 
     * asynchronous {@link EditorUI} discovery.
     * </p>
     */
    @Override
    protected JComponent createInnerComponent() {
        JEditorPane editor = new JEditorPane() {
            @Override
            public boolean getScrollableTracksViewportWidth() { 
                return false; 
            }

            @Override
            public Dimension getPreferredSize() {
                Dimension d = super.getPreferredSize();
                if (!editing && !verticalScrollEnabled) {
                    try {
                        View v = getUI().getRootView(this);
                        if (v != null) {
                            v.setSize(3000, Integer.MAX_VALUE); 
                            int length = getDocument().getLength();
                            if (length > 0) {
                                Rectangle r = modelToView(length - 1);
                                if (r != null) {
                                    d.height = r.y + r.height + 10; 
                                    return d;
                                }
                            }
                            d.height = (int) v.getPreferredSpan(View.Y_AXIS) + 10;
                        }
                    } catch (Exception e) {
                        log.debug("Preferred size calculation pending for {}", language);
                    }
                }
                return d;
            }
        };

        editor.setEditable(false);
        editor.setOpaque(false);
        editor.setBackground(new Color(0, 0, 0, 0));

        // 1. REACTIVE LISTENER: Adopt high-fidelity components when EditorUI is attached.
        editor.addPropertyChangeListener("editorUI", evt -> {
            if (evt.getNewValue() instanceof EditorUI) {
                performHighFidelityOnboarding((EditorUI) evt.getNewValue());
            }
        });

        // 2. TRIGGER IDE DISCOVERY: Setting ContentType triggers the BaseTextUI onboarding.
        editor.setEditorKit(kit);
        editor.setContentType(kit.getContentType());
        
        Document doc = editor.getDocument();
        doc.putProperty("mimeType", kit.getContentType());
        
        // CRITICAL: If we have a FileObject, associate it with the document to trigger the Parsing API (errors, etc.)
        if (fileObject != null) {
            doc.putProperty(Document.StreamDescriptionProperty, fileObject);
        }
        
        editor.setText(currentContent);
        
        // 3. SEMANTIC ACTIVATION: Trigger the Parsing API for standalone documents.
        Source.create(doc);

        // 4. SYNCHRONOUS CHECK: Sometimes the UI is already there.
        EditorUI eui = Utilities.getEditorUI(editor);
        if (eui != null) {
            performHighFidelityOnboarding(eui);
        }

        return editor;
    }

    /**
     * Surgically adopts the IDE-built high-fidelity components and triggers 
     * semantic features like code folding.
     * 
     * @param eui The NetBeans EditorUI (or NbEditorUI) delegate.
     */
    private synchronized void performHighFidelityOnboarding(EditorUI eui) {
        if (onboarded) {
            return;
        }

        SwingUtilities.invokeLater(() -> {
            if (onboarded || component == null) {
                return;
            }

            try {
                // THE OFFICIAL PATH: Using the IDE's assembly line ensures gutters, 
                // folding lines, and sidebars are built correctly.
                JComponent extComp = eui.getExtComponent();
                JScrollPane officialScroll = findScrollPane(extComp);
                
                if (officialScroll != null && scrollPane != null) {
                    // ADOPT VIEWPORT: NetBeans reparents the EditorPane into a LayeredPane.
                    // We must adopt the whole viewport to keep the text visible.
                    scrollPane.setViewport(officialScroll.getViewport());
                    
                    // ADOPT SIDEBARS: This gives us the Gutter (line numbers) and FoldIndicator.
                    scrollPane.setRowHeader(officialScroll.getRowHeader());
                    scrollPane.setColumnHeader(officialScroll.getColumnHeader());
                    
                    // MATCH ANAHATA STYLE
                    scrollPane.setOpaque(false);
                    scrollPane.getViewport().setOpaque(false);
                    
                    // FORCE FOLDING: Standalone editors need a kick to start the FoldManager.
                    FoldHierarchy hierarchy = FoldHierarchy.get((JEditorPane) innerComponent);
                    hierarchy.render(() -> { }); // No-op to trigger hierarchy sync
                    
                    onboarded = true;
                    log.info("NetBeans high-fidelity onboarding (folds/gutters) complete for {}.", language);
                    
                    component.revalidate();
                    component.repaint();
                }
            } catch (Exception e) {
                log.error("High-fidelity onboarding failed for {}: {}", language, e.getMessage());
            }
        });
    }

    /**
     * Adds an annotation (glyph icon and tooltip) to the gutter for a specific line.
     * <p>
     * This uses the official NetBeans {@link Annotations} system. The icon and 
     * styling are determined by the provided annotation type.
     * </p>
     * 
     * @param line The 1-based line number.
     * @param type The NetBeans annotation type (e.g., "bookmark", "breakpoint").
     * @param description The tooltip text for the annotation.
     */
    public void addGutterAnnotation(int line, String type, String description) {
        if (!(innerComponent instanceof JEditorPane)) {
            return;
        }
        
        JEditorPane editor = (JEditorPane) innerComponent;
        BaseDocument doc = (BaseDocument) editor.getDocument();
        
        SwingUtilities.invokeLater(() -> {
            try {
                Annotations annos = doc.getAnnotations();
                
                // NetBeans uses 0-based offsets for annotation placement.
                int offset = Utilities.getRowStartFromLineOffset(doc, line - 1);
                
                AnnotationDesc desc = new AnnotationDesc(offset, -1) {
                    @Override public String getAnnotationType() { return type; }
                    @Override public String getShortDescription() { return description; }
                    @Override public int getOffset() { return offset; }
                    @Override public int getLine() { return line; }
                };
                
                annos.addAnnotation(desc);
                
                // Refresh gutter to show the new icon
                EditorUI eui = Utilities.getEditorUI(editor);
                if (eui != null && eui.getGlyphGutter() != null) {
                    eui.getGlyphGutter().update();
                }
            } catch (Exception e) {
                log.warn("Failed to add gutter annotation: {}", e.getMessage());
            }
        });
    }

    /**
     * Recursively searches for the JScrollPane in the IDE's assembled component tree.
     * 
     * @param c The root container of the assembly.
     * @return The found JScrollPane, or null if not found.
     */
    private JScrollPane findScrollPane(java.awt.Container c) {
        for (java.awt.Component comp : c.getComponents()) {
            if (comp instanceof JScrollPane) {
                return (JScrollPane) comp;
            }
            if (comp instanceof java.awt.Container) {
                JScrollPane found = findScrollPane((java.awt.Container) comp);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void updateComponentContent(String content) {
        if (innerComponent instanceof JEditorPane) {
            ((JEditorPane) innerComponent).setText(content);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getCurrentContentFromComponent() {
        if (innerComponent instanceof JEditorPane) {
            return ((JEditorPane) innerComponent).getText();
        }
        return "";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        if (innerComponent instanceof JEditorPane) {
            ((JEditorPane) innerComponent).setEditable(editable);
        }
    }
}
