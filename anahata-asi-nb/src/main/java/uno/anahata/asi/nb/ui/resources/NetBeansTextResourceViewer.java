/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.ui.resources;

import java.awt.BorderLayout;
import java.io.IOException;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.Timer;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.editor.fold.FoldHierarchy;
import org.netbeans.api.editor.mimelookup.MimeLookup;
import org.netbeans.editor.EditorUI;
import org.netbeans.editor.Utilities;
import org.netbeans.modules.parsing.api.Source;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileSystem;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;

/**
 * A NetBeans-native text resource viewer that provides 100% IDE fidelity.
 * <p>
 * This viewer implementation adopts the fully assembled {@code EditorUI.extComponent} 
 * from the IDE assembly line to provide gutters, line numbers, and folds.
 * </p>
 * <p>
 * <b>Virtual Fidelity:</b> If a resource is virtual (e.g., a chat snippet 
 * or tool proposal), it creates a transient Memory-FS {@link FileObject} to 
 * trigger the NetBeans parsing and syntax highlighting engine.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class NetBeansTextResourceViewer extends AbstractTextResourceViewer {

    /** The actual NetBeans editor pane. */
    private JEditorPane editor;
    /** Wrapper to hold the extComponent once it is assembled by the IDE. */
    private final JPanel wrapper = new JPanel(new BorderLayout());
    /** Flag to prevent redundant adoption attempts. */
    private boolean onboarded = false;

    /**
     * Constructs a new NetBeansTextResourceViewer.
     * 
     * @param agiPanel The parent panel.
     * @param resource The resource to render.
     */
    public NetBeansTextResourceViewer(AgiPanel agiPanel, Resource resource) {
        super(agiPanel, resource);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Returns the wrapper panel that will eventually 
     * contain the adopted IDE frame.</p>
     */
    @Override
    protected JComponent createPreviewComponent() {
        if (editor == null) {
            initEditor();
        }
        return wrapper;
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Returns the same wrapper panel used for preview. 
     * NetBeans editors handle mode switching via editability flags on the pane.</p>
     */
    @Override
    protected JComponent createEditorComponent() {
        return wrapper;
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
     * Initializes the native editor and performs the ExtComponent adoption.
     * It configures the NetBeans Document with a transient FileObject to 
     * enable advanced IDE features like error highlighting and code folding.
     */
    private void initEditor() {
        try {
            String mime = resource.getMimeType();
            EditorKit kit = MimeLookup.getLookup(mime).lookup(EditorKit.class);
            
            this.editor = new JEditorPane();
            editor.setEditorKit(kit);
            
            // 1. Resolve Native Data (Physical vs Virtual)
            FileObject fo = resolveFileObject();

            // 2. Configure Document
            Document doc = editor.getDocument();
            doc.putProperty("mimeType", mime);
            
            if (fo != null) {
                try {
                    DataObject dobj = DataObject.find(fo);
                    // CRITICAL: Linking the DataObject activates Errors, Hints, and Folds
                    doc.putProperty(Document.StreamDescriptionProperty, dobj);
                } catch (Exception ex) {
                    log.warn("Failed to find DataObject for resource: {}", resource.getName());
                }
            }

            // 3. Activation
            editor.setEditable(false);
            Source.create(doc);

            // 4. Adoption Loop: Wait for the IDE to assemble the frame (gutters, sidebars)
            Timer timer = new Timer(300, e -> {
                if (onboarded) {
                    return;
                }
                
                EditorUI eui = Utilities.getEditorUI(editor);
                if (eui != null) {
                    JComponent ext = eui.getExtComponent();
                    if (ext != null) {
                        log.info("Adopting NetBeans ExtComponent for: {}", resource.getName());
                        wrapper.removeAll();
                        wrapper.add(ext, BorderLayout.CENTER);
                        
                        // Force initial fold rendering
                        FoldHierarchy.get(editor).render(() -> {});
                        
                        onboarded = true;
                        ((Timer)e.getSource()).stop();
                        
                        // Apply scroll behavior to the adopted frame
                        configureScrollBehavior();
                        
                        wrapper.revalidate();
                        wrapper.repaint();
                    }
                }
            });
            timer.start();

        } catch (Exception e) {
            log.error("Failed to initialize NetBeans editor component", e);
        }
    }

    /**
     * Resolves a FileObject for the resource, using a Memory-FS probe for virtual resources.
     * This enables the full NetBeans parsing engine even for transient chat snippets.
     */
    private FileObject resolveFileObject() {
        if (!resource.getHandle().isVirtual()) {
            String path = resource.getHandle().getUri().getPath();
            if (path != null) {
                return FileUtil.toFileObject(new java.io.File(path));
            }
        }

        // VIRTUAL FIDELITY: Create a transient memory-backed FileObject for syntax/parsing
        try {
            FileSystem mfs = FileUtil.createMemoryFileSystem();
            return mfs.getRoot().createData(resource.getName());
        } catch (IOException ex) {
            log.debug("Memory-FS probe failed for virtual resource: {}", resource.getName());
            return null;
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Enables editability and loads full content 
     * from the handle for modifications.</p>
     */
    @Override
    protected void onEditorActivated() {
        if (editor == null) {
            return;
        }
        try {
            editor.setText(resource.asText());
            editor.setEditable(true);
            editor.setCaretPosition(0);
        } catch (Exception e) {
            log.error("Failed to load full content for editing", e);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Disables editability and synchronizes with 
     * processed viewport data.</p>
     */
    @Override
    protected void onPreviewActivated() {
        if (editor == null) {
            return;
        }
        editor.setEditable(false);
        syncWithResource();
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Returns current JEditorPane text.</p>
     */
    @Override
    public String getEditorContent() {
        return (editor != null) ? editor.getText() : null;
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Streaming ingestion point for NetBeans pane.</p>
     */
    @Override
    protected void updatePreviewContent(String content) {
        if (editor != null) {
            editor.setText(content != null ? content : "");
            editor.setCaretPosition(0);
        }
    }
}
