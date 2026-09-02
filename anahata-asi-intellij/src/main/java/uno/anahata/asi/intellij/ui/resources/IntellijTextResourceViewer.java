/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.resources;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.resources.view.AbstractTextResourceViewer;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import java.awt.BorderLayout;

/**
 * An IntelliJ IDEA native text resource viewer that provides full IDE editor fidelity
 * (syntax highlighting, line numbers, folding, and theme synchronization) using IntelliJ's
 * {@link EditorFactory}.
 *
 * @author anahata
 */
@Slf4j
public class IntellijTextResourceViewer extends AbstractTextResourceViewer {

    /**
     * The wrapper panel hosting the active editor component.
     */
    private final JPanel wrapper = new JPanel(new BorderLayout());

    /**
     * The IntelliJ editor instance.
     */
    private Editor editor;

    /**
     * The backing document instance.
     */
    private Document document;

    /**
     * Constructs the IntelliJ text resource viewer.
     *
     * @param agiPanel the owning AGI panel.
     * @param resource the text resource being displayed.
     */
    public IntellijTextResourceViewer(AgiPanel agiPanel, Resource resource) {
        super(agiPanel, resource);
        setPreviewAsEditor(true);
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
     * {@inheritDoc}
     */
    @Override
    protected JComponent createEditorComponent() {
        if (editor == null) {
            initEditor();
        }
        return wrapper;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void setComponentEditable(boolean editable) {
        if (editor instanceof EditorEx editorEx) {
            editorEx.setViewer(!editable);
        }
    }

    /**
     * Initializes the IntelliJ editor and binds it to the resource document.
     */
    private void initEditor() {
        Project project = resolveProject();
        FileType fileType = FileTypeManager.getInstance().getFileTypeByFileName(resource.getName());

        VirtualFile vf = null;
        if (resource.getHandle() instanceof PathHandle ph) {
            vf = JavaPsi.findVirtualFile(ph.getPath());
        }

        if (vf != null) {
            document = FileDocumentManager.getInstance().getDocument(vf);
        }
        if (document == null) {
            String text = "";
            try {
                text = resource.asText();
            } catch (Exception e) {
                log.warn("Failed to read text from resource {}", resource.getName(), e);
            }
            document = EditorFactory.getInstance().createDocument(text);
        }

        if (isEditing()) {
            editor = EditorFactory.getInstance().createEditor(document, project);
        } else {
            editor = EditorFactory.getInstance().createViewer(document, project);
        }
        editor.getSettings().setLineNumbersShown(true);
        editor.getSettings().setFoldingOutlineShown(true);
        editor.getSettings().setLineMarkerAreaShown(true);

        document.addDocumentListener(new DocumentListener() {
            @Override
            public void documentChanged(DocumentEvent event) {
                if (!verticalScrollEnabled) {
                    revalidate();
                    repaint();
                }
            }
        });

        wrapper.removeAll();
        wrapper.add(editor.getComponent(), BorderLayout.CENTER);
        configureScrollBehavior();
    }

    /**
     * Resolves the hosting project for this viewer.
     *
     * @return the project, or null if none is open.
     */
    private Project resolveProject() {
        if (resource.getHandle() instanceof PathHandle ph) {
            VirtualFile vf = JavaPsi.findVirtualFile(ph.getPath());
            if (vf != null) {
                Project p = JavaPsi.findHostProject(vf);
                if (p != null) {
                    return p;
                }
            }
        }
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        return open.length > 0 ? open[0] : null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Releases the editor when the component is detached from the UI hierarchy.
     * </p>
     */
    @Override
    public void removeNotify() {
        super.removeNotify();
        if (editor != null && !editor.isDisposed()) {
            EditorFactory.getInstance().releaseEditor(editor);
            editor = null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the internal scroll pane used by the IntelliJ editor component.
     * </p>
     */
    @Override
    public JScrollPane getScrollPane() {
        if (editor instanceof EditorEx editorEx) {
            return editorEx.getScrollPane();
        }
        return editor != null ? uno.anahata.asi.swing.internal.SwingUtils.findComponent(editor.getComponent(), JScrollPane.class) : null;
    }

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
        if (editor instanceof EditorEx editorEx) {
            editorEx.setViewer(true);
        }
        syncWithResource();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getEditorContent() {
        if (document != null) {
            return document.getText();
        }
        try {
            return resource.asText();
        } catch (Exception e) {
            log.warn("Failed to get editor content from resource {}", resource.getName(), e);
            return "";
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void updatePreviewContent(String content) {
        if (document != null && content != null && !document.getText().equals(content)) {
            Project project = resolveProject();
            ApplicationManager.getApplication().invokeLater(() -> {
                if (project != null && !project.isDisposed()) {
                    WriteCommandAction.runWriteCommandAction(project, () -> document.setText(content));
                } else {
                    ApplicationManager.getApplication().runWriteAction(() -> document.setText(content));
                }
            });
        }
    }
}
