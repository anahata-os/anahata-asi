/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui.resources;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.highlighter.EditorHighlighter;
import com.intellij.openapi.editor.highlighter.EditorHighlighterFactory;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.fileTypes.PlainTextFileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.intellij.resources.handle.IntellijHandle;
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
    private JPanel wrapper;

    /**
     * Returns the wrapper panel, lazily initializing it if null.
     *
     * @return the wrapper panel hosting the editor.
     */
    private JPanel getWrapper() {
        if (wrapper == null) {
            wrapper = new JPanel(new BorderLayout());
        }
        return wrapper;
    }

    /**
     * The IntelliJ editor instance.
     */
    private Editor editor;

    /**
     * The backing document instance.
     */
    private Document document;

    /**
     * The document listener that re-lays out the viewer when embedded (non-scrolling) content
     * changes. Held so it can be detached in {@link #removeNotify()} — critical when {@link #document}
     * is a shared live file document, otherwise listeners (and this viewer) leak on every open.
     */
    private DocumentListener documentListener;

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
     * Constructs the IntelliJ text resource viewer bound to a container context.
     *
     * @param container the owning ASI container.
     * @param resource the text resource being displayed.
     */
    public IntellijTextResourceViewer(AbstractAsiContainer container, Resource resource) {
        super(container, resource);
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
        return getWrapper();
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
     * Normalizes the resource name by mapping markdown language identifiers to standard
     * file extensions recognized by the IntelliJ {@link FileTypeManager}.
     *
     * @param name the raw resource or snippet name.
     * @return the normalized file name with a canonical extension.
     */
    static String normalizeResourceFileName(String name) {
        if (name == null || name.isBlank()) {
            return "snippet.txt";
        }
        String prefix;
        String ext;
        if (!name.contains(".")) {
            prefix = "snippet.";
            ext = name.trim().toLowerCase();
        } else {
            int lastDot = name.lastIndexOf('.');
            prefix = name.substring(0, lastDot + 1);
            ext = name.substring(lastDot + 1).trim().toLowerCase();
        }
        String canonicalExt = switch (ext) {
            case "javascript", "js", "mjs", "cjs" -> "js";
            case "typescript", "ts", "mts", "cts" -> "ts";
            case "python", "py", "pyw" -> "py";
            case "golang", "go" -> "go";
            case "rust", "rs" -> "rs";
            case "ruby", "rb" -> "rb";
            case "shell", "sh", "bash", "zsh" -> "sh";
            case "bat", "batch", "cmd" -> "bat";
            case "yaml", "yml" -> "yml";
            case "json", "jsonc" -> "json";
            case "markdown", "md" -> "md";
            case "html", "htm" -> "html";
            case "xml" -> "xml";
            case "sql" -> "sql";
            case "kotlin", "kt", "kts" -> "kt";
            case "java" -> "java";
            case "css" -> "css";
            case "scss" -> "scss";
            case "less" -> "less";
            case "properties" -> "properties";
            case "ini" -> "ini";
            case "dockerfile", "docker" -> "dockerfile";
            case "text", "txt", "plain", "plaintext" -> "txt";
            case "c" -> "c";
            case "cpp", "c++", "cc", "cxx" -> "cpp";
            case "csharp", "cs", "c#" -> "cs";
            case "php" -> "php";
            case "scala" -> "scala";
            case "groovy" -> "groovy";
            case "diff", "patch" -> "patch";
            case "lua" -> "lua";
            case "dart" -> "dart";
            case "perl", "pl" -> "pl";
            case "r" -> "r";
            case "swift" -> "swift";
            case "graphql", "gql" -> "graphql";
            case "proto", "protobuf" -> "proto";
            case "vue" -> "vue";
            case "jsx" -> "jsx";
            case "tsx" -> "tsx";
            default -> ext;
        };
        return prefix + canonicalExt;
    }

    /**
     * Initializes the IntelliJ editor and binds it to the resource document.
     */
    private void initEditor() {
        Project project = resolveProject();
        String normalizedName = normalizeResourceFileName(resource.getName());
        FileType rawFileType = FileTypeManager.getInstance().getFileTypeByFileName(normalizedName);
        final FileType fileType = (rawFileType == null || rawFileType.isBinary() || rawFileType.getName().equalsIgnoreCase("UNKNOWN"))
                ? PlainTextFileType.INSTANCE
                : rawFileType;

        VirtualFile vf = null;
        if (resource.getHandle() instanceof IntellijHandle ih) {
            vf = ih.getVirtualFile();
        } else if (resource.getHandle() instanceof PathHandle ph) {
            vf = JavaPsi.findVirtualFile(ph.getPath());
        }

        if (vf != null) {
            final VirtualFile targetVf = vf;
            document = ReadAction.compute(() -> FileDocumentManager.getInstance().getDocument(targetVf));
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

        boolean isViewer = !isEditing();
        if (vf != null) {
            editor = EditorFactory.getInstance().createEditor(document, project, vf, isViewer);
        } else {
            editor = EditorFactory.getInstance().createEditor(document, project, fileType, isViewer);
        }

        if (editor instanceof EditorEx editorEx) {
            final VirtualFile targetVf = vf;
            EditorHighlighter highlighter = ReadAction.compute(() -> {
                if (targetVf != null) {
                    return EditorHighlighterFactory.getInstance().createEditorHighlighter(project, targetVf);
                } else {
                    EditorHighlighter h = EditorHighlighterFactory.getInstance().createEditorHighlighter(editorEx.getColorsScheme(), normalizedName, project);
                    return h != null ? h : EditorHighlighterFactory.getInstance().createEditorHighlighter(project, fileType);
                }
            });
            if (highlighter != null) {
                editorEx.setHighlighter(highlighter);
            }
        }

        editor.getSettings().setLineNumbersShown(true);
        editor.getSettings().setFoldingOutlineShown(true);
        editor.getSettings().setLineMarkerAreaShown(true);

        documentListener = new DocumentListener() {
            @Override
            public void documentChanged(DocumentEvent event) {
                if (!verticalScrollEnabled) {
                    revalidate();
                    repaint();
                }
            }
        };
        document.addDocumentListener(documentListener);

        getWrapper().removeAll();
        getWrapper().add(editor.getComponent(), BorderLayout.CENTER);
        configureScrollBehavior();
    }

    /**
     * Resolves the hosting project for this viewer.
     *
     * @return the project, or null if none is open.
     */
    private Project resolveProject() {
        VirtualFile vf = null;
        if (resource.getHandle() instanceof IntellijHandle ih) {
            vf = ih.getVirtualFile();
        } else if (resource.getHandle() instanceof PathHandle ph) {
            vf = JavaPsi.findVirtualFile(ph.getPath());
        }
        if (vf != null) {
            Project p = JavaPsi.findHostProject(vf);
            if (p != null) {
                return p;
            }
        }
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        return open.length > 0 ? open[0] : null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Recreates the editor when the component is (re)attached to the UI hierarchy, so a viewer
     * that was detached and shown again (e.g. a tab hide/show cycle, which releases the editor in
     * {@link #removeNotify()}) rebuilds its editor instead of displaying a disposed one.
     * </p>
     */
    @Override
    public void addNotify() {
        super.addNotify();
        if (editor == null) {
            initEditor();
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Detaches the document listener and releases the editor when the component leaves the UI
     * hierarchy. Removing the listener is essential when {@link #document} is a shared live file
     * document, which outlives this viewer and would otherwise retain the listener (and this
     * viewer) indefinitely.
     * </p>
     */
    @Override
    public void removeNotify() {
        super.removeNotify();
        if (document != null && documentListener != null) {
            document.removeDocumentListener(documentListener);
            documentListener = null;
        }
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
            Runnable write = () -> {
                Project project = resolveProject();
                if (project != null && !project.isDisposed()) {
                    WriteCommandAction.runWriteCommandAction(project, () -> document.setText(content));
                } else {
                    ApplicationManager.getApplication().runWriteAction(() -> document.setText(content));
                }
            };
            if (ApplicationManager.getApplication().isDispatchThread()) {
                write.run();
            } else {
                ApplicationManager.getApplication().invokeLater(write);
            }
        }
    }
}
