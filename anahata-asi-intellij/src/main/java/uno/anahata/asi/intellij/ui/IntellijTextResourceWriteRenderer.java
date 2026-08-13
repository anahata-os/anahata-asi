/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import com.intellij.diff.DiffContentFactory;
import com.intellij.diff.DiffManager;
import com.intellij.diff.DiffRequestPanel;
import com.intellij.diff.contents.DocumentContent;
import com.intellij.diff.requests.SimpleDiffRequest;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.editor.impl.DocumentMarkupModel;
import com.intellij.openapi.editor.markup.GutterIconRenderer;
import com.intellij.openapi.editor.markup.HighlighterLayer;
import com.intellij.openapi.editor.markup.HighlighterTargetArea;
import com.intellij.openapi.editor.markup.MarkupModel;
import com.intellij.openapi.editor.markup.RangeHighlighter;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.Disposer;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.persistence.kryo.KryoUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRenderer;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;
import uno.anahata.asi.toolkit.resources.text.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.resources.text.LineComment;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.util.List;
import java.util.Objects;

/**
 * IntelliJ diff renderer for any {@link AbstractTextResourceWrite} tool-call argument
 * (full-file updates, replacements, line edits).
 * <p>
 * This is the IntelliJ counterpart of the NetBeans {@code AbstractTextResourceWriteRenderer}:
 * it computes the proposed content by applying the tool's structured edits in-memory to the
 * real file, then shows an editable side-by-side diff (current-on-disk vs proposed) using the
 * platform {@link DiffRequestPanel}. While the tool call is {@code PENDING}, edits the user
 * makes to the proposed pane are written back into the tool call via
 * {@link AbstractToolCall#setModifiedArgument} (using a Kryo-cloned DTO carrying a
 * {@code manualOverride}), so the human can refine the AI's proposal before approving.
 * </p>
 * <p>
 * A single instance of this renderer is registered for each concrete write DTO type in
 * {@code IntellijAsiContainer}; the framework instantiates it per parameter via its public
 * no-arg constructor and drives it through {@link #init}/{@link #render}/{@link #updateContent}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class IntellijTextResourceWriteRenderer implements ParameterRenderer<AbstractTextResourceWrite> {

    /**
     * The stable host component returned to the tool-call panel.
     */
    private final JPanel container = new JPanel(new BorderLayout());

    /**
     * The owning tool-call panel (source of the live {@link Agi} session).
     */
    private transient AgiPanel agiPanel;

    /**
     * The tool call whose argument this renderer visualizes and edits.
     */
    private transient AbstractToolCall<?, ?> call;

    /**
     * The name of the argument being rendered.
     */
    private String paramName;

    /**
     * The current DTO value (may be swapped by {@link #updateContent}).
     */
    private transient AbstractTextResourceWrite update;

    /**
     * The reusable platform diff panel (created lazily on first successful render).
     */
    private transient DiffRequestPanel diffPanel;

    /**
     * Parent disposable for {@link #diffPanel}.
     */
    private transient Disposable panelDisposable;

    /**
     * Per-request disposable owning the write-back document listener.
     */
    private transient Disposable contentDisposable;

    /**
     * Last rendered base content, for change-detection to avoid needless rebuilds.
     */
    private transient String lastBase;

    /**
     * Last rendered proposed content, for change-detection.
     */
    private transient String lastProposed;

    /**
     * Constructs the renderer (instantiated reflectively by the parameter-renderer factory via its
     * public no-arg constructor).
     */
    public IntellijTextResourceWriteRenderer() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init(AgiPanel agiPanel, AbstractToolCall<?, ?> call, String paramName, AbstractTextResourceWrite value) {
        this.agiPanel = agiPanel;
        this.call = call;
        this.paramName = paramName;
        this.update = value;
        this.panelDisposable = Disposer.newDisposable("AnahataDiffRenderer");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public JComponent getComponent() {
        return container;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void updateContent(AbstractTextResourceWrite value) {
        this.update = value;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Validates the write, computes base/proposed content and (re)builds the diff. Returns
     * {@code true} when the visible component changed, {@code false} when nothing was updated.
     * </p>
     */
    @Override
    public boolean render() {
        Agi agi = agiPanel.getAgi();
        boolean pending = call.getResponse().getStatus() == ToolExecutionStatus.PENDING;

        try {
            update.validate(agi);
        } catch (Exception e) {
            if (pending) {
                call.getResponse().fail(e.getMessage());
                call.getResponse().addError(e);
            }
            showError("Validation failed: " + e.getMessage());
            return true;
        }

        try {
            if (pending || update.getOriginalContent() == null) {
                update.captureOriginalContent(agi);
            }
            String base = nullToEmpty(update.getOriginalContent());
            String proposed = nullToEmpty(update.calculateResultingContent(agi));

            if (diffPanel != null && Objects.equals(base, lastBase) && Objects.equals(proposed, lastProposed)) {
                return false;
            }
            lastBase = base;
            lastProposed = proposed;
            buildDiff(base, proposed, pending);
            return true;
        } catch (Exception e) {
            log.warn("Failed to render diff for {}", paramName, e);
            showError("Diff render failed: " + e.getMessage());
            return true;
        }
    }

    /**
     * Builds (or refreshes) the side-by-side diff and wires write-back on the proposed pane.
     *
     * @param base     the current on-disk content.
     * @param proposed the proposed content after applying the tool's edits.
     * @param editable whether the proposed pane should be editable (PENDING only).
     */
    private void buildDiff(String base, String proposed, boolean editable) {
        Project project = ProjectManager.getInstance().getDefaultProject();
        FileType fileType = fileTypeFor(update.getOriginalResourceName());
        DiffContentFactory factory = DiffContentFactory.getInstance();

        DocumentContent baseContent = factory.create(project, base, fileType);
        DocumentContent proposedContent = factory.create(project, proposed, fileType);
        addGutterComments(project, proposedContent);

        if (diffPanel == null) {
            diffPanel = DiffManager.getInstance().createRequestPanel(project, panelDisposable, null);
        }

        if (contentDisposable != null) {
            Disposer.dispose(contentDisposable);
        }
        contentDisposable = Disposer.newDisposable("AnahataDiffContent");
        Disposer.register(panelDisposable, contentDisposable);

        if (editable) {
            Document proposedDoc = proposedContent.getDocument();
            proposedDoc.addDocumentListener(new DocumentListener() {
                @Override
                public void documentChanged(DocumentEvent event) {
                    writeBack(proposedDoc.getText());
                }
            }, contentDisposable);
        }

        String proposedTitle = editable ? "Proposed (editable)" : "Proposed";
        SimpleDiffRequest request = new SimpleDiffRequest(
                "Anahata: " + safeName(update.getOriginalResourceName()),
                baseContent, proposedContent, "Current on disk", proposedTitle);
        diffPanel.setRequest(request);

        container.removeAll();
        container.add(diffPanel.getComponent(), BorderLayout.CENTER);
        container.revalidate();
        container.repaint();
    }

    /**
     * Paints the AI's per-line commentary as gutter icons on the proposed pane.
     * <p>
     * Markers are added to the document-level markup model, so they appear in the diff
     * viewer's proposed editor; hovering a marker shows the comment. Only
     * {@link FullTextResourceUpdate} currently carries line comments; other write types add
     * none.
     * </p>
     *
     * @param project the host project.
     * @param content the proposed document content.
     */
    private void addGutterComments(Project project, DocumentContent content) {
        List<LineComment> comments = lineComments();
        if (comments.isEmpty()) {
            return;
        }
        Document doc = content.getDocument();
        MarkupModel markup = DocumentMarkupModel.forDocument(doc, project, true);
        int lineCount = doc.getLineCount();
        for (LineComment comment : comments) {
            int lineIndex = comment.getLineNumber() - 1;
            if (lineIndex < 0 || lineIndex >= lineCount) {
                continue;
            }
            RangeHighlighter highlighter = markup.addRangeHighlighter(
                    doc.getLineStartOffset(lineIndex), doc.getLineEndOffset(lineIndex),
                    HighlighterLayer.ADDITIONAL_SYNTAX, null, HighlighterTargetArea.LINES_IN_RANGE);
            highlighter.setGutterIconRenderer(new CommentGutterRenderer(comment.getComment()));
        }
    }

    /**
     * Extracts the AI's line comments from the current DTO (only full-file updates carry them).
     *
     * @return the line comments, or an empty list.
     */
    private List<LineComment> lineComments() {
        if (update instanceof FullTextResourceUpdate fullUpdate && fullUpdate.getLineComments() != null) {
            return fullUpdate.getLineComments();
        }
        return List.of();
    }

    /**
     * Writes user edits from the proposed pane back into the tool call as a modified
     * argument, via a Kryo-cloned DTO carrying the edited text as a manual override.
     *
     * @param editedText the current text of the proposed pane.
     */
    private void writeBack(String editedText) {
        try {
            AbstractTextResourceWrite edited = KryoUtils.clone(update);
            edited.setManualOverride(editedText);
            call.setModifiedArgument(paramName, edited);
        } catch (Exception e) {
            log.warn("Failed to write back edited content for {}", paramName, e);
        }
    }

    /**
     * Replaces the content with a red error label.
     *
     * @param message the error message to display.
     */
    private void showError(String message) {
        JLabel label = new JLabel("<html><body style='color:#C0392B'>" + escape(message) + "</body></html>");
        label.setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));
        container.removeAll();
        container.add(label, BorderLayout.CENTER);
        container.revalidate();
        container.repaint();
    }

    /**
     * Resolves the editor file type for a resource name, defaulting to plain text.
     *
     * @param resourceName the resource name (may be {@code null}).
     * @return the resolved file type.
     */
    private static FileType fileTypeFor(String resourceName) {
        return FileTypeManager.getInstance().getFileTypeByFileName(resourceName != null ? resourceName : "resource.txt");
    }

    /**
     * Returns a display-safe resource name.
     *
     * @param resourceName the resource name (may be {@code null}).
     * @return a non-null label.
     */
    private static String safeName(String resourceName) {
        return resourceName != null ? resourceName : "resource";
    }

    /**
     * Maps {@code null} to the empty string.
     *
     * @param value the value.
     * @return the value, or "" if null.
     */
    private static String nullToEmpty(String value) {
        return value != null ? value : "";
    }

    /**
     * Minimal HTML escaping for error labels.
     *
     * @param text the raw text.
     * @return HTML-escaped text.
     */
    private static String escape(String text) {
        return text == null ? "" : text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    /**
     * A gutter marker that shows an AI line comment as its tooltip on the proposed pane.
     */
    private static final class CommentGutterRenderer extends GutterIconRenderer {

        /**
         * The comment text shown on hover.
         */
        private final String comment;

        /**
         * Creates a gutter renderer for a line comment.
         *
         * @param comment the comment text.
         */
        private CommentGutterRenderer(String comment) {
            this.comment = comment != null ? comment : "";
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Icon getIcon() {
            return AllIcons.General.Balloon;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String getTooltipText() {
            return comment;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean equals(Object other) {
            return other instanceof CommentGutterRenderer renderer && renderer.comment.equals(comment);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int hashCode() {
            return comment.hashCode();
        }
    }
}
