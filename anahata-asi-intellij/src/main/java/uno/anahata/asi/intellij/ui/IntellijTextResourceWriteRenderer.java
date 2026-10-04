/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.diff.DiffContentFactory;
import com.intellij.diff.DiffManager;
import com.intellij.diff.DiffRequestPanel;
import com.intellij.diff.contents.DocumentContent;
import com.intellij.diff.requests.SimpleDiffRequest;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.editor.event.EditorMouseEvent;
import com.intellij.openapi.editor.event.EditorMouseEventArea;
import com.intellij.openapi.editor.event.EditorMouseMotionListener;
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
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.HintHint;
import com.intellij.ui.LightweightHint;
import org.jetbrains.annotations.NotNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.resource.handle.PathHandle;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.intellij.internal.ProjectUtils;
import uno.anahata.asi.persistence.kryo.KryoUtils;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.SwingAgiConfig;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRenderer;
import uno.anahata.asi.swing.agi.resources.ResourceUiRegistry;
import uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite;
import uno.anahata.asi.toolkit.resources.text.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.resources.text.LineComment;
import uno.anahata.asi.agi.resource.Resource;
import net.miginfocom.swing.MigLayout;
import uno.anahata.asi.intellij.ui.AnahataFileIconProvider;
import uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementBatch;
import uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementIntent;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Point;
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

        if (pending) {
            try {
                update.validate(agi);
            } catch (Exception e) {
                call.getResponse().fail(e.getMessage());
                call.getResponse().addError(e);
                showError("Validation failed: " + e.getMessage());
                return true;
            }
        }

        try {
            if (pending || update.getOriginalContent() == null) {
                update.captureOriginalContent(agi);
            }
            String base = nullToEmpty(update.getOriginalContent());
            String proposed;
            try {
                proposed = nullToEmpty(update.calculateResultingContent(agi));
            } catch (Exception ex) {
                log.debug("Could not recalculate resulting content, falling back to base", ex);
                proposed = base;
            }

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
     * Resolves the hosting project for this diff renderer.
     *
     * @return the project, or null if none is open.
     */
    private Project resolveProject() {
        if (update.getOriginalResourceName() != null) {
            VirtualFile vf = ProjectUtils.findVirtualFile(update.getOriginalResourceName());
            if (vf != null) {
                Project p = ProjectUtils.findHostProject(vf);
                if (p != null) {
                    return p;
                }
            }
        }
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        return open.length > 0 ? open[0] : null;
    }

    /**
     * Builds (or refreshes) the side-by-side diff and wires write-back on the proposed pane.
     *
     * @param base     the current on-disk content.
     * @param proposed the proposed content after applying the tool's edits.
     * @param editable whether the proposed pane should be editable (PENDING only).
     */
    private void buildDiff(String base, String proposed, boolean editable) {
        Project project = resolveProject();
        FileType fileType = fileTypeFor(update.getOriginalResourceName());
        DiffContentFactory factory = DiffContentFactory.getInstance();

        DocumentContent baseContent = factory.create(project, base, fileType);
        DocumentContent proposedContent = (editable && project != null)
                ? factory.createEditable(project, proposed, fileType)
                : factory.create(project, proposed, fileType);
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

        String baseTitle = editable ? "Current on disk" : "Original State";
        String proposedTitle;
        if (editable) {
            proposedTitle = "Proposed (editable)";
        } else if (call.getResponse().getStatus() == ToolExecutionStatus.EXECUTED) {
            proposedTitle = "Applied Changes";
        } else {
            proposedTitle = "Proposed (" + call.getResponse().getStatus() + ")";
        }
        SimpleDiffRequest request = new SimpleDiffRequest(
                "Anahata: " + safeName(update.getOriginalResourceName()),
                baseContent, proposedContent, baseTitle, proposedTitle);
        diffPanel.setRequest(request);
        attachZeroDelayGutterTooltip(project, proposedContent, lineComments(), contentDisposable);

        Resource resource = (update.getResourceUuid() != null)
                ? agiPanel.getAgi().getResourceManager().get(update.getResourceUuid())
                : null;
        JPanel headerPanel = createHeaderPanel(resource, lineComments(), call.getResponse().getStatus());

        container.removeAll();
        container.add(headerPanel, BorderLayout.NORTH);
        container.add(diffPanel.getComponent(), BorderLayout.CENTER);
        container.revalidate();
        container.repaint();
    }

    /**
     * Creates the top header panel containing the file identity, action buttons,
     * surgical AST intent summary, and AI line comments.
     *
     * @param resource the managed resource being updated.
     * @param comments the list of AI line comments.
     * @param status   the tool call execution status.
     * @return the populated header panel.
     */
    private JPanel createHeaderPanel(Resource resource, List<LineComment> comments, ToolExecutionStatus status) {
        JPanel panel = new JPanel(new BorderLayout());
        JPanel topRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 10, 5));
        topRow.setOpaque(false);

        String intentSuffix = "";
        if (update instanceof CodeRefinementBatch batch && batch.getIntents() != null && !batch.getIntents().isEmpty()) {
            intentSuffix = " (" + batch.getIntents().size() + " AST Intents)";
        }

        String labelText;
        switch (status) {
            case PENDING -> labelText = "Proposed Changes" + intentSuffix + ":";
            case EXECUTED -> labelText = "Applied Changes" + intentSuffix + ":";
            case DECLINED -> labelText = "Changes (Declined)" + intentSuffix + ":";
            case FAILED -> labelText = "Changes (Failed)" + intentSuffix + ":";
            default -> labelText = "Changes (" + status + ")" + intentSuffix + ":";
        }
        JLabel statusLabel = new JLabel(labelText);
        statusLabel.setFont(statusLabel.getFont().deriveFont(Font.BOLD));
        topRow.add(statusLabel);

        if (resource != null) {
            String displayName = resource.getHtmlDisplayName();
            if (displayName == null) {
                displayName = resource.getName();
            }
            JLabel htmlDisplayName = new JLabel(displayName);
            htmlDisplayName.setToolTipText(resource.getHandle().getUri().toString());
            htmlDisplayName.setOpaque(false);

            Icon icon =  agiPanel.getAgiConfig().getIconProvider().getIconFor(resource);
            if (icon != null) {
                htmlDisplayName.setIcon(icon);
            }

            topRow.add(htmlDisplayName);

            ResourceUiRegistry.getInstance().getResourceUI().populateActions(topRow, resource, agiPanel);
        }

        panel.add(topRow, BorderLayout.NORTH);

        JComponent intentPanel = createIntentPanel();
        boolean hasComments = comments != null && !comments.isEmpty();

        if (intentPanel != null || hasComments) {
            JPanel dashboard = new JPanel(new MigLayout("fillx, insets 0 15 5 10", "[grow, left][]", "[]"));
            dashboard.setOpaque(false);

            if (intentPanel != null) {
                dashboard.add(intentPanel, "cell 0 0, aligny top, growx");
            }

            if (hasComments) {
                StringBuilder sb = new StringBuilder("<html><div style='text-align: right;'>");
                for (LineComment lc : comments) {
                    sb.append("<i style='color: #888888; font-size: 10pt;'>Line ").append(lc.getLineNumber()).append(":</i> ")
                            .append("<span style='color: #666666; font-size: 10pt;'>").append(escape(lc.getComment())).append("</span><br>");
                }
                sb.append("</div></html>");

                JLabel commentsLabel = new JLabel(sb.toString());
                commentsLabel.setVerticalAlignment(JLabel.TOP);
                dashboard.add(commentsLabel, "cell 1 0, aligny top, alignx right");
            }

            panel.add(dashboard, BorderLayout.CENTER);
        }

        return panel;
    }

    /**
     * Creates an intent panel summarizing structural AST operations for {@link CodeRefinementBatch}.
     *
     * @return the intent summary component, or {@code null} if not applicable.
     */
    private JComponent createIntentPanel() {
        if (update instanceof CodeRefinementBatch batch && batch.getIntents() != null && !batch.getIntents().isEmpty()) {
            JPanel panel = new JPanel(new MigLayout("fillx, insets 0", "[grow]", "[]"));
            panel.setOpaque(false);

            JLabel title = new JLabel("<html><b>Surgical AST Intents (" + batch.getIntents().size() + "):</b></html>");
            panel.add(title, "wrap");

            for (CodeRefinementIntent intent : batch.getIntents()) {
                JLabel label = new JLabel("<html>" + intent.getHtmlDisplay() + "</html>");
                label.setToolTipText("Structural Modification: " + intent.getType());
                panel.add(label, "gapleft 15, wrap");
            }
            return panel;
        }
        return null;
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
        if (comments.isEmpty() || project == null) {
            return;
        }
        Document doc = content.getDocument();
        MarkupModel markup = DocumentMarkupModel.forDocument(doc, project, true);
        int lineCount = doc.getLineCount();
        for (LineComment comment : comments) {
            if (comment.getComment() == null || comment.getComment().isBlank()) {
                continue;
            }
            int lineIndex = comment.getLineNumber() - 1;
            if (lineIndex < 0 || lineIndex >= lineCount) {
                continue;
            }
            RangeHighlighter highlighter = markup.addRangeHighlighter(
                    doc.getLineStartOffset(lineIndex), doc.getLineEndOffset(lineIndex),
                    HighlighterLayer.ADDITIONAL_SYNTAX, null, HighlighterTargetArea.LINES_IN_RANGE);
            highlighter.setErrorStripeTooltip(comment.getComment());
            highlighter.setGutterIconRenderer(new CommentGutterRenderer(comment.getComment()));
        }
    }

    /**
     * Extracts the AI's line comments from the current DTO by delegating to
     * {@link AbstractTextResourceWrite#calculateLineComments}.
     *
     * @return the line comments, or an empty list.
     */
    private List<LineComment> lineComments() {
        return update.calculateLineComments(agiPanel.getAgi());
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
     * Attaches an immediate mouse motion listener to the proposed editor to display
     * AI line comments with zero dwell delay whenever the mouse enters the gutter marker icon.
     *
     * @param project           the host project.
     * @param proposedContent   the proposed document content.
     * @param comments          the list of line comments.
     * @param contentDisposable the parent disposable for listener cleanup.
     */
    private void attachZeroDelayGutterTooltip(Project project, DocumentContent proposedContent, List<LineComment> comments, Disposable contentDisposable) {
        if (project == null || comments == null || comments.isEmpty()) {
            return;
        }
        Editor[] editors = EditorFactory.getInstance().getEditors(proposedContent.getDocument(), project);
        if (editors.length == 0) {
            return;
        }
        Editor proposedEditor = editors[0];
        proposedEditor.addEditorMouseMotionListener(new EditorMouseMotionListener() {
            private int lastLine = -1;

            @Override
            public void mouseMoved(@NotNull EditorMouseEvent e) {
                if (e.getArea() == EditorMouseEventArea.LINE_MARKERS_AREA) {
                    int lineIndex = e.getLogicalPosition().line;
                    if (lineIndex == lastLine) {
                        return;
                    }
                    lastLine = lineIndex;
                    for (LineComment c : comments) {
                        if (c.getLineNumber() - 1 == lineIndex && c.getComment() != null && !c.getComment().isBlank()) {
                            Point p = e.getMouseEvent().getPoint();
                            HintHint hintHint = new HintHint(proposedEditor.getContentComponent(), p)
                                    .setShowImmediately(true)
                                    .setAwtTooltip(true)
                                    .setPreferredPosition(Balloon.Position.atRight);
                            LightweightHint hint = new LightweightHint(new JLabel("<html>" + escape(c.getComment()) + "</html>"));
                            HintManagerImpl.getInstanceImpl().showEditorHint(hint, proposedEditor, p,
                                    HintManager.HIDE_BY_ANY_KEY | HintManager.HIDE_BY_TEXT_CHANGE | HintManager.HIDE_BY_OTHER_HINT | HintManager.HIDE_BY_SCROLLING,
                                    0, false, hintHint);
                            return;
                        }
                    }
                } else {
                    lastLine = -1;
                }
            }
        }, contentDisposable);
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
         * @param comment the non-blank comment text.
         */
        private CommentGutterRenderer(@lombok.NonNull String comment) {
            this.comment = comment;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Icon getIcon() {
            return AnahataFileIconProvider.getFileIcon();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String getTooltipText() {
            return "<html>" + escape(comment) + "</html>";
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
