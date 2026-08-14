/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.ide;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.intellij.internal.JavaPsi;

import java.awt.Point;
import java.awt.Rectangle;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Provides tools for interacting with the IntelliJ IDEA source editor: opening
 * files, navigating to lines, listing open editors, and closing editors.
 * <p>
 * The IntelliJ port replaces the NetBeans {@code EditorRegistry}/{@code TopComponent}
 * crawl with the platform's first-class {@link FileEditorManager}. All editor and
 * UI-model access is marshalled onto the Event Dispatch Thread via
 * {@code ApplicationManager.getApplication().invokeAndWait}, because {@code @AgiTool} methods and
 * {@code populateMessage} are typically invoked from background AI-execution
 * threads where direct editor access is illegal.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for interacting with the IntelliJ IDEA editor.")
public class Editor extends AnahataToolkit {

    /**
     * Constructs the Editor toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Editor() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Injects a real-time snapshot of every open editor across all open projects
     * (path, owning project, modification state and — for the focused editor — the
     * caret line), followed by the active selection and the currently visible code
     * snippet. This lets the ASI "see" the developer's live working context.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        List<String> rows = new ArrayList<>();
        List<String> snippets = new ArrayList<>();

        ApplicationManager.getApplication().invokeAndWait(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                FileEditorManager fem = FileEditorManager.getInstance(project);
                // Fully-qualified because this toolkit class is itself named 'Editor',
                // which forbids importing com.intellij.openapi.editor.Editor.
                com.intellij.openapi.editor.Editor selected = fem.getSelectedTextEditor();
                VirtualFile selectedFile = (selected != null) ? FileDocumentManager.getInstance().getFile(selected.getDocument()) : null;

                for (VirtualFile vf : fem.getOpenFiles()) {
                    boolean modified = FileDocumentManager.getInstance().isFileModified(vf);
                    String caretLine = "-";
                    if (selected != null && vf.equals(selectedFile)) {
                        caretLine = String.valueOf(selected.getCaretModel().getLogicalPosition().line + 1);
                    }
                    rows.add("| " + vf.getPath() + " | " + project.getName() + " | "
                            + (modified ? "Y" : "N") + " | " + caretLine + " |");
                }

                if (selected != null && selectedFile != null) {
                    appendSelectionAndVisible(selected, selectedFile, snippets);
                }
            }
        });

        if (rows.isEmpty()) {
            return;
        }

        StringBuilder sb = new StringBuilder();
        sb.append("## Open Editor Files\n");
        sb.append("| File Path | Project | Modified | Caret Line |\n");
        sb.append("|---|---|---|---|\n");
        for (String row : rows) {
            sb.append(row).append("\n");
        }
        ragMessage.addTextPart(sb.toString());
        for (String snippet : snippets) {
            ragMessage.addTextPart(snippet);
        }
    }

    /**
     * Opens a file in the IntelliJ editor, optionally scrolling to a 1-based line.
     * <p>
     * The virtual file is resolved (refreshing the VFS if needed) and hosted in the
     * project whose content roots contain it, falling back to the first open project
     * so that files outside any project can still be viewed. Navigation runs on the
     * EDT via an {@link OpenFileDescriptor}.
     * </p>
     *
     * @param filePath     the absolute path of the file to open.
     * @param scrollToLine the 1-based line number to scroll to, or {@code null} for the file start.
     * @return a human-readable confirmation of the action.
     * @throws AgiToolException if the file does not exist, cannot be resolved, or no project can host it.
     */
    @AgiTool("Opens a specified file in the IntelliJ editor and optionally scrolls to a specific line.")
    public String openFile(
            @AgiToolParam("The absolute path of the file to open.") String filePath,
            @AgiToolParam(value = "The line number to scroll to (1-based).", required = false) Integer scrollToLine) throws AgiToolException {

        Path path = Path.of(filePath);
        if (!Files.exists(path)) {
            throw new AgiToolException("File does not exist at path: " + filePath);
        }

        VirtualFile vf = VfsUtil.findFile(path, true);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }

        Project project = JavaPsi.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host file: " + filePath);
        }

        int lineIndex = (scrollToLine != null && scrollToLine > 0) ? scrollToLine - 1 : 0;
        ApplicationManager.getApplication().invokeAndWait(() ->
                new OpenFileDescriptor(project, vf, lineIndex, 0).navigate(true));

        log("Opened " + filePath + (scrollToLine != null ? " at line " + scrollToLine : ""));
        return "Successfully opened file: " + filePath + (scrollToLine != null ? " (line " + scrollToLine + ")" : "");
    }

    /**
     * Lists every file currently open in the editor, across all open projects,
     * annotated with its unsaved-changes state.
     *
     * @return a newline-delimited listing, or a message when nothing is open.
     */
    @AgiTool("Gets a list of all files open in the editor")
    public String getOpenFiles() {
        StringBuilder sb = new StringBuilder();
        ApplicationManager.getApplication().invokeAndWait(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                for (VirtualFile vf : FileEditorManager.getInstance(project).getOpenFiles()) {
                    boolean modified = FileDocumentManager.getInstance().isFileModified(vf);
                    sb.append("File: ").append(vf.getPath())
                      .append(" [unsavedChanges=").append(modified).append("]\n");
                }
            }
        });
        return sb.length() == 0 ? "No files are currently open in the editor." : sb.toString();
    }

    /**
     * Closes every open editor across all open projects.
     *
     * @return a summary of which files were closed.
     */
    @AgiTool("Closes all files currently open in the IDE.")
    public String closeAllFiles() {
        List<String> closed = new ArrayList<>();
        ApplicationManager.getApplication().invokeAndWait(() -> {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                FileEditorManager fem = FileEditorManager.getInstance(project);
                for (VirtualFile vf : fem.getOpenFiles()) {
                    fem.closeFile(vf);
                    closed.add(vf.getPath());
                }
            }
        });
        return closed.isEmpty() ? "No files were open to close." : "Closed: " + String.join(", ", closed);
    }

    /**
     * Appends the active selection and the visible viewport of the given editor to
     * the supplied snippet list as fenced Markdown blocks.
     * <p>
     * Must be called on the EDT: it reads the editor's selection, scrolling and
     * document models directly.
     * </p>
     *
     * @param editor   the focused editor to inspect.
     * @param file     the virtual file backing the editor (used for snippet labels).
     * @param snippets the accumulator to append rendered snippets to.
     */
    private void appendSelectionAndVisible(com.intellij.openapi.editor.Editor editor, VirtualFile file, List<String> snippets) {
        String selection = editor.getSelectionModel().getSelectedText();
        if (selection != null && !selection.isEmpty()) {
            snippets.add("**Selection in " + file.getName() + ":**\n```\n" + selection + "\n```");
        }

        Document doc = editor.getDocument();
        Rectangle area = editor.getScrollingModel().getVisibleArea();
        LogicalPosition startPos = editor.xyToLogicalPosition(new Point(area.x, area.y));
        LogicalPosition endPos = editor.xyToLogicalPosition(new Point(area.x + area.width, area.y + area.height));
        int lineCount = doc.getLineCount();
        if (lineCount == 0) {
            return;
        }
        int startLine = Math.max(0, Math.min(startPos.line, lineCount - 1));
        int endLine = Math.max(startLine, Math.min(endPos.line, lineCount - 1));
        int startOffset = doc.getLineStartOffset(startLine);
        int endOffset = doc.getLineEndOffset(endLine);
        String visible = doc.getText(new TextRange(startOffset, endOffset));
        if (!visible.isBlank()) {
            snippets.add("**Visible Lines in " + file.getName() + " (" + (startLine + 1) + "-" + (endLine + 1) + "):**\n```\n" + visible + "\n```");
        }
    }
}
