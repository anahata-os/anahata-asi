/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerImpl;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.intellij.internal.JavaPsi;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Surfaces IntelliJ's on-the-fly analysis (inspections + annotators) for a file.
 * <p>
 * This is the IntelliJ counterpart of the NetBeans {@code Hints} toolkit. Where NetBeans
 * drives its hints SPI, IntelliJ runs the code-analysis daemon's main passes on demand via
 * {@link DaemonCodeAnalyzerImpl#runMainPasses} and reports the resulting
 * {@link HighlightInfo}s (errors, warnings, weak warnings) with line numbers and messages.
 * Applying quick-fixes programmatically is deferred — it requires resolving each highlight's
 * {@code IntentionAction} and running it in a write command.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for reporting IntelliJ inspection warnings and errors for a file.")
public class Hints extends AnahataToolkit {

    /**
     * Constructs the Hints toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Hints() {
    }

    /**
     * Lists the inspection/annotator highlights for a file at or above a severity threshold.
     * <p>
     * Runs the analysis daemon's main passes synchronously (on this background tool thread,
     * under a progress indicator) so the result reflects the current file state without
     * requiring the file to be open in an editor.
     * </p>
     *
     * @param filePath    the absolute path of the file to analyze.
     * @param minSeverity the minimum severity to include: {@code ERROR}, {@code WARNING} (default),
     *                    {@code WEAK_WARNING}, or {@code INFO}.
     * @return a Markdown listing of highlights (severity, line, message).
     * @throws AgiToolException if the file cannot be resolved.
     */
    @AgiTool("Lists IntelliJ inspection/annotator highlights (errors, warnings) for a file, with line numbers and messages.")
    public String getFileHints(
            @AgiToolParam("The absolute path of the file to analyze.") String filePath,
            @AgiToolParam(value = "Minimum severity: ERROR, WARNING (default), WEAK_WARNING or INFO.", required = false) String minSeverity) throws AgiToolException {

        Path path = Path.of(filePath);
        if (!Files.exists(path)) {
            throw new AgiToolException("File does not exist: " + filePath);
        }
        VirtualFile vf = JavaPsi.findVirtualFile(filePath);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }
        Project project = JavaPsi.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host file: " + filePath);
        }
        JavaPsi.requireSmart(project);

        Object[] resolved = ReadAction.compute(() -> {
            PsiFile psiFile = JavaPsi.findPsiFile(project, vf);
            Document document = FileDocumentManager.getInstance().getDocument(vf);
            return new Object[]{psiFile, document};
        });
        PsiFile psiFile = (PsiFile) resolved[0];
        Document document = (Document) resolved[1];
        if (psiFile == null || document == null) {
            throw new AgiToolException("Could not resolve PSI/document for: " + filePath);
        }

        HighlightSeverity threshold = parseSeverity(minSeverity);

        List<HighlightInfo> infos = runMainPasses(project, psiFile, document);

        StringBuilder sb = new StringBuilder();
        int count = 0;
        for (HighlightInfo info : infos) {
            if (info.getSeverity().compareTo(threshold) < 0) {
                continue;
            }
            String message = info.getDescription() != null ? info.getDescription() : info.getText();
            if (message == null) {
                continue;
            }
            int line = document.getLineNumber(info.getStartOffset()) + 1;
            sb.append("- [").append(info.getSeverity().getName()).append("] line ").append(line)
              .append(": ").append(message.replace('\n', ' ')).append("\n");
            count++;
        }

        if (count == 0) {
            return "No hints at or above " + threshold.getName() + " for " + filePath + ".";
        }
        return "## Hints for " + vf.getName() + " (" + count + ")\n" + sb;
    }

    /**
     * Applies a quick-fix to a highlight on a given line of a file.
     * <p>
     * Re-runs the analysis daemon, locates a highlight on the target line whose registered
     * quick-fix matches (by name substring, or the first fix), opens the file at that offset,
     * and invokes the fix's {@link IntentionAction} inside a write command.
     * </p>
     *
     * @param filePath the absolute path of the file.
     * @param line     the 1-based line number of the highlight to fix.
     * @param fixName  a case-insensitive substring of the fix name, or {@code null} for the first fix.
     * @return a confirmation naming the applied fix.
     * @throws AgiToolException if the file, highlight, or a matching fix cannot be resolved.
     */
    @AgiTool("Applies a quick-fix to an inspection highlight on a given line of a file.")
    public String applyHint(
            @AgiToolParam("The absolute path of the file.") String filePath,
            @AgiToolParam("The 1-based line number of the highlight to fix.") int line,
            @AgiToolParam(value = "A case-insensitive substring of the fix name, or null for the first fix.", required = false) String fixName) throws AgiToolException {

        VirtualFile vf = JavaPsi.findVirtualFile(filePath);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }
        Project project = JavaPsi.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host file: " + filePath);
        }
        JavaPsi.requireSmart(project);
        Object[] resolved = ReadAction.compute(() -> new Object[]{
                JavaPsi.findPsiFile(project, vf), FileDocumentManager.getInstance().getDocument(vf)});
        PsiFile psiFile = (PsiFile) resolved[0];
        Document document = (Document) resolved[1];
        if (psiFile == null || document == null) {
            throw new AgiToolException("Could not resolve PSI/document for: " + filePath);
        }

        int targetLine = line - 1;
        List<HighlightInfo> infos = runMainPasses(project, psiFile, document);

        IntentionAction[] chosen = new IntentionAction[1];
        int[] offset = {-1};
        String[] fixLabel = new String[1];
        ReadAction.run(() -> {
            for (HighlightInfo info : infos) {
                if (document.getLineNumber(info.getStartOffset()) != targetLine || info.quickFixActionRanges == null) {
                    continue;
                }
                for (Pair<HighlightInfo.IntentionActionDescriptor, TextRange> pair : info.quickFixActionRanges) {
                    IntentionAction action = pair.getFirst().getAction();
                    if (fixName == null || action.getText().toLowerCase().contains(fixName.toLowerCase())) {
                        chosen[0] = action;
                        offset[0] = info.getStartOffset();
                        fixLabel[0] = action.getText();
                        return;
                    }
                }
            }
        });

        if (chosen[0] == null) {
            throw new AgiToolException("No matching quick-fix found on line " + line + " of " + filePath);
        }

        IntentionAction fix = chosen[0];
        int caretOffset = offset[0];
        Editor[] editorHolder = new Editor[1];
        ApplicationManager.getApplication().invokeAndWait(() -> {
            new OpenFileDescriptor(project, vf, caretOffset).navigate(true);
            editorHolder[0] = FileEditorManager.getInstance(project).getSelectedTextEditor();
        });
        Editor editor = editorHolder[0];
        if (editor == null) {
            throw new AgiToolException("Could not open an editor for: " + filePath);
        }

        boolean[] applied = {false};
        ApplicationManager.getApplication().invokeAndWait(() ->
                WriteCommandAction.runWriteCommandAction(project, () -> {
                    if (fix.isAvailable(project, editor, psiFile)) {
                        fix.invoke(project, editor, psiFile);
                        applied[0] = true;
                    }
                }));
        if (!applied[0]) {
            throw new AgiToolException("Quick-fix '" + fixLabel[0] + "' was not applicable in context.");
        }
        log("Applied quick-fix: " + fixLabel[0]);
        return "Applied quick-fix: " + fixLabel[0];
    }

    /**
     * Runs the analysis daemon's main passes for a file synchronously and returns its
     * highlights. Must be called off the EDT (AI tool threads qualify).
     *
     * @param project  the host project.
     * @param psiFile  the file to analyze.
     * @param document the file's document.
     * @return the highlights produced by the main passes.
     */
    private List<HighlightInfo> runMainPasses(Project project, PsiFile psiFile, Document document) {
        List<HighlightInfo> infos = new ArrayList<>();
        ProgressManager.getInstance().runProcess(() -> {
            DaemonCodeAnalyzerImpl analyzer = (DaemonCodeAnalyzerImpl) DaemonCodeAnalyzer.getInstance(project);
            infos.addAll(analyzer.runMainPasses(psiFile, document, new EmptyProgressIndicator()));
        }, new EmptyProgressIndicator());
        return infos;
    }

    /**
     * Parses a severity name into a {@link HighlightSeverity}, defaulting to {@code WARNING}.
     *
     * @param name the severity name (case-insensitive), or {@code null}.
     * @return the resolved severity threshold.
     */
    private HighlightSeverity parseSeverity(String name) {
        if (name == null) {
            return HighlightSeverity.WARNING;
        }
        return switch (name.trim().toUpperCase()) {
            case "ERROR" -> HighlightSeverity.ERROR;
            case "WEAK_WARNING", "WEAKWARNING" -> HighlightSeverity.WEAK_WARNING;
            case "INFO", "INFORMATION" -> HighlightSeverity.INFORMATION;
            default -> HighlightSeverity.WARNING;
        };
    }
}
