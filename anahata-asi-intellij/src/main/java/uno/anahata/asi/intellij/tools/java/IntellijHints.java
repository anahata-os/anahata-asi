/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerImpl;
import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.daemon.impl.HighlightingSessionImpl;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInspection.ex.InspectionProfileImpl;
import com.intellij.codeInspection.ex.InspectionToolWrapper;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.profile.codeInspection.InspectionProjectProfileManager;
import com.intellij.psi.PsiFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.ide.tools.hints.AbstractHints;
import uno.anahata.asi.ide.tools.hints.HintInfo;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.intellij.internal.ProjectUtils;

/**
 * Surfaces IntelliJ's on-the-fly analysis (inspections + annotators) for a file.
 * <p>
 * This is the IntelliJ counterpart of the NetBeans {@code Hints} toolkit. Where NetBeans
 * drives its hints SPI, IntelliJ runs the code-analysis daemon's main passes on demand via
 * {@link DaemonCodeAnalyzerImpl#runMainPasses} and reports the resulting
 * {@link HighlightInfo}s (errors, warnings, weak warnings) with line numbers, messages,
 * and available quick-fix action names wrapped in {@link IntellijHintInfo}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for reporting IntelliJ inspection warnings and errors for a file.")
public class IntellijHints extends AbstractHints {

    /**
     * Constructs the Hints toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public IntellijHints() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Supplies usage guidance and the live catalog of all registered
     * IntelliJ Java inspections categorized into markdown tables showing active and disabled status
     * for prefix KV-cache optimization.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("Hints Toolkit Instructions:\n");
        sb.append("- Use `getFileHints` to inspect a file for code warnings, errors, and available quick-fix actions.\n");
        sb.append("- Use `applyHint` with the line number and the exact action name from `[Fixes: ...]` to execute a single-shot quick fix.\n");
        sb.append("- For resources already loaded in context, live inspection hints are dynamically evaluated and displayed directly in their resource headers.\n\n");
        sb.append(getHintMetadata());
        return Collections.singletonList(sb.toString());
    }

    /**
     * Constructs a Markdown summary table of all registered IntelliJ Java inspections
     * grouped by category, showing their enabled or disabled status, unique rule ID,
     * and short description.
     *
     * @return Formatted Markdown table containing the complete hints metadata profile.
     */
    public String getHintMetadata() {
        StringBuilder sb = new StringBuilder();
        try {
            Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
            if (openProjects.length == 0) {
                return "";
            }
            Project project = openProjects[0];
            InspectionProfileImpl profile = InspectionProjectProfileManager.getInstance(project).getCurrentProfile();
            List<InspectionToolWrapper<?, ?>> tools = profile.getInspectionTools(null);

            Map<String, List<InspectionToolWrapper<?, ?>>> byCategory = new TreeMap<>();
            int totalJavaTools = 0;
            int totalEnabled = 0;
            int totalDisabled = 0;

            for (InspectionToolWrapper<?, ?> tool : tools) {
                String lang = tool.getLanguage();
                if (!("JAVA".equalsIgnoreCase(lang) || "JVM".equalsIgnoreCase(lang) || "UAST".equalsIgnoreCase(lang))) {
                    continue;
                }
                totalJavaTools++;
                boolean isEnabled = profile.isToolEnabled(tool.getDisplayKey(), null);
                if (isEnabled) {
                    totalEnabled++;
                } else {
                    totalDisabled++;
                }

                String[] groupPath = tool.getGroupPath();
                String group = (groupPath != null && groupPath.length > 0) ? String.join(" > ", groupPath) : tool.getGroupDisplayName();
                if (group == null || group.isBlank()) {
                    group = "Other";
                }
                byCategory.computeIfAbsent(group, k -> new ArrayList<>()).add(tool);
            }

            sb.append("## IntelliJ IDEA Java Inspections Profile\n");
            sb.append("- **Profile**: ").append(profile.getName())
              .append(" | **Total Java Rules**: ").append(totalJavaTools)
              .append(" | **Active**: ").append(totalEnabled)
              .append(" | **Disabled**: ").append(totalDisabled).append("\n\n");

            for (Map.Entry<String, List<InspectionToolWrapper<?, ?>>> entry : byCategory.entrySet()) {
                String cat = entry.getKey();
                List<InspectionToolWrapper<?, ?>> catTools = entry.getValue();

                List<InspectionToolWrapper<?, ?>> enabledRules = new ArrayList<>();
                List<InspectionToolWrapper<?, ?>> disabledRules = new ArrayList<>();

                for (InspectionToolWrapper<?, ?> t : catTools) {
                    if (profile.isToolEnabled(t.getDisplayKey(), null)) {
                        enabledRules.add(t);
                    } else {
                        disabledRules.add(t);
                    }
                }

                enabledRules.sort(Comparator.comparing(InspectionToolWrapper::getDisplayName));
                disabledRules.sort(Comparator.comparing(InspectionToolWrapper::getDisplayName));

                sb.append("### `").append(cat).append("`\n\n");
                sb.append("| Enabled | ID | Short Description |\n");
                sb.append("|---|---|---|\n");

                for (InspectionToolWrapper<?, ?> r : enabledRules) {
                    String name = r.getDisplayName().replace("|", "/");
                    sb.append("| ✅ | `").append(r.getShortName()).append("` | ").append(name).append(" |\n");
                }
                for (InspectionToolWrapper<?, ?> r : disabledRules) {
                    String name = r.getDisplayName().replace("|", "/");
                    sb.append("|   | `").append(r.getShortName()).append("` | ").append(name).append(" |\n");
                }
                sb.append("\n");
            }
        } catch (Exception e) {
            log.warn("Could not build IntelliJ Hints system instructions: {}", e.getMessage());
        }
        return sb.toString();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Runs IntelliJ's code analysis daemon main passes on demand
     * using {@link DaemonCodeAnalyzerImpl#runMainPasses} and extracts diagnostics
     * into {@link IntellijHintInfo} DTOs populated with available quick-fix action names.
     * </p>
     */
    @Override
    @AgiTool("Lists IntelliJ inspection/annotator highlights (errors, warnings) and available quick fixes for a file.")
    public List<HintInfo> getFileHints(
            @AgiToolParam(value = "The absolute path of the file to analyze.", rendererId = "path") String filePath) throws Exception {

        Path path = Path.of(filePath);
        if (!Files.exists(path)) {
            throw new AgiToolException("File does not exist: " + filePath);
        }
        VirtualFile vf = ProjectUtils.findVirtualFile(filePath);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }
        Project project = ProjectUtils.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host file: " + filePath);
        }
        JavaPsi.requireSmart(project);

        Object[] resolved = ReadAction.computeBlocking(() -> {
            PsiFile psiFile = JavaPsi.findPsiFile(project, vf);
            Document document = FileDocumentManager.getInstance().getDocument(vf);
            return new Object[]{psiFile, document};
        });
        PsiFile psiFile = (PsiFile) resolved[0];
        Document document = (Document) resolved[1];
        if (psiFile == null || document == null) {
            throw new AgiToolException("Could not resolve PSI/document for: " + filePath);
        }

        List<HighlightInfo> infos = runMainPasses(project, psiFile, document);
        List<HintInfo> hints = new ArrayList<>();

        for (HighlightInfo info : infos) {
            String message = info.getDescription();
            if (message == null || message.isBlank()) {
                continue;
            }
            int line = document.getLineNumber(info.getStartOffset()) + 1;
            int column = info.getStartOffset() - document.getLineStartOffset(line - 1) + 1;
            String severity = info.getSeverity().getName();
            String id = info.getInspectionToolId() != null ? info.getInspectionToolId()
                    : (info.getToolId() != null ? info.getToolId().toString() : null);

            List<String> fixNames = new ArrayList<>();
            ReadAction.runBlocking(() -> {
                info.findRegisteredQuickFix((descriptor, fixRange) -> {
                    IntentionAction action = descriptor.getAction();
                    if (action != null && action.getText() != null && !action.getText().isBlank()) {
                        fixNames.add(action.getText());
                    }
                    return null;
                });
            });

            hints.add(new IntellijHintInfo(filePath, message.replace('\n', ' '), severity, line, column, id, fixNames));
        }

        return hints;
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
     * @param line the 1-based line number of the highlight to fix.
     * @param fixName a case-insensitive substring of the fix name, or {@code null} for the first fix.
     * @return a confirmation naming the applied fix.
     * @throws AgiToolException if the file, highlight, or a matching fix cannot be resolved.
     */
    @AgiTool("Applies a quick-fix to an inspection highlight on a given line of a file.")
    public String applyHint(
            @AgiToolParam("The absolute path of the file.") String filePath,
            @AgiToolParam("The 1-based line number of the highlight to fix.") int line,
            @AgiToolParam(value = "A case-insensitive substring of the fix name, or null for the first fix.", required = false) String fixName) throws AgiToolException {

        VirtualFile vf = ProjectUtils.findVirtualFile(filePath);
        if (vf == null) {
            throw new AgiToolException("Could not resolve a VirtualFile for: " + filePath);
        }
        Project project = ProjectUtils.findHostProject(vf);
        if (project == null) {
            throw new AgiToolException("No open project can host file: " + filePath);
        }
        JavaPsi.requireSmart(project);
        Object[] resolved = ReadAction.computeBlocking(() -> new Object[]{
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
        ReadAction.runBlocking(() -> {
            for (HighlightInfo info : infos) {
                if (document.getLineNumber(info.getStartOffset()) != targetLine) {
                    continue;
                }
                info.findRegisteredQuickFix((descriptor, fixRange) -> {
                    IntentionAction action = descriptor.getAction();
                    if (fixName == null || action.getText().toLowerCase().contains(fixName.toLowerCase())) {
                        chosen[0] = action;
                        offset[0] = info.getStartOffset();
                        fixLabel[0] = action.getText();
                        return action;
                    }
                    return null;
                });
                if (chosen[0] != null) {
                    return;
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
     * @param project the host project.
     * @param psiFile the file to analyze.
     * @param document the file's document.
     * @return the highlights produced by the main passes.
     */
    private List<HighlightInfo> runMainPasses(Project project, PsiFile psiFile, Document document) {
        List<HighlightInfo> infos = new ArrayList<>();
        DaemonProgressIndicator progress = new DaemonProgressIndicator();
        ProgressManager.getInstance().runProcess(() -> {
            HighlightingSessionImpl.runInsideHighlightingSession(
                    psiFile,
                    EditorColorsManager.getInstance().getGlobalScheme(),
                    ProperTextRange.create(0, document.getTextLength()),
                    false,
                    session -> {
                        DaemonCodeAnalyzerImpl analyzer = (DaemonCodeAnalyzerImpl) DaemonCodeAnalyzer.getInstance(project);
                        ReadAction.runBlocking(() -> infos.addAll(analyzer.runMainPasses(psiFile, document, progress)));
                    }
            );
        }, progress);
        return infos;
    }
}
