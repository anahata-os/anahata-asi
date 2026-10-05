/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.codeHighlighting.TextEditorHighlightingPass;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.HighlightDisplayKey;
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerEx;
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerImpl;
import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.daemon.impl.HighlightInfoProcessor;
import com.intellij.codeInsight.daemon.impl.HighlightingSessionImpl;
import com.intellij.codeInsight.daemon.impl.TextEditorHighlightingPassRegistrarEx;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.multiverse.CodeInsightContexts;
import com.intellij.codeInspection.ex.InspectionProfileImpl;
import com.intellij.codeInspection.ex.InspectionToolWrapper;
import com.intellij.lang.annotation.HighlightSeverity;
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
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.profile.codeInspection.InspectionProjectProfileManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.locks.ReentrantLock;
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
     * Mutex to serialize on-demand highlighting pass execution, preventing multiple
     * background context-provider threads from triggering concurrent passes and starving
     * the Event Dispatch Thread of write permits.
     */
    private static final ReentrantLock ON_DEMAND_ANALYSIS_LOCK = new ReentrantLock();

    /** Maximum bounded wait in milliseconds for active daemon highlighting to finish. */
    private static final long DAEMON_ACTIVE_WAIT_TIMEOUT_MS = 2000;

    /**
     * Constructs the Hints toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public IntellijHints() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Combines universal hint instructions from {@code super.getSystemInstructions()}
     * with IntelliJ-specific usage guidance for single-shot quick fixes via {@link #applyHint(String, int, String)}
     * and the live catalog of all registered IntelliJ Java inspections categorized into markdown tables for prefix KV-cache optimization.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>();
        instructions.add("""
                ### IntellijHints Toolkit Instructions:
                - The `IntellijHints` toolkit allows running inspections on arbitrary files on disk using `getFileHints` and applying quick fixes via `applyHint`.
                - Use `applyHint` with the line number and the action name from `[Fixes: ...]` to execute a single-shot quick fix in one turn. Only fixes tagged with `(⚡)` can be executed headlessly; fixes tagged with `(👤)` require user interaction in the IDE.
                - Use `setHintsEnabled` with inspection tool IDs to dynamically enable or disable inspections in the active project profile.
                """);
        instructions.addAll(super.getSystemInstructions());
        instructions.add(getHintMetadata());
        return instructions;
    }

    /**
     * Constructs a compact Markdown summary of all registered IntelliJ Java inspections
     * grouped by category, listing active rule IDs first followed by inactive rule IDs.
     *
     * @return Formatted Markdown containing the categorized inspection IDs.
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

            Map<String, List<String>> activeByCat = new TreeMap<>();
            Map<String, List<String>> inactiveByCat = new TreeMap<>();
            int totalActive = 0;
            int totalInactive = 0;

            for (InspectionToolWrapper<?, ?> tool : tools) {
                String lang = tool.getLanguage();
                if (!("JAVA".equalsIgnoreCase(lang) || "JVM".equalsIgnoreCase(lang) || "UAST".equalsIgnoreCase(lang))) {
                    continue;
                }
                String[] groupPath = tool.getGroupPath();
                String group = (groupPath != null && groupPath.length > 0) ? String.join(" > ", groupPath) : tool.getGroupDisplayName();
                if (group == null || group.isBlank()) {
                    group = "Other";
                }

                boolean isEnabled = profile.isToolEnabled(tool.getDisplayKey(), null);
                String id = "`" + tool.getShortName() + "`";
                if (isEnabled) {
                    totalActive++;
                    activeByCat.computeIfAbsent(group, k -> new ArrayList<>()).add(id);
                } else {
                    totalInactive++;
                    inactiveByCat.computeIfAbsent(group, k -> new ArrayList<>()).add(id);
                }
            }

            sb.append("## IntelliJ IDEA Java Inspections Profile\n");
            sb.append("- **Profile**: ").append(profile.getName())
              .append(" | **Total Rules**: ").append(totalActive + totalInactive)
              .append(" | **Active**: ").append(totalActive)
              .append(" | **Inactive**: ").append(totalInactive).append("\n\n");

            for (String cat : activeByCat.keySet()) {
                List<String> active = activeByCat.getOrDefault(cat, Collections.emptyList());
                List<String> inactive = inactiveByCat.getOrDefault(cat, Collections.emptyList());

                sb.append("### `").append(cat).append("`\n");
                if (!active.isEmpty()) {
                    sb.append("- **Active**: ").append(String.join(", ", active)).append("\n");
                }
                if (!inactive.isEmpty()) {
                    sb.append("- **Inactive**: ").append(String.join(", ", inactive)).append("\n");
                }
                sb.append("\n");
            }

            for (String cat : inactiveByCat.keySet()) {
                if (!activeByCat.containsKey(cat)) {
                    List<String> inactive = inactiveByCat.get(cat);
                    sb.append("### `").append(cat).append("`\n");
                    sb.append("- **Inactive**: ").append(String.join(", ", inactive)).append("\n\n");
                }
            }
        } catch (Exception e) {
            log.warn("Could not build IntelliJ Hints system instructions: {}", e.getMessage());
        }
        return sb.toString();
    }

    /**
     * Enables or disables multiple IntelliJ Java inspections by their unique IDs in the active inspection profile.
     *
     * @param hintIds The list of unique inspection tool IDs to toggle (e.g. {@code ControlFlowStatementWithoutBraces}).
     * @param enabled Whether to enable (true) or disable (false) the specified inspections.
     * @return A summary describing how many inspections were toggled.
     * @throws Exception If updating the inspection profile fails.
     */
    @AgiTool("Enables or disables multiple IntelliJ Java inspections by their tool IDs in the active inspection profile.")
    public String setHintsEnabled(
            @AgiToolParam("The list of inspection tool IDs to toggle (e.g. 'ControlFlowStatementWithoutBraces').") List<String> hintIds,
            @AgiToolParam("Whether to enable (true) or disable (false) the inspections.") boolean enabled) throws Exception {

        if (hintIds == null || hintIds.isEmpty()) {
            throw new AgiToolException("No inspection tool IDs specified to toggle.");
        }
        Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
        if (openProjects.length == 0) {
            throw new AgiToolException("No open project available.");
        }
        Project project = openProjects[0];
        InspectionProfileImpl profile = InspectionProjectProfileManager.getInstance(project).getCurrentProfile();

        List<String> modified = new ArrayList<>();
        List<String> notFound = new ArrayList<>();

        for (String id : hintIds) {
            if (id == null || id.isBlank()) {
                continue;
            }
            String toolId = id.trim().replace("`", "");
            HighlightDisplayKey key = HighlightDisplayKey.find(toolId);
            InspectionToolWrapper<?, ?> tool = profile.getInspectionTool(toolId, project);
            if (tool != null || key != null) {
                profile.setToolEnabled(toolId, enabled, project, true);
                modified.add(tool != null ? tool.getDisplayName() + " (`" + toolId + "`)" : "`" + toolId + "`");
            } else {
                notFound.add(toolId);
            }
        }

        StringBuilder sb = new StringBuilder();
        sb.append(enabled ? "Enabled " : "Disabled ").append(modified.size()).append(" inspection(s) in profile '")
          .append(profile.getName()).append("':\n");
        for (String m : modified) {
            sb.append("- ").append(m).append("\n");
        }
        if (!notFound.isEmpty()) {
            sb.append("Not found (").append(notFound.size()).append("): ").append(notFound).append("\n");
        }
        log((enabled ? "Enabled " : "Disabled ") + modified.size() + " inspection(s) in profile " + profile.getName());
        return sb.toString().trim();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Prioritizes IntelliJ's native cached highlights in
     * {@link com.intellij.openapi.editor.impl.DocumentMarkupModel} when analysis has completed,
     * waits for active background daemon scanning to finish, or safely executes passes on-demand
     * under a mutex (excluding external tool passes to eliminate lock contention).
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
        if (DumbService.isDumb(project)) {
            return Collections.emptyList();
        }

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

        List<HighlightInfo> infos = getHighlightInfos(project, psiFile, document, filePath);
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
                        String text = action.getText().trim();
                        boolean isHeadless = isHeadlessFix(action, text);
                        fixNames.add(text + (isHeadless ? " (⚡)" : " (👤)"));
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
        List<HighlightInfo> infos = getHighlightInfos(project, psiFile, document, filePath);

        IntentionAction[] chosen = new IntentionAction[1];
        int[] offset = {-1};
        String[] fixLabel = new String[1];
        String requestedFix = (fixName != null && !fixName.isBlank())
                ? fixName.replace("(⚡)", "").replace("(👤)", "").trim().toLowerCase()
                : null;
        ReadAction.runBlocking(() -> {
            for (HighlightInfo info : infos) {
                if (document.getLineNumber(info.getStartOffset()) != targetLine) {
                    continue;
                }
                info.findRegisteredQuickFix((descriptor, fixRange) -> {
                    IntentionAction action = descriptor.getAction();
                    String actionText = (action != null) ? action.getText() : null;
                    if (actionText != null && (requestedFix == null || actionText.toLowerCase().contains(requestedFix))) {
                        chosen[0] = action;
                        offset[0] = info.getStartOffset();
                        fixLabel[0] = actionText;
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
        if (!isHeadlessFix(fix, fixLabel[0])) {
            throw new AgiToolException("Quick-fix '" + fixLabel[0] + "' is interactive (👤) and requires user interaction in the IDE UI; it cannot be applied headlessly.");
        }
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
     * Resolves all {@link HighlightInfo}s for a file, prioritizing IntelliJ's native cached
     * highlights in {@link com.intellij.openapi.editor.impl.DocumentMarkupModel} when analysis is complete,
     * waiting for active scanning if currently running, and safely executing passes on-demand under a mutex if needed.
     *
     * @param project The host project.
     * @param psiFile The PSI file to analyze.
     * @param document The document corresponding to the file.
     * @param filePath The absolute file path.
     * @return The list of highlight infos found in the file.
     */
    private List<HighlightInfo> getHighlightInfos(Project project, PsiFile psiFile, Document document, String filePath) {
        DaemonCodeAnalyzerImpl analyzer = (DaemonCodeAnalyzerImpl) DaemonCodeAnalyzer.getInstance(project);

        // 1. Commit any pending document changes to PSI before inspecting
        PsiDocumentManager.getInstance(project).commitDocument(document);

        // 2. Check IntelliJ native cache guarantee: is analysis already finished?
        boolean isFinished = ReadAction.computeBlocking(() ->
                !project.isDisposed() && psiFile.isValid() && analyzer.isAllAnalysisFinished(psiFile)
        );

        // 3. If not finished, do a bounded wait if daemon is actively running or scheduled
        if (!isFinished) {
            long deadline = System.currentTimeMillis() + DAEMON_ACTIVE_WAIT_TIMEOUT_MS;
            while (System.currentTimeMillis() < deadline) {
                ProgressManager.checkCanceled();
                boolean finished = ReadAction.computeBlocking(() ->
                        !project.isDisposed() && psiFile.isValid() && analyzer.isAllAnalysisFinished(psiFile)
                );
                if (finished) {
                    isFinished = true;
                    break;
                }
                boolean runningOrPending = ReadAction.computeBlocking(() ->
                        !project.isDisposed() && analyzer.isRunningOrPending()
                );
                if (!runningOrPending) {
                    break;
                }
                try {
                    Thread.sleep(30);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }

        // 4. If analysis is finished, extract directly from DocumentMarkupModel (0 ms)
        if (isFinished) {
            List<HighlightInfo> cachedInfos = new ArrayList<>();
            ReadAction.runBlocking(() -> {
                DaemonCodeAnalyzerEx.processHighlights(
                        document,
                        project,
                        HighlightSeverity.INFORMATION,
                        0,
                        document.getTextLength(),
                        info -> {
                            if (info.getDescription() != null && !info.getDescription().isBlank()) {
                                cachedInfos.add(info);
                            }
                            return true;
                        }
                );
            });
            return cachedInfos;
        }

        // 5. Fallback for closed files: run on-demand passes under mutex
        ON_DEMAND_ANALYSIS_LOCK.lock();
        try {
            // Double-check under lock in case another thread or daemon just finished it
            boolean finishedUnderLock = ReadAction.computeBlocking(() ->
                    !project.isDisposed() && psiFile.isValid() && analyzer.isAllAnalysisFinished(psiFile)
            );
            if (finishedUnderLock) {
                List<HighlightInfo> cachedInfos = new ArrayList<>();
                ReadAction.runBlocking(() -> {
                    DaemonCodeAnalyzerEx.processHighlights(
                            document,
                            project,
                            HighlightSeverity.INFORMATION,
                            0,
                            document.getTextLength(),
                            info -> {
                                if (info.getDescription() != null && !info.getDescription().isBlank()) {
                                    cachedInfos.add(info);
                                }
                                return true;
                            }
                    );
                });
                return cachedInfos;
            }

            return collectPassesOnDemand(project, psiFile, document);
        } finally {
            ON_DEMAND_ANALYSIS_LOCK.unlock();
        }
    }

    /**
     * Determines whether an intention action or quick-fix can be executed headlessly
     * without displaying interactive UI dialogs, chooser popups, or dictionary selectors.
     *
     * @param action The intention action to evaluate.
     * @param text The human-readable action label.
     * @return {@code true} if the fix is headless (⚡), {@code false} if interactive (👤).
     */
    private static boolean isHeadlessFix(IntentionAction action, String text) {
        String className = action.getClass().getName();
        String lower = text.toLowerCase();
        if (className.contains("Grazie") || lower.contains("dictionary")) {
            return false;
        }
        if (lower.contains("settings") || lower.contains("options") || lower.contains("report") || lower.contains("cloud")) {
            return false;
        }
        if (lower.startsWith("safe delete") || lower.startsWith("show all duplicates") || lower.contains("do not detect duplicates") || lower.startsWith("extract method")) {
            return false;
        }
        if (className.contains("ModCommand")) {
            return true;
        }
        return action.startInWriteAction();
    }

    /**
     * Executes highlighting passes for a single file on demand without global daemon cancellation,
     * excluding external tool passes to avoid sleep loops, and marks the file clean in FileStatusMap.
     *
     * @param project The host project.
     * @param psiFile The file to analyze.
     * @param document The file's document.
     * @return The list of collected highlight infos.
     */
    private List<HighlightInfo> collectPassesOnDemand(Project project, PsiFile psiFile, Document document) {
        List<HighlightInfo> infos = new ArrayList<>();
        DaemonProgressIndicator progress = new DaemonProgressIndicator();
        DaemonCodeAnalyzerImpl analyzer = (DaemonCodeAnalyzerImpl) DaemonCodeAnalyzer.getInstance(project);

        ProgressManager.getInstance().runProcess(() -> {
            HighlightingSessionImpl.runInsideHighlightingSession(
                    psiFile,
                    EditorColorsManager.getInstance().getGlobalScheme(),
                    ProperTextRange.create(0, document.getTextLength()),
                    false,
                    session -> {
                        TextEditorHighlightingPassRegistrarEx registrar = TextEditorHighlightingPassRegistrarEx.getInstanceEx(project);
                        List<TextEditorHighlightingPass> passes = ReadAction.computeBlocking(() ->
                                registrar.instantiateMainPasses(psiFile, document, HighlightInfoProcessor.getEmpty())
                        );

                        for (TextEditorHighlightingPass pass : passes) {
                            ProgressManager.checkCanceled();
                            // Skip ExternalToolPass to avoid external CLI linter polling and sleep loops
                            if (pass.getClass().getSimpleName().contains("ExternalTool")) {
                                continue;
                            }
                            ReadAction.runBlocking(() -> pass.doCollectInformation(progress));
                            List<HighlightInfo> passResult = pass.getInfos();
                            if (passResult != null && !passResult.isEmpty()) {
                                for (HighlightInfo info : passResult) {
                                    if (info.getDescription() != null && !info.getDescription().isBlank()) {
                                        infos.add(info);
                                    }
                                }
                            }
                            analyzer.getFileStatusMap().markFileUpToDate(document, CodeInsightContexts.anyContext(), pass.getId(), progress);
                        }
                    }
            );
        }, progress);
        return infos;
    }
}
