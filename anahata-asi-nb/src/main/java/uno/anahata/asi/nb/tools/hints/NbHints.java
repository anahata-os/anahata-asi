/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.hints;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.modules.java.hints.spiimpl.RulesManager;
import org.netbeans.modules.java.hints.spiimpl.hints.HintsInvoker;
import org.netbeans.modules.java.hints.spiimpl.options.HintsSettings;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.netbeans.spi.editor.hints.Fix;
import uno.anahata.asi.nb.tools.project.NbProjects;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.ide.tools.hints.AbstractHints;
import uno.anahata.asi.ide.tools.hints.HintInfo;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.java.source.JavaSource;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.agi.tool.Page;
import uno.anahata.asi.nb.tools.java.JavaSourceUtils;

/**
 * A toolkit for managing and applying Java hints and code fixes within the
 * NetBeans IDE.
 * <p>
 * This toolkit provides tools for automated code cleanup, such as removing
 * unused imports, based on the IDE's internal static analysis and AST-aware
 * transformation engines.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for managing and applying Java hints and code fixes.")
public class NbHints extends AbstractHints {

    /**
     * Constructs the Hints toolkit.
     */
    public NbHints() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Supplies canonical FQN standard, member hints instructions,
     * and the live catalog of all registered NetBeans Java hints categorized
     * into markdown tables showing enabled and disabled status for maximum prefix KV-cache efficiency.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("Hints Toolkit Instructions:\n");
        sb.append("- Use `getMemberHints` with a Canonical FQN to find issues in specific members.\n\n");
        sb.append(JavaSourceUtils.CANONICAL_FQN_STANDARD).append("\n\n");
        sb.append(getHintMetadata());
        return Collections.singletonList(sb.toString());
    }

    /**
     * Constructs a Markdown summary table of all registered NetBeans Java hints
     * grouped by category, showing their enabled or disabled status, unique rule ID,
     * and short description.
     *
     * @return Formatted Markdown table containing the complete hints metadata profile.
     */
    public String getHintMetadata() {
        StringBuilder sb = new StringBuilder();
        try {
            RulesManager rm = RulesManager.getInstance();
            var hints = rm.readHints(null, null, null);
            HintsSettings settings = HintsSettings.getGlobalSettings();

            Map<String, List<org.netbeans.modules.java.hints.providers.spi.HintMetadata>> byCategory = new TreeMap<>();
            int totalRules = hints.size();
            int totalEnabled = 0;
            int totalDisabled = 0;

            for (var entry : hints.entrySet()) {
                var hm = (org.netbeans.modules.java.hints.providers.spi.HintMetadata) entry.getKey();
                boolean isEnabled = settings.isEnabled(hm);
                if (isEnabled) {
                    totalEnabled++;
                } else {
                    totalDisabled++;
                }

                String cat = hm.category != null ? hm.category : "other";
                byCategory.computeIfAbsent(cat, k -> new ArrayList<>()).add(hm);
            }

            sb.append("## NetBeans Java Hints & Inspections Profile\n");
            sb.append("- **Total Rules**: ").append(totalRules)
              .append(" | **Active**: ").append(totalEnabled)
              .append(" | **Disabled**: ").append(totalDisabled).append("\n\n");

            for (Map.Entry<String, List<org.netbeans.modules.java.hints.providers.spi.HintMetadata>> entry : byCategory.entrySet()) {
                String cat = entry.getKey();
                List<org.netbeans.modules.java.hints.providers.spi.HintMetadata> items = entry.getValue();

                List<org.netbeans.modules.java.hints.providers.spi.HintMetadata> enabledRules = new ArrayList<>();
                List<org.netbeans.modules.java.hints.providers.spi.HintMetadata> disabledRules = new ArrayList<>();

                for (var item : items) {
                    if (settings.isEnabled(item)) {
                        enabledRules.add(item);
                    } else {
                        disabledRules.add(item);
                    }
                }

                enabledRules.sort((a, b) -> {
                    String nameA = a.displayName != null ? a.displayName : "";
                    String nameB = b.displayName != null ? b.displayName : "";
                    return nameA.compareTo(nameB);
                });
                disabledRules.sort((a, b) -> {
                    String nameA = a.displayName != null ? a.displayName : "";
                    String nameB = b.displayName != null ? b.displayName : "";
                    return nameA.compareTo(nameB);
                });

                sb.append("### `").append(cat).append("`\n\n");
                sb.append("| Enabled | ID | Short Description |\n");
                sb.append("|---|---|---|\n");

                for (var r : enabledRules) {
                    String name = r.displayName != null ? r.displayName.replace("|", "/") : "";
                    sb.append("| ✅ | `").append(r.id).append("` | ").append(name).append(" |\n");
                }
                for (var r : disabledRules) {
                    String name = r.displayName != null ? r.displayName.replace("|", "/") : "";
                    sb.append("|   | `").append(r.id).append("` | ").append(name).append(" |\n");
                }
                sb.append("\n");
            }
        } catch (Exception e) {
            log.warn("Could not build Hints system instructions: {}", e.getMessage());
        }
        return sb.toString();
    }

    /**
     * Surgically removes all unused imports from a Java source file.
     * <p>
     * This tool uses the NetBeans 'JavaFixAllImports' API to identify and
     * remove import statements that are not referenced within the file's scope
     * (including nested and anonymous classes). The operation is performed
     * synchronously within a modification task.
     * </p>
     *
     * @param filePath The absolute path of the Java file to clean.
     * @return A message indicating the result of the operation.
     * @throws Exception if the operation fails or the file is not a valid Java
     * source.
     */
    @AgiTool("Surgically removes all unused imports from a Java source file.")
    public String removeUnusedImports(
            @AgiToolParam(value = "The absolute path of the Java file to clean.", rendererId = "path") String filePath
    ) throws Exception {
        return applyHintFix(filePath, "text/x-java:Imports_UNUSED", null);
    }

    /**
     * Gets all Java hints for a specific class member.
     *
     * @param filePath the absolute path of the Java file
     * @param memberFqn the ABSOLUTE FQN of the member
     * @return a list of hints located within the member's source range
     * @throws Exception if resolution fails
     */
    @AgiTool("Gets all Java hints for a specific class member (method, field, etc.)")
    public List<HintInfo> getMemberHints(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ABSOLUTE FQN of the member.") String memberFqn
    ) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            throw new IOException("Could not get JavaSource for: " + filePath);
        }

        List<HintInfo> results = new ArrayList<>();
        js.runUserActionTask(info -> {
            info.toPhase(JavaSource.Phase.RESOLVED);
            com.sun.source.tree.Tree tree = JavaSourceUtils.findTree(info, memberFqn);
            if (tree == null) {
                return;
            }

            com.sun.source.util.SourcePositions sp = info.getTrees().getSourcePositions();
            long start = sp.getStartPosition(info.getCompilationUnit(), tree);
            long end = sp.getEndPosition(info.getCompilationUnit(), tree);

            HintsSettings settings = HintsSettings.getSettingsFor(info.getFileObject());
            HintsInvoker invoker = new HintsInvoker(settings, new AtomicBoolean());
            List<ErrorDescription> hints = invoker.computeHints(info);
            if (hints != null) {
                for (ErrorDescription ed : hints) {
                    if (ed == null) {
                        continue;
                    }
                    long hintStart = ed.getRange().getBegin().getOffset();
                    if (hintStart >= start && hintStart <= end) {
                        results.add(new HintInfo(fo.getPath(), ed.getDescription(), ed.getSeverity().toString(), ed.getRange().getBegin().getLine(), ed.getRange().getBegin().getColumn(), ed.getId()));
                    }
                }
            }
        }, true);

        return results;
    }

    /**
     * Applies the first available fix for a specific Java hint identified by
     * its ID, optionally targeting a specific line.
     * <p>
     * This tool computes all hints for the given file, searches for the one
     * matching the provided {@code hintId} (and optional {@code line}), and
     * invokes its primary fix implementation. The operation is performed within
     * a modification task to ensure atomic application of changes to the underlying source.
     * </p>
     *
     * @param filePath The absolute path of the Java file.
     * @param hintId The unique identifier of the hint whose fix should be applied.
     * @param line Optional 1-based line number to fix only the occurrence at this line. If null, fixes all occurrences across the file.
     * @return A descriptive message indicating whether the fix was successfully applied or if the hint/fix was not found.
     * @throws Exception if the Java source cannot be resolved or the modification task fails.
     */
    @AgiTool("Applies a specific netbeans hint fix to a file.")
    public String applyHintFix(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ID of the hint to fix.") String hintId,
            @AgiToolParam(value = "Optional 1-based line number to fix only the occurrence at this line. If omitted, applies to all occurrences across the file.", required = false) Integer line
    ) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            throw new IOException("Could not get JavaSource for: " + filePath);
        }
        StringBuilder sb = new StringBuilder();
        boolean applied;
        do {
            final boolean[] appliedRef = new boolean[1];
            js.runModificationTask(copy -> {
                copy.toPhase(JavaSource.Phase.RESOLVED);
                HintsSettings settings = HintsSettings.getSettingsFor(copy.getFileObject());
                HintsInvoker invoker = new HintsInvoker(settings, new AtomicBoolean());
                List<ErrorDescription> hints = invoker.computeHints(copy);
                if (hints != null) {
                    for (ErrorDescription ed : hints) {
                        if (ed == null) {
                            continue;
                        }
                        if (hintId.equals(ed.getId()) && (line == null || line.intValue() == ed.getRange().getBegin().getLine())) {
                            List<Fix> fixes = ed.getFixes().getFixes();
                            if (fixes != null && !fixes.isEmpty()) {
                                fixes.get(0).implement();
                                sb.append("Applied fix ").append(hintId).append(" at line ").append(ed.getRange().getBegin().getLine()).append("\n");
                                appliedRef[0] = true;
                                break; // Exit inner loop to re-scan fresh AST
                            }
                        }
                    }
                }
            }).commit();
            applied = appliedRef[0] && line == null;
        } while (applied);
        if (sb.length() == 0) {
            return "No hints found or no fix available for: " + hintId + (line != null ? " at line " + line : "");
        }
        JavaSourceUtils.handleSave(fo);
        return sb.toString();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Computes hints for the specified Java source file
     * using NetBeans {@link HintsInvoker} and {@link HintsSettings}.
     * </p>
     */
    @Override
    @AgiTool("Gets all Java hints for a specific file.")
    public List<HintInfo> getFileHints(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath
    ) throws Exception {
        File file = new File(filePath);
        if (!file.exists()) {
            throw new IOException("File does not exist: " + filePath);
        }
        FileObject fo = FileUtil.toFileObject(FileUtil.normalizeFile(file));
        if (fo == null) {
            throw new IOException("Could not get FileObject for: " + filePath);
        }
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            throw new IOException("Could not get JavaSource for: " + filePath);
        }
        List<HintInfo> fileHints = new ArrayList<>();
        js.runUserActionTask(info -> {
            info.toPhase(JavaSource.Phase.RESOLVED);
            HintsSettings settings = HintsSettings.getSettingsFor(info.getFileObject());
            HintsInvoker invoker = new HintsInvoker(settings, new AtomicBoolean());
            List<ErrorDescription> hints = invoker.computeHints(info);
            if (hints != null) {
                for (ErrorDescription ed : hints) {
                    if (ed == null) {
                        continue;
                    }
                    fileHints.add(new HintInfo(fo.getPath(), ed.getDescription(), ed.getSeverity().toString(), ed.getRange().getBegin().getLine(), ed.getRange().getBegin().getColumn(), ed.getId()));
                }
            }
        }, true);
        return fileHints;
    }

    /**
     * Gets all Java hints (warnings, suggestions) for a specific project, with
     * pagination and optional type filtering.
     *
     * @param projectPath The absolute path of the project to scan.
     * @param startIndex The starting index for pagination.
     * @param pageSize The maximum number of hints to return.
     * @param hintIds Optional list of hint IDs to filter by.
     * @return A paginated list of all found hints.
     * @throws Exception if the project cannot be found or the scan fails.
     */
    @AgiTool("Gets all Java hints (warnings, suggestions) for a specific project, with pagination and optional type filtering.")
    public Page<HintInfo> getAllHints(
            @AgiToolParam(value = "The absolute path of the project.", rendererId = "path") String projectPath,
            @AgiToolParam(value = "Optional list of hint IDs to filter by.", required = false) List<String> hintIds,
            @AgiToolParam(value = "The starting index for pagination. Defaults to 0 if not provided", required = false) Integer startIndex,
            @AgiToolParam(value = "The maximum number of hints to return. Defaults to 108 if not provided", required = false) Integer pageSize
    ) throws Exception {
        if (startIndex == null) {
            startIndex = 0;
        }
        if (pageSize == null) {
            pageSize = 108;
        }
        Project project = NbProjects.findOpenProject(projectPath);
        List<HintInfo> allHints = new ArrayList<>();
        Sources sources = ProjectUtils.getSources(project);
        SourceGroup[] groups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);
        for (SourceGroup sg : groups) {
            FileObject root = sg.getRootFolder();
            Enumeration<? extends FileObject> children = root.getChildren(true);
            while (children.hasMoreElements()) {
                FileObject fo = children.nextElement();
                if (!fo.isFolder() && "text/x-java".equals(fo.getMIMEType())) {
                    JavaSource js = JavaSource.forFileObject(fo);
                    if (js != null) {
                        js.runUserActionTask(info -> {
                            info.toPhase(JavaSource.Phase.RESOLVED);
                            HintsSettings settings = HintsSettings.getSettingsFor(info.getFileObject());
                            HintsInvoker invoker = new HintsInvoker(settings, new AtomicBoolean());
                            List<ErrorDescription> hints = invoker.computeHints(info);
                            if (hints != null) {
                                for (ErrorDescription ed : hints) {
                                    if (ed == null) {
                                        continue;
                                    }
                                    if (hintIds == null || hintIds.isEmpty() || hintIds.contains(ed.getId())) {
                                        allHints.add(new HintInfo(fo.getPath(), ed.getDescription(), ed.getSeverity().toString(), ed.getRange().getBegin().getLine(), ed.getRange().getBegin().getColumn(), ed.getId()));
                                    }
                                }
                            }
                        }, true);
                    }
                }
            }
        }
        return Page.of(allHints, startIndex, pageSize);
    }



    /**
     * Enables or disables multiple NetBeans Java hints by their unique IDs in the global editor settings.
     *
     * @param hintIds The list of unique hint IDs to toggle.
     * @param enabled Whether to enable (true) or disable (false) the specified hints.
     * @return A summary describing how many hints were toggled.
     * @throws Exception If updating the hints settings fails.
     */
    @AgiTool("Enables or disables multiple NetBeans Java hints by their unique IDs in the editor profile.")
    public String setHintsEnabled(
            @AgiToolParam("The list of unique hint IDs to toggle.") List<String> hintIds,
            @AgiToolParam("Whether to enable (true) or disable (false) the hints.") boolean enabled
    ) throws Exception {
        RulesManager rm = RulesManager.getInstance();
        Map<org.netbeans.modules.java.hints.providers.spi.HintMetadata, ?> allHints = rm.readHints(null, null, null);
        HintsSettings settings = HintsSettings.getGlobalSettings();

        List<String> modified = new ArrayList<>();
        List<String> notFound = new ArrayList<>(hintIds);

        for (org.netbeans.modules.java.hints.providers.spi.HintMetadata hm : allHints.keySet()) {
            if (hintIds.contains(hm.id)) {
                settings.setEnabled(hm, enabled);
                modified.add(hm.displayName + " (" + hm.id + ")");
                notFound.remove(hm.id);
            }
        }

        StringBuilder sb = new StringBuilder();
        sb.append(enabled ? "Enabled " : "Disabled ").append(modified.size()).append(" hints:\n");
        for (String m : modified) {
            sb.append("- ").append(m).append("\n");
        }
        if (!notFound.isEmpty()) {
            sb.append("Not found (").append(notFound.size()).append("): ").append(notFound).append("\n");
        }
        return sb.toString();
    }


}
