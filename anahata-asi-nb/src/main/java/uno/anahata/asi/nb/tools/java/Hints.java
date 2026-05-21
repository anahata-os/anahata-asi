/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
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
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolParam;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.java.source.JavaSource;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.agi.tool.Page;

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
public class Hints extends AnahataToolkit {

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(JavaSourceUtils.CANONICAL_FQN_STANDARD
                + "\n"
                + "Hints Toolkit Instructions:\n"
                + "- Use `getMemberHints` with a Canonical FQN to find issues in specific members.\n"
        );
    }

    /**
     * Represents metadata for a registered Java hint type.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HintMetadata {

        /** The unique identifier of the hint. */
        String id;
        /** The user-visible display name of the hint. */
        String displayName;
        /** The description of what this hint checks. */
        String description;
        /** The category folder where the hint belongs. */
        String category;
        /** The severity level of this hint. */
        String severity;
        /** Whether this hint is active in the IDE settings. */
        boolean enabled;
    }

    /**
     * Represents a single Java hint or code fix suggestion.
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class HintInfo {

        /**
         * The absolute path of the file containing the hint.
         */
        private String filePath;
        /**
         * A human-readable description of the hint.
         */
        private String description;
        /**
         * The severity of the hint (e.g., VERIFY, WARNING, ERROR).
         */
        private String severity;
        /**
         * The 1-based line number where the hint starts.
         */
        private int line;
        /**
         * The 1-based column number where the hint starts.
         */
        private int column;
        /**
         * The unique identifier for the hint type.
         */
        private String id;
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
        return applyHintFix(filePath, "text/x-java:Imports_UNUSED");
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
     * its ID.
     * <p>
     * This tool computes all hints for the given file, searches for the one
     * matching the provided {@code hintId}, and invokes its primary fix
     * implementation. The operation is performed within a modification task to
     * ensure atomic application of changes to the underlying source.
     * </p>
     *
     * @param filePath The absolute path of the Java file.
     * @param hintId The unique identifier of the hint whose fix should be
     * applied.
     * @return A descriptive message indicating whether the fix was successfully
     * applied or if the hint/fix was not found.
     * @throws Exception if the Java source cannot be resolved or the
     * modification task fails.
     */
    @AgiTool("Applies a specific netbeans hint fix to a file.")
    public String applyHintFix(
            @AgiToolParam(value = "The absolute path of the Java file.", rendererId = "path") String filePath,
            @AgiToolParam("The ID of the hint to fix.") String hintId
    ) throws Exception {
        FileObject fo = JavaSourceUtils.getFileObject(filePath);
        JavaSource js = JavaSource.forFileObject(fo);
        if (js == null) {
            throw new IOException("Could not get JavaSource for: " + filePath);
        }
        StringBuilder sb = new StringBuilder();
        boolean applied;
        do {
            applied = false;
            final boolean[] appliedRef = new boolean[1];
            js.runModificationTask(copy-> {
                copy.toPhase(JavaSource.Phase.RESOLVED);
                HintsSettings settings = HintsSettings.getSettingsFor(copy.getFileObject());
                HintsInvoker invoker = new HintsInvoker(settings, new AtomicBoolean());
                List<ErrorDescription> hints = invoker.computeHints(copy);
                if (hints != null) {
                    for (ErrorDescription ed : hints) {
                        if (ed == null) {
                            continue;
                        }
                        if (hintId.equals(ed.getId())) {
                            List<Fix> fixes = ed.getFixes().getFixes();
                            if (fixes != null && !fixes.isEmpty()) {
                                fixes.get(0).implement();
                                sb.append("Applied fix ").append(hintId).append(" at line ").append(ed.getRange().getBegin().getLine() + 1).append("\n");
                                appliedRef[0] = true;
                                break; // Exit inner loop to re-scan fresh AST
                            }
                        }
                    }
                }
            }).commit();
            applied = appliedRef[0];
        } while (applied);
        if (sb.length() == 0) {
            return "No hints found or no fix available for: " + hintId;
        }
        JavaSourceUtils.handleSave(fo);
        return sb.toString();
    }

    /**
     * Gets all Java hints for a specific file.
     *
     * @param filePath The absolute path of the Java file.
     * @return A list of hints found in the file.
     * @throws Exception if the file cannot be processed.
     */
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
        Project project = Projects.findOpenProject(projectPath);
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
     * Returns a list of all unique Hint IDs registered in the NetBeans IDE.
     *
     * @return A list of metadata for all registered hints.
     */
    @AgiTool("Returns a list of all unique Hint IDs registered in the NetBeans IDE.")
    public List<HintMetadata> listAvailableHints() {
        List<HintMetadata> results = new ArrayList<>();
        try {
            RulesManager rm = RulesManager.getInstance();
            Map<org.netbeans.modules.java.hints.providers.spi.HintMetadata, ?> hints = rm.readHints(null, null, null);
            for (org.netbeans.modules.java.hints.providers.spi.HintMetadata hm : hints.keySet()) {
                results.add(new HintMetadata(hm.id, hm.displayName, hm.description, hm.category, hm.severity != null ? hm.severity.toString() : null, hm.enabled));
            }
        } catch (Exception e) {
            log.error("Error listing hints", e);
        }
        results.sort((a, b) -> a.getId().compareTo(b.getId()));
        return results;
    }


}
