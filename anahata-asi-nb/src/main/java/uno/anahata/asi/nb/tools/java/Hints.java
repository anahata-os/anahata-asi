/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.ModificationResult;
import org.netbeans.api.java.source.WorkingCopy;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.modules.java.editor.imports.JavaFixAllImports;
import org.netbeans.modules.java.hints.spiimpl.hints.HintsInvoker;
import org.netbeans.modules.java.hints.spiimpl.options.HintsSettings;
import org.netbeans.spi.editor.hints.ErrorDescription;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import uno.anahata.asi.model.Page;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for managing and applying Java hints and code fixes within the NetBeans IDE.
 * <p>
 * This toolkit provides tools for automated code cleanup, such as removing unused imports, 
 * based on the IDE's internal static analysis and AST-aware transformation engines.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for managing and applying Java hints and code fixes.")
public class Hints extends AnahataToolkit {

    /**
     * Represents a single Java hint or code fix suggestion.
     */
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class HintInfo {
        /** The absolute path of the file containing the hint. */
        private String filePath;
        /** A human-readable description of the hint. */
        private String description;
        /** The severity of the hint (e.g., VERIFY, WARNING, ERROR). */
        private String severity;
        /** The 1-based line number where the hint starts. */
        private int line;
        /** The 1-based column number where the hint starts. */
        private int column;
        /** The unique identifier for the hint type. */
        private String id;
    }

    /**
     * Surgically removes all unused imports from a Java source file.
     * <p>
     * This tool uses the NetBeans 'JavaFixAllImports' API to identify and remove 
     * import statements that are not referenced within the file's scope (including 
     * nested and anonymous classes). The operation is performed synchronously 
     * within a modification task.
     * </p>
     * 
     * @param filePath The absolute path of the Java file to clean.
     * @return A message indicating the result of the operation.
     * @throws Exception if the operation fails or the file is not a valid Java source.
     */
    @AiTool("Surgically removes all unused imports from a Java source file.")
    public String removeUnusedImports(
            @AiToolParam(value = "The absolute path of the Java file to clean.", rendererId = "path") String filePath
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

        final StringBuilder sb = new StringBuilder();
        ModificationResult result = js.runModificationTask(copy -> {
            copy.toPhase(JavaSource.Phase.RESOLVED);
            try {
                // 1. Get the private computeImports method
                Method computeImports = JavaFixAllImports.class.getDeclaredMethod("computeImports", org.netbeans.api.java.source.CompilationInfo.class);
                computeImports.setAccessible(true);

                // 2. Compute the import data
                Object importData = computeImports.invoke(null, copy);

                // 3. Get the private performFixImports method
                // We use reflection to find the internal ImportData and CandidateDescription classes
                Class<?>[] declaredClasses = JavaFixAllImports.class.getDeclaredClasses();
                Class<?> importDataClass = null;
                Class<?> candidateDescClass = null;
                for (Class<?> c : declaredClasses) {
                    if (c.getSimpleName().equals("ImportData")) {
                        importDataClass = c;
                    }
                    if (c.getSimpleName().equals("CandidateDescription")) {
                        candidateDescClass = c;
                    }
                }
                
                if (importDataClass == null || candidateDescClass == null) {
                    throw new IllegalStateException("Could not find internal ImportData or CandidateDescription classes.");
                }

                Object emptyCandidates = Array.newInstance(candidateDescClass, 0);

                Method performFixImports = JavaFixAllImports.class.getDeclaredMethod("performFixImports", WorkingCopy.class, importDataClass, emptyCandidates.getClass(), boolean.class);
                performFixImports.setAccessible(true);

                // 4. Perform the fix (remove unused = true)
                performFixImports.invoke(null, copy, importData, emptyCandidates, true);
                sb.append("Successfully removed unused imports from ").append(file.getName());

            } catch (Exception e) {
                log.error("Error during removeUnusedImports reflection", e);
                sb.append("Error: ").append(e.toString());
            }
        });

        result.commit();
        return sb.toString();
    }

    /**
     * Gets all Java hints (warnings, suggestions) for a specific project, with pagination.
     * <p>
     * This tool performs a comprehensive static analysis of all Java files within 
     * the project's source groups, using the IDE's internal hint engine.
     * </p>
     * 
     * @param projectPath The absolute path of the project to scan.
     * @param startIndex The starting index for pagination.
     * @param pageSize The maximum number of hints to return.
     * @return A paginated list of all found hints.
     * @throws Exception if the project cannot be found or the scan fails.
     */
    @AiTool("Gets all Java hints (warnings, suggestions) for a specific project, with pagination.")
    public Page<HintInfo> getAllHints(
            @AiToolParam(value = "The absolute path of the project.", rendererId = "path") String projectPath,
            @AiToolParam("The starting index for pagination.") int startIndex,
            @AiToolParam("The maximum number of hints to return.") int pageSize
    ) throws Exception {
        Project project = Projects.findOpenProject(projectPath);
        List<HintInfo> allHints = new ArrayList<>();
        
        // Find all Java files
        List<FileObject> javaFiles = new ArrayList<>();
        Sources sources = ProjectUtils.getSources(project);
        SourceGroup[] groups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);
        for (SourceGroup sg : groups) {
            FileObject root = sg.getRootFolder();
            Enumeration<? extends FileObject> children = root.getChildren(true);
            while (children.hasMoreElements()) {
                FileObject fo = children.nextElement();
                if (!fo.isFolder() && "text/x-java".equals(fo.getMIMEType())) {
                    javaFiles.add(fo);
                }
            }
        }
        
        for (FileObject fo : javaFiles) {
            JavaSource js = JavaSource.forFileObject(fo);
            if (js != null) {
                js.runUserActionTask(info -> {
                    info.toPhase(JavaSource.Phase.RESOLVED);
                    HintsSettings settings = HintsSettings.getSettingsFor(info.getFileObject());
                    HintsInvoker invoker = new HintsInvoker(settings, new AtomicBoolean());
                    List<ErrorDescription> hints = invoker.computeHints(info);
                    for (ErrorDescription ed : hints) {
                        allHints.add(new HintInfo(
                                fo.getPath(),
                                ed.getDescription(),
                                ed.getSeverity().toString(),
                                ed.getRange().getBegin().getLine(),
                                ed.getRange().getBegin().getColumn(),
                                ed.getId()
                        ));
                    }
                }, true);
            }
        }
        
        return Page.of(allHints, startIndex, pageSize);
    }
}
