/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.roots.OrderEnumerator;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.swing.toolkit.SwingJava;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * An IntelliJ-aware extension of the core {@code Java} toolkit that can compile and execute
 * a script against a specific open project's classpath (the "Singularity Loop" / hot-reload
 * workflow).
 * <p>
 * This is the IntelliJ port of the NetBeans {@code NbJava}. Where NetBeans crawls
 * {@code ClassPathProvider}/{@code SourceGroup}s, IntelliJ resolves the full runtime
 * classpath — module compile outputs plus library jars — via a single
 * {@link OrderEnumerator} query, which the core {@code compileAndExecute} then appends to its
 * child-first class loader.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("An IntelliJ-aware toolkit for compiling and executing Java code against a project's classpath.")
public class IntellijJava extends SwingJava {

    /**
     * Constructs the IntellijJava toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public IntellijJava() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Appends IntelliJ-specific guidance describing when to prefer
     * {@code compileAndExecuteInProject} over the default {@code compileAndExecute}.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());
        instructions.add(
                "\n**IntelliJ Classpath & Execution**:\n"
                + "- Use `compileAndExecute` for scripts that only need the plugin's default classpath (filesystem + bundled libraries).\n"
                + "- Use `compileAndExecuteInProject` only when your script must import types compiled from a specific open project's output (its module `classes`) or that project's external dependencies.");
        return instructions;
    }

    /**
     * Compiles and executes a Java script against a specific open project's classpath.
     * <p>
     * The project's compiled module outputs (and, optionally, its library dependencies and
     * test scope) are resolved via {@link OrderEnumerator} and appended to the child-first
     * class loader used by the core {@code compileAndExecute}.
     * </p>
     *
     * @param sourceCode                 the script source (a public class extending the core Swing tool base).
     * @param projectPath                the absolute base path of the open project to run in.
     * @param includeProjectDependencies whether to include the project's library dependencies.
     * @param includeTestContext         whether to include test outputs and test-scoped dependencies.
     * @param compilerOptions            optional additional compiler options.
     * @return the result of the execution.
     * @throws Exception on resolution or execution failure.
     */
    @AgiTool("Executes a Java script within the context of a specific open IntelliJ project, appending that project's classpath to the script's child-first class loader.")
    public Object compileAndExecuteInProject(
            @AgiToolParam(value = "The script source (a public class with no package declaration, extending the Swing tool base).", rendererId = "java") String sourceCode,
            @AgiToolParam("The absolute base path of the open IntelliJ project to run in.") String projectPath,
            @AgiToolParam("Whether to include the project's external library dependencies.") boolean includeProjectDependencies,
            @AgiToolParam("Whether to include the project's test outputs and test-scoped dependencies.") boolean includeTestContext,
            @AgiToolParam(value = "Optional additional compiler options (e.g. ['--release','21']).", required = false) String[] compilerOptions) throws Exception {

        String extraClassPath = buildProjectClasspathString(projectPath, includeProjectDependencies, includeTestContext);
        return compileAndExecute(sourceCode, extraClassPath, compilerOptions);
    }

    /**
     * Builds the classpath string for an open project via {@link OrderEnumerator}.
     *
     * @param projectPath                the absolute base path of the open project.
     * @param includeProjectDependencies whether to include library dependencies.
     * @param includeTestContext         whether to include the test scope.
     * @return a path-separator-joined classpath string.
     * @throws AgiToolException if the project is not open or resolves to an empty classpath.
     */
    public String buildProjectClasspathString(String projectPath, boolean includeProjectDependencies, boolean includeTestContext) throws AgiToolException {
        Project project = resolveProject(projectPath);
        JavaPsi.requireSmart(project);
        String classpath = ReadAction.compute(() -> {
            OrderEnumerator enumerator = OrderEnumerator.orderEntries(project).recursively().withoutSdk();
            if (!includeTestContext) {
                enumerator = enumerator.productionOnly();
            }
            if (!includeProjectDependencies) {
                enumerator = enumerator.withoutLibraries();
            }
            return enumerator.classes().getPathsList().getPathsString();
        });
        if (classpath == null || classpath.isBlank()) {
            throw new AgiToolException("Could not resolve any classpath entries for project: " + projectPath);
        }
        return classpath;
    }

    /**
     * Resolves an open {@link Project} by its base path, falling back to VFS content lookup.
     *
     * @param projectPath the absolute base path.
     * @return the matching open project.
     * @throws AgiToolException if no open project matches.
     */
    private Project resolveProject(String projectPath) throws AgiToolException {
        String target = Path.of(projectPath).toAbsolutePath().toString();
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            String basePath = project.getBasePath();
            if (basePath != null && Path.of(basePath).toAbsolutePath().toString().equals(target)) {
                return project;
            }
        }
        VirtualFile vf = JavaPsi.findVirtualFile(projectPath);
        if (vf != null) {
            Project project = JavaPsi.findHostProject(vf);
            if (project != null) {
                return project;
            }
        }
        throw new AgiToolException("No open IntelliJ project at: " + projectPath);
    }
}
