/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.projectRoots.JavaSdk;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.OrderEnumerator;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SystemUtils;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.OnTheFlyAgiTool;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.intellij.IntellijAsiContainer;
import uno.anahata.asi.intellij.internal.IntellijPluginUtils;
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.intellij.tools.project.Projects;
import uno.anahata.asi.intellij.ui.IntellijTextResourceWriteRenderer;
import uno.anahata.asi.intellij.ui.resources.IntellijResourceUI;
import uno.anahata.asi.intellij.ui.resources.IntellijTextResourceViewer;
import uno.anahata.asi.swing.toolkit.SwingJava;
import uno.anahata.asi.toolkit.java.KnownJdk;

import javax.tools.ToolProvider;
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Stream;

/**
 * An IntelliJ-aware extension of the core {@code Java} toolkit that can compile and execute
 * dynamic scripts on the application JVM and against a specific open project's classpath.
 * <p>
 * This toolkit handles:
 * </p>
 * <ul>
 *   <li>Automatic discovery of the full IntelliJ plugin classpath (combining IntelliJ platform libraries,
 *       bundled plugin dependencies, and runtime classes).</li>
 *   <li>External {@code javac} compilation support when running on JetBrains Runtime (JBR) where
 *       in-memory {@code JavaCompiler} is omitted.</li>
 *   <li>Project classpath resolution via {@link OrderEnumerator} for hot-reloading project bytecode.</li>
 *   <li>Parent-first classloader delegation to preserve ThreadLocal context and singleton identities.</li>
 * </ul>
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
     * Registers IntelliJ-specific parent-first infrastructure classes and bootstraps
     * the full IntelliJ plugin runtime classpath.
     * </p>
     */
    @Override
    public void initialize() {
        super.initialize();
        registerParentFirstClass(IntellijAsiContainer.class);
        registerParentFirstClass(IntellijResourceUI.class);
        registerParentFirstClass(IntellijTextResourceViewer.class);
        registerParentFirstClass(IntellijTextResourceWriteRenderer.class);
        registerParentFirstClass(Projects.class);
        registerParentFirstClass(JavaPsi.class);
        registerParentFirstClass(OrderEnumerator.class);
        setDefaultClasspath(IntellijPluginUtils.getFullAnahataAsiPluginClasspath());
        log.debug("IntellijJava initialize() default classpath initialized with {} entries.",
                getDefaultClasspath().split(File.pathSeparator).length);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Re-establishes the full default classpath upon deserialization.
     * </p>
     */
    @Override
    public void postActivate() {
        super.postActivate();
        setDefaultClasspath(IntellijPluginUtils.getFullAnahataAsiPluginClasspath());
        log.debug("IntellijJava postActivate() completed.");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Appends IntelliJ-specific guidance describing classpath architecture, JDK selection,
     * and when to prefer {@code compileAndExecuteInProject} over the default {@code compileAndExecute}.
     * </p>
     *
     * @return the list of system instruction blocks.
     * @throws Exception if an error occurs while assembling instructions.
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> instructions = new ArrayList<>(super.getSystemInstructions());
        instructions.add(
                "\n**IntelliJ Classpath & Compilation Architecture**:\n"
                + "- **Plugin Classpath**: Includes all IntelliJ Platform OpenAPI libraries, bundled plugin dependencies (core, swing, intellij), and active IDE runtime classes.\n"
                + "- **JDK / Javac Resolution**: When running inside JetBrains Runtime (JBR), the toolkit automatically invokes `javac` from the configured Project SDK or registered SDKs in `ProjectJdkTable`. You can also supply an explicit JDK name or path.\n"
                + "- **Hot Reloading via `compileAndExecuteInProject`**: Appends the target project's compiled `target/classes` and library dependencies to the child-first classloader, prioritizing project bytecode.\n");
        return instructions;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Appends open projects' configured SDKs to the RAG message.
     * </p>
     *
     * @param ragMessage the incoming RAG message to populate.
     * @throws Exception if an error occurs during message population.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        super.populateMessage(ragMessage);
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        if (open.length > 0) {
            StringBuilder sb = new StringBuilder();
            sb.append("\n## IntelliJ Open Projects SDKs\n");
            for (Project p : open) {
                Sdk sdk = ProjectRootManager.getInstance(p).getProjectSdk();
                sb.append("- **").append(p.getName()).append("**: ")
                        .append(sdk != null ? sdk.getName() + " (`" + sdk.getHomePath() + "`)" : "Not Configured")
                        .append("\n");
            }
            ragMessage.addTextPart(sb.toString());
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Augments standard system JDK discovery with IntelliJ's registered SDKs from
     * {@link ProjectJdkTable}, open projects' configured SDKs, and IntelliJ's suggested home paths.
     * </p>
     *
     * @return the list of discovered {@link KnownJdk} instances.
     */
    @Override
    public List<KnownJdk> getKnownJdks() {
        List<KnownJdk> result = new ArrayList<>(super.getKnownJdks());
        Set<Path> seenJavacPaths = new HashSet<>();
        for (KnownJdk existing : result) {
            if (existing.javacPath() != null) {
                seenJavacPaths.add(existing.javacPath().toAbsolutePath().normalize());
            }
        }

        // 1. Check open projects' configured Project SDKs (marked as preferred)
        try {
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                Sdk sdk = ProjectRootManager.getInstance(project).getProjectSdk();
                if (sdk != null && sdk.getHomePath() != null) {
                    Path home = Path.of(sdk.getHomePath());
                    Path javac = findJavacInJdkHome(home);
                    if (javac != null && seenJavacPaths.add(javac.toAbsolutePath().normalize())) {
                        result.add(0, new KnownJdk("Project: " + project.getName() + " (" + sdk.getName() + ")", home, javac, sdk.getVersionString(), true));
                    }
                }
            }
        } catch (Throwable t) {
            log.debug("IntellijJava: Error resolving open project SDKs", t);
        }

        // 2. Check all registered SDKs in ProjectJdkTable
        try {
            for (Sdk sdk : ProjectJdkTable.getInstance().getAllJdks()) {
                if (sdk.getHomePath() != null) {
                    Path home = Path.of(sdk.getHomePath());
                    Path javac = findJavacInJdkHome(home);
                    if (javac != null && seenJavacPaths.add(javac.toAbsolutePath().normalize())) {
                        result.add(new KnownJdk(sdk.getName(), home, javac, sdk.getVersionString(), false));
                    }
                }
            }
        } catch (Throwable t) {
            log.debug("IntellijJava: Error querying ProjectJdkTable", t);
        }

        // 3. Check IntelliJ's suggested JDK home paths
        try {
            for (String suggested : JavaSdk.getInstance().suggestHomePaths()) {
                Path home = Path.of(suggested);
                Path javac = findJavacInJdkHome(home);
                if (javac != null && seenJavacPaths.add(javac.toAbsolutePath().normalize())) {
                    result.add(new KnownJdk("Suggested: " + home.getFileName(), home, javac, null, false));
                }
            }
        } catch (Throwable ignored) {
        }

        return result;
    }

    /**
     * Compiles and executes a Java script against a specific open project's classpath.
     * <p>
     * The project's compiled module outputs (and, optionally, its library dependencies and
     * test scope) are resolved via {@link OrderEnumerator} and appended to the child-first
     * class loader. Automatically compiles using the project's configured Project SDK.
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

        Project project = resolveProject(projectPath);
        String extraClassPath = buildProjectClasspathString(projectPath, includeProjectDependencies, includeTestContext);

        String projectJavac = null;
        Sdk sdk = ProjectRootManager.getInstance(project).getProjectSdk();
        if (sdk != null) {
            String homePath = sdk.getHomePath();
            if (homePath != null) {
                Path javac = findJavacInJdkHome(Path.of(homePath));
                if (javac != null) {
                    projectJavac = javac.toAbsolutePath().toString();
                    log("Resolved project '" + project.getName() + "' SDK: " + sdk.getName() + " -> javac: " + projectJavac);
                } else {
                    log("Warning: Project '" + project.getName() + "' SDK '" + sdk.getName() + "' has home directory '" + homePath + "' but no executable javac binary was found. Falling back to default compiler.");
                }
            } else {
                log("Warning: Project '" + project.getName() + "' SDK '" + sdk.getName() + "' has no configured home path. Falling back to default compiler.");
            }
        } else {
            log("Warning: Project '" + project.getName() + "' has no configured Project SDK. Falling back to default compiler.");
        }

        return compileAndExecute(sourceCode, extraClassPath, compilerOptions, projectJavac);
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
        log("Resolved classpath for project '" + project.getName() + "' (dependencies=" + includeProjectDependencies + ", tests=" + includeTestContext + ") with " + classpath.split(File.pathSeparator).length + " entries.");
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
