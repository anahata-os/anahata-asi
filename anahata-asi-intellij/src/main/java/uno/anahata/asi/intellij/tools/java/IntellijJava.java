/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java;

import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.projectRoots.JavaSdk;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.OrderEnumerator;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.PathUtil;
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
import uno.anahata.asi.intellij.internal.JavaPsi;
import uno.anahata.asi.intellij.tools.project.Projects;
import uno.anahata.asi.intellij.ui.IntellijTextResourceWriteRenderer;
import uno.anahata.asi.intellij.ui.resources.IntellijResourceUI;
import uno.anahata.asi.intellij.ui.resources.IntellijTextResourceViewer;
import uno.anahata.asi.swing.toolkit.SwingJava;

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
 * <ul>
 *   <li>Automatic discovery of the full IntelliJ plugin classpath (combining IntelliJ platform libraries,
 *       bundled plugin dependencies, and runtime classes).</li>
 *   <li>External {@code javac} compilation support when running on JetBrains Runtime (JBR) where
 *       in-memory {@code JavaCompiler} is omitted.</li>
 *   <li>Project classpath resolution via {@link OrderEnumerator} for hot-reloading project bytecode.</li>
 *   <li>Parent-first classloader delegation to preserve ThreadLocal context and singleton identities.</li>
 * </ul>
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
        setDefaultClasspath(buildFullPluginClasspath());
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
        setDefaultClasspath(buildFullPluginClasspath());
        log.debug("IntellijJava postActivate() completed.");
    }

    /**
     * {@inheritDoc}
     * <p>
     * Appends IntelliJ-specific guidance describing classpath architecture, JDK selection,
     * and when to prefer {@code compileAndExecuteInProject} over the default {@code compileAndExecute}.
     * </p>
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
     * Appends available SDKs and the resolved javac executable path to the RAG message.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        super.populateMessage(ragMessage);
        StringBuilder sb = new StringBuilder();
        sb.append("\n## IntelliJ Java Compiler & SDK Environment\n");

        // Active javac path
        try {
            Path javac = resolveJavac(null);
            sb.append("- **Active Javac Compiler**: `").append(javac.toAbsolutePath()).append("`\n");
        } catch (Exception e) {
            sb.append("- **Active Javac Compiler**: ⚠️ ").append(e.getMessage()).append("\n");
        }

        // Project SDKs for open projects
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        if (open.length > 0) {
            sb.append("- **Open Projects SDKs**:\n");
            for (Project p : open) {
                Sdk sdk = ProjectRootManager.getInstance(p).getProjectSdk();
                sb.append("  * **").append(p.getName()).append("**: ")
                        .append(sdk != null ? sdk.getName() + " (`" + sdk.getHomePath() + "`)" : "Not Configured")
                        .append("\n");
            }
        }

        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Assembles the comprehensive default classpath for the IntelliJ plugin runtime.
     *
     * @return the combined classpath string separated by {@link File#pathSeparator}.
     */
    public String buildFullPluginClasspath() {
        Set<String> paths = new LinkedHashSet<>();

        // 1. Current class code source / JAR
        try {
            String jarPath = PathUtil.getJarPathForClass(IntellijJava.class);
            if (jarPath != null && Files.exists(Path.of(jarPath))) {
                paths.add(jarPath);
            }
        } catch (Throwable ignored) {
        }

        // 2. Plugin directory lib folder
        try {
            PluginId pluginId = PluginId.getId("uno.anahata.asi.intellij");
            IdeaPluginDescriptor plugin = PluginManagerCore.getPlugin(pluginId);
            if (plugin != null && plugin.getPluginPath() != null) {
                Path libDir = plugin.getPluginPath().resolve("lib");
                if (Files.exists(libDir) && Files.isDirectory(libDir)) {
                    try (Stream<Path> stream = Files.list(libDir)) {
                        stream.filter(p -> p.toString().endsWith(".jar"))
                                .forEach(p -> paths.add(p.toAbsolutePath().toString()));
                    }
                }
                Path classesDir = plugin.getPluginPath().resolve("classes");
                if (Files.exists(classesDir) && Files.isDirectory(classesDir)) {
                    paths.add(classesDir.toAbsolutePath().toString());
                }
            }
        } catch (Throwable ignored) {
        }

        // 3. IntelliJ Platform lib folder
        try {
            String libPath = PathManager.getLibPath();
            if (libPath != null && Files.exists(Path.of(libPath))) {
                try (Stream<Path> stream = Files.list(Path.of(libPath))) {
                    stream.filter(p -> p.toString().endsWith(".jar"))
                            .forEach(p -> paths.add(p.toAbsolutePath().toString()));
                }
            }
        } catch (Throwable ignored) {
        }

        // 4. Java plugin lib folder
        try {
            PluginId javaPluginId = PluginId.getId("com.intellij.java");
            IdeaPluginDescriptor javaPlugin = PluginManagerCore.getPlugin(javaPluginId);
            if (javaPlugin != null && javaPlugin.getPluginPath() != null) {
                Path libDir = javaPlugin.getPluginPath().resolve("lib");
                if (Files.exists(libDir) && Files.isDirectory(libDir)) {
                    try (Stream<Path> stream = Files.list(libDir)) {
                        stream.filter(p -> p.toString().endsWith(".jar"))
                                .forEach(p -> paths.add(p.toAbsolutePath().toString()));
                    }
                }
            }
        } catch (Throwable ignored) {
        }

        // 5. System class path entries
        String sysCp = System.getProperty("java.class.path");
        if (sysCp != null && !sysCp.isBlank()) {
            for (String entry : sysCp.split(File.pathSeparator)) {
                if (!entry.isBlank() && Files.exists(Path.of(entry))) {
                    paths.add(entry);
                }
            }
        }

        return String.join(File.pathSeparator, paths);
    }

    /**
     * Resolves the path to a javac executable from a specified JDK name/path, or auto-detects from the workspace.
     *
     * @param jdkNameOrPath optional JDK name (e.g. "24", "corretto-21") or directory path; if null, auto-detects.
     * @return the absolute path to the javac executable.
     * @throws AgiToolException if no javac executable could be found.
     */
    public static Path resolveJavac(String jdkNameOrPath) throws AgiToolException {
        // 1. If explicit name or path is provided
        if (jdkNameOrPath != null && !jdkNameOrPath.isBlank()) {
            Sdk sdk = ProjectJdkTable.getInstance().findJdk(jdkNameOrPath.trim());
            if (sdk != null && sdk.getHomePath() != null) {
                Path javac = findJavacInJdkHome(Path.of(sdk.getHomePath()));
                if (javac != null) {
                    return javac;
                }
            }
            Path dir = Path.of(jdkNameOrPath.trim());
            if (Files.isDirectory(dir)) {
                Path javac = findJavacInJdkHome(dir);
                if (javac != null) {
                    return javac;
                }
            }
            if (Files.isExecutable(dir) && dir.getFileName().toString().startsWith("javac")) {
                return dir;
            }
        }

        // 2. Check open projects' configured SDKs
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            Sdk sdk = ProjectRootManager.getInstance(project).getProjectSdk();
            if (sdk != null && sdk.getHomePath() != null) {
                Path javac = findJavacInJdkHome(Path.of(sdk.getHomePath()));
                if (javac != null) {
                    return javac;
                }
            }
        }

        // 3. Check all registered SDKs in ProjectJdkTable
        for (Sdk sdk : ProjectJdkTable.getInstance().getAllJdks()) {
            if (sdk.getHomePath() != null) {
                Path javac = findJavacInJdkHome(Path.of(sdk.getHomePath()));
                if (javac != null) {
                    return javac;
                }
            }
        }

        // 4. Check suggested system JDK home paths
        try {
            for (String suggested : JavaSdk.getInstance().suggestHomePaths()) {
                Path javac = findJavacInJdkHome(Path.of(suggested));
                if (javac != null) {
                    return javac;
                }
            }
        } catch (Throwable ignored) {
        }

        // 5. Check JAVA_HOME environment variable
        String javaHome = System.getenv("JAVA_HOME");
        if (javaHome != null) {
            Path javac = findJavacInJdkHome(Path.of(javaHome));
            if (javac != null) {
                return javac;
            }
        }

        // 6. Check common OS JDK directories
        List<Path> standardRoots = List.of(
                Path.of("/Library/Java/JavaVirtualMachines"),
                Path.of("/usr/lib/jvm"),
                Path.of("C:\\Program Files\\Java"),
                Path.of("C:\\Program Files\\Eclipse Adoptium")
        );
        for (Path root : standardRoots) {
            if (Files.exists(root) && Files.isDirectory(root)) {
                try (Stream<Path> stream = Files.list(root)) {
                    for (Path candidate : stream.toList()) {
                        Path javac = findJavacInJdkHome(candidate);
                        if (javac != null) {
                            return javac;
                        }
                    }
                } catch (Exception ignored) {
                }
            }
        }

        throw new AgiToolException("No JDK javac compiler found. Please configure a Project SDK in IntelliJ or set a JDK home path via Projects.setProjectSdk.");
    }

    /**
     * Inspects a JDK home directory and finds the javac binary.
     *
     * @param home the candidate JDK home directory.
     * @return the path to javac if found, or null.
     */
    private static Path findJavacInJdkHome(Path home) {
        if (home == null || !Files.exists(home)) {
            return null;
        }
        Path direct = home.resolve("bin").resolve(SystemUtils.IS_OS_WINDOWS ? "javac.exe" : "javac");
        if (Files.exists(direct) && Files.isExecutable(direct)) {
            return direct;
        }
        Path macHome = home.resolve("Contents").resolve("Home").resolve("bin").resolve("javac");
        if (Files.exists(macHome) && Files.isExecutable(macHome)) {
            return macHome;
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Compiles Java source code. If in-memory {@link JavaCompiler} is available,
     * it delegates to the in-memory compiler in {@code super.compile}. If running on JRE/JBR
     * where in-memory javac is omitted, it automatically delegates to {@link #compileWithExternalJavac}.
     * </p>
     */
    @Override
    public Class<?> compile(
            String sourceCode,
            String className,
            String extraClassPath,
            String[] compilerOptions,
            javax.tools.JavaCompiler compiler)
            throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, java.lang.reflect.InvocationTargetException {
        if (compiler != null) {
            try {
                return super.compile(sourceCode, className, extraClassPath, compilerOptions, compiler);
            } catch (Throwable t) {
                log.warn("In-memory JavaCompiler failed, falling back to external javac: {}", t.getMessage());
            }
        }
        try {
            Path javac = resolveJavac(null);
            return compileWithExternalJavac(sourceCode, className, extraClassPath, compilerOptions, javac);
        } catch (Exception e) {
            throw new RuntimeException("Compilation failed via external javac", e);
        }
    }

    /**
     * Compiles Java source code using the external javac process and loads the resulting class.
     *
     * @param sourceCode      the Java source code.
     * @param className       the simple class name.
     * @param extraClassPath  optional additional classpath entries.
     * @param compilerOptions optional compiler options.
     * @param javacPath       the absolute path to the javac executable.
     * @return the loaded {@link Class}.
     * @throws Exception on compilation or classloading failure.
     */
    protected Class<?> compileWithExternalJavac(
            String sourceCode,
            String className,
            String extraClassPath,
            String[] compilerOptions,
            Path javacPath) throws Exception {

        final ToolContext ctx = getToolContext();
        Path tempDir = Files.createTempDirectory("anahata-javac-" + className + "-");
        try {
            Path sourceFile = tempDir.resolve(className + ".java");
            Files.writeString(sourceFile, sourceCode, StandardCharsets.UTF_8);

            String classpath = getDefaultClasspath();
            if (extraClassPath != null && !extraClassPath.isEmpty()) {
                classpath = extraClassPath + File.pathSeparator + classpath;
            }

            List<String> command = new ArrayList<>();
            command.add(javacPath.toAbsolutePath().toString());
            command.add("-d");
            command.add(tempDir.toAbsolutePath().toString());
            command.add("-classpath");
            command.add(classpath);

            if (compilerOptions != null) {
                command.addAll(Arrays.asList(compilerOptions));
            }

            // Only pass --proc:none to avoid annotation processing delays
            command.add("-proc:none");
            command.add(sourceFile.toAbsolutePath().toString());

            log.info("Executing javac: {}", command);
            ProcessBuilder pb = new ProcessBuilder(command);
            pb.redirectErrorStream(true);
            Process process = pb.start();
            String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            int exitCode = process.waitFor();

            if (exitCode != 0) {
                log.error("Compilation error via javac ({}):\n{}", javacPath, output);
                throw new RuntimeException("Compilation error via javac (" + javacPath.getFileName() + "):\n" + output);
            }

            // Read compiled .class files from tempDir
            Map<String, byte[]> compiledClasses = new HashMap<>();
            try (Stream<Path> stream = Files.walk(tempDir)) {
                for (Path file : stream.filter(p -> p.toString().endsWith(".class")).toList()) {
                    String relative = tempDir.relativize(file).toString();
                    String classFqn = relative.replace(File.separatorChar, '.').replace('/', '.');
                    if (classFqn.endsWith(".class")) {
                        classFqn = classFqn.substring(0, classFqn.length() - 6);
                    }
                    compiledClasses.put(classFqn, Files.readAllBytes(file));
                }
            }

            List<URL> urlList = new ArrayList<>();
            urlList.add(tempDir.toUri().toURL());
            if (extraClassPath != null && !extraClassPath.isEmpty()) {
                for (String entry : extraClassPath.split(File.pathSeparator)) {
                    try {
                        urlList.add(new File(entry).toURI().toURL());
                    } catch (Exception e) {
                        log.warn("Invalid classpath entry: {}", entry, e);
                    }
                }
            }

            URLClassLoader reloadingClassLoader = new URLClassLoader(urlList.toArray(new URL[0]), Thread.currentThread().getContextClassLoader()) {
                @Override
                protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
                    synchronized (getClassLoadingLock(name)) {
                        Class<?> c = findLoadedClass(name);
                        if (c == null) {
                            if (parentFirstClassess.contains(name)) {
                                if (ctx != null) {
                                    ctx.log("Delegating infrastructure class to parent: " + name);
                                }
                                return super.loadClass(name, resolve);
                            }
                            byte[] bytes = compiledClasses.get(name);
                            if (bytes != null) {
                                c = defineClass(name, bytes, 0, bytes.length);
                            } else {
                                try {
                                    c = findClass(name);
                                } catch (ClassNotFoundException e) {
                                    byte[] fallbackBytes = findClassFallbackBytes(name);
                                    if (fallbackBytes != null) {
                                        c = defineClass(name, fallbackBytes, 0, fallbackBytes.length);
                                    } else {
                                        c = super.loadClass(name, resolve);
                                    }
                                }
                            }
                        }
                        if (resolve) {
                            resolveClass(c);
                        }
                        return c;
                    }
                }
            };

            return reloadingClassLoader.loadClass(className);
        } finally {
            tempDir.toFile().deleteOnExit();
        }
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
