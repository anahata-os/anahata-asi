/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.java;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.modules.maven.api.NbMavenProject;
import org.netbeans.spi.java.classpath.ClassPathProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.netbeans.api.java.source.SourceUtils;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.io.IOException;
import uno.anahata.asi.nb.module.NetBeansModuleUtils;

import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.toolkit.java.Java;
import uno.anahata.asi.swing.toolkit.SwingJava;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.nb.NetBeansAsiContainer;
import uno.anahata.asi.nb.resources.handle.NbHandle;
import uno.anahata.asi.toolkit.java.classpath.VeryPrettyClassPathPrinter;

/**
 * A NetBeans-aware extension of the core {@link Java} toolkit. It adds the
 * ability to execute code within the context of a specific project, enabling a
 * powerful "hot-reload" workflow by prioritizing the project's compiled output.
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A NetBeans-aware toolkit for compiling and executing Java code.")
public class NbJava extends SwingJava {

    /**
     * {@inheritDoc}
     * <p>
     * Sets the default classpath to include the NetBeans modules
     * environment.</p>
     */
    @Override
    public void initialize() {
        super.initialize();
        registerParentFirstClass(NbHandle.class);
        registerParentFirstClass(NetBeansAsiContainer.class);
        setDefaultClasspath(NetBeansModuleUtils.getNetBeansClasspath());
        log.info("initialize() default classPath:" + getDefaultClasspath());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Re-establishes the default classpath after deserialization, ensuring
     * connectivity to the NetBeans module system is maintained.</p>
     */
    @Override
    public void postActivate() {
        super.postActivate();
        //we should really only be doing this if we are in dev / reload mode
        setDefaultClasspath(NetBeansModuleUtils.getNetBeansClasspath());
        log.info("NbJava rebind() completed. default classPath:" + getDefaultClasspath());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Overrides the factory to inject the specialized
     * {@link NetBeansJarHandler}.</p>
     */
    @Override
    protected VeryPrettyClassPathPrinter createClassPathPrinter() {
        VeryPrettyClassPathPrinter printer = super.createClassPathPrinter();
        printer.addHandler(new NetBeansJarHandler());
        return printer;
    }

    /**
     * A transient registry of JAR files that support Multi-Release versions.
     * Used to bridge classes from META-INF/versions into the dynamic loader.
     */
    private transient List<File> mrJarRegistry;

    /**
     * {@inheritDoc}
     * <p>
     * Invalidates the MR-JAR registry when the classpath changes to ensure
     * environmental consistency.</p>
     */
    @Override
    public void setDefaultClasspath(String defaultCompilerClasspath) {
        super.setDefaultClasspath(defaultCompilerClasspath);
        synchronized (this) {
            mrJarRegistry = null;
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implements the surgical fallback by searching for missing classes within
     * the registered Multi-Release JARs using the current JVM version.
     * Restricted to "org.lwjgl." as it is just to workaround a netbeans bug in JarClassLoader.</p>
     */
    @Override
    protected byte[] findClassFallbackBytes(String name) {
        // SURGICAL FILTER: Only bridge packages we know need MR-support in this context
        if (!name.startsWith("org.lwjgl.")/* && !name.startsWith("org.jspecify.")*/) {
            return null;
        }

        ensureMrJarRegistry();
        String path = name.replace('.', '/') + ".class";

        for (File jarFile : mrJarRegistry) {
            // Using the modern JarFile constructor which is Multi-Release aware.
            // Passing Runtime.version() ensures that JarFile.getEntry(path) 
            // performs the correct backwards lookup (e.g., META-INF/versions/25/...)
            try (JarFile jf = new JarFile(jarFile, true, ZipFile.OPEN_READ, Runtime.version())) {
                ZipEntry ze = jf.getEntry(path);
                if (ze != null) {
                    try (var is = jf.getInputStream(ze)) {
                        byte[] bytes = is.readAllBytes();
                        log.info("NbJava: Successfully bridged class '{}' from MR-JAR: {}", name, jarFile.getName());
                        return bytes;
                    }
                }
            } catch (IOException e) {
                log.error("NbJava: Error reading from MR-JAR '{}' during fallback lookup for class '{}': {}",
                        jarFile.getAbsolutePath(), name, e.getMessage());
            }
        }
        return null;
    }

    /**
     * Lazily populates the Multi-Release JAR registry by scanning the current
     * default classpath.
     */
    private synchronized void ensureMrJarRegistry() {
        if (mrJarRegistry != null) {
            return;
        }
        mrJarRegistry = new ArrayList<>();
        String classpath = getDefaultClasspath();
        if (classpath == null) {
            return;
        }

        for (String entry : classpath.split(File.pathSeparator)) {
            File f = new File(entry);
            if (f.exists() && f.isFile() && entry.endsWith(".jar")) {
                try (JarFile jar = new JarFile(f)) {
                    var mf = jar.getManifest();
                    if (mf != null && "true".equalsIgnoreCase(mf.getMainAttributes().getValue("Multi-Release"))) {
                        mrJarRegistry.add(f);
                    }
                } catch (IOException e) {
                    log.warn("NbJava: Failed to read manifest from JAR '{}' while building MR registry: {}", f.getAbsolutePath(), e.getMessage());
                }
            }
        }
        log.info("NbJava: MR-JAR registry populated with {} entries.", mrJarRegistry.size());
    }

    /**
     * Compiles and executes Java source code within the context of a specific
     * NetBeans project. This tool enables a powerful 'hot-reload' workflow by
     * creating a dynamic classpath that prioritizes the project's own build
     * directories (e.g., 'target/classes') over the application's default
     * classpath.
     *
     * @param projectPath The absolute path of the NetBeans project to run in.
     * @param sourceCode Source code of a public class named **Anahata** that
     * has **no package declaration** and **extends AnahataTool**.
     * @param includeDependencies Whether to include the project's COMPILE and
     * EXECUTE dependencies.
     * @param includeTestDependencies Whether to include the project's test
     * source folders and test dependencies.
     * @param compilerOptions Optional additional compiler options.
     * @return The result of the execution.
     * @throws Exception on error.
     */
    @AgiTool(
            value = "Proxy tool for compileAndExecute that aggregates the extraClassPath parameter by including the target/classes directory of the specified NetBeans project and calls compileAndExecute passing this target/classs directory so any types on the specified project can be imported in the Anahata java class. "
                    + "Additionally, if includeDependencies is true, the target/classess of any open projects that are a dependency of the specified project will be added to the extraClassPath as well as the jar files of all dependencies. "
            + "This tool enables a powerful 'hot-reload' workflow by creating a dynamic classpath that prioritizes the project's own build directories (e.g., 'target/classes') over the projects artifacts so you can update one or more java files and run a script that uses it in the same turn to test the changes made to the java files. "
            + "Usage Rule: Use the normal compileAndExecute **if you are need to imort any java types from any of the open projects in the Anahata class. You don't need to do compileAndExecuteInProject to read a file in that project or to use a netbeans api that does anything to that project or to any file in that project. This tool only builds the extraClassPath parameter of compileAndExecute**"
    )
    public Object compileAndExecuteInProject(
            @AgiToolParam(value = "Source code of a public class named **Anahata** that is **public** has **no package declaration**, **extends SwingAgiTool** (or whatever is indicated in the system instructions) and implements the call() method of java.util.concurrent.Callable", rendererId = "java") String sourceCode,
            @AgiToolParam("The absolute path of the NetBeans project to run in.") String projectPath,
            @AgiToolParam("Whether to include the project's COMPILE and EXECUTE **dependencies**. If any of the dependencies is an open project, the target/classess of that project will be added to the extraClassPath") boolean includeDependencies,
            @AgiToolParam("Whether to include the project's test source folders and test dependencies. If any test dependencies is an open project, the target/test-classess of that project will be added to the extraClassPath") boolean includeTestDependencies,
            @AgiToolParam(value = "Optional additional compiler options.", required = false) String[] compilerOptions) throws Exception {

        Project project = Projects.findOpenProject(projectPath);
        Projects projectsToolkit = getToolManager().getToolkitInstance(Projects.class).orElseThrow(() -> new IllegalStateException("Projects toolkit not found"));

        waitForIde(project, projectsToolkit.isCompileOnSaveEnabled(project));

        ClassPathProvider cpp = project.getLookup().lookup(ClassPathProvider.class);
        if (cpp == null) {
            throw new IllegalStateException("Could not find ClassPathProvider for project: " + projectPath);
        }

        // Detect NBM packaging
        boolean isNbm = false;
        NbMavenProject nbMavenProject = project.getLookup().lookup(NbMavenProject.class);
        if (nbMavenProject != null) {
            String packaging = nbMavenProject.getMavenProject().getPackaging();
            isNbm = "nbm".equals(packaging) || "nbm-application".equals(packaging);
        }

        // Map open projects to their target/classes for hot-reload swapping
        Map<String, String> openProjectArtifacts = new HashMap<>();

        for (Project p : OpenProjects.getDefault().getOpenProjects()) {
            String cosStatus = projectsToolkit.isCompileOnSaveEnabled(p);
            if (cosStatus.startsWith("all") || cosStatus.equalsIgnoreCase("Enabled")) {
                NbMavenProject nmp = p.getLookup().lookup(NbMavenProject.class);
                if (nmp != null) {
                    org.apache.maven.project.MavenProject mp = nmp.getMavenProject();
                    String key = mp.getGroupId() + ":" + mp.getArtifactId();
                    FileObject targetClasses = p.getProjectDirectory().getFileObject("target/classes");
                    if (targetClasses != null && targetClasses.isFolder()) {
                        openProjectArtifacts.put(key, FileUtil.toFile(targetClasses).getAbsolutePath());
                    }
                }
            }
        }

        // Map JAR paths to artifact keys for the current project
        Map<String, String> jarToArtifactKey = new HashMap<>();
        if (nbMavenProject != null) {
            for (org.apache.maven.artifact.Artifact art : nbMavenProject.getMavenProject().getArtifacts()) {
                File f = art.getFile();
                if (f != null) {
                    jarToArtifactKey.put(f.getAbsolutePath(), art.getGroupId() + ":" + art.getArtifactId());
                }
            }
        }

        List<String> internalPaths = new ArrayList<>();
        List<String> dependencyPaths = new ArrayList<>();

        // Get the current default classpath to avoid duplication
        String defaultCp = getDefaultClasspath();
        Set<String> existingPaths = new HashSet<>(Arrays.asList(defaultCp.split(File.pathSeparator)));
        Set<String> existingBaseNames = new HashSet<>();
        for (String path : existingPaths) {
            if (path.endsWith(".jar")) {
                existingBaseNames.add(getJarBaseName(new File(path).getName()));
            }
        }

        Sources sources = ProjectUtils.getSources(project);
        SourceGroup[] javaGroups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);
        SourceGroup[] resourceGroups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_RESOURCES);

        List<SourceGroup> allSourceGroups = new ArrayList<>(Arrays.asList(javaGroups));
        allSourceGroups.addAll(Arrays.asList(resourceGroups));

        for (SourceGroup sg : allSourceGroups) {
            boolean isTest = sg.getDisplayName().toLowerCase().contains("test");
            if (isTest && !includeTestDependencies) {
                continue;
            }

            ClassPath compileCp = cpp.findClassPath(sg.getRootFolder(), ClassPath.COMPILE);
            ClassPath executeCp = cpp.findClassPath(sg.getRootFolder(), ClassPath.EXECUTE);

            Set<FileObject> allRoots = new LinkedHashSet<>();
            if (compileCp != null) {
                allRoots.addAll(Arrays.asList(compileCp.getRoots()));
            }
            if (executeCp != null) {
                allRoots.addAll(Arrays.asList(executeCp.getRoots()));
            }

            for (FileObject entry : allRoots) {
                URL url = entry.toURL();
                File f = FileUtil.archiveOrDirForURL(url);

                if (f != null) {
                    String absolutePath = f.getAbsolutePath();

                    if (f.isDirectory()) {
                        if (!internalPaths.contains(absolutePath)) {
                            internalPaths.add(absolutePath);
                        }
                    } else {
                        // It's a JAR. Check if it's an open project we can swap for source.
                        String artifactKey = jarToArtifactKey.get(absolutePath);
                        if (artifactKey != null && openProjectArtifacts.containsKey(artifactKey)) {
                            String sourcePath = openProjectArtifacts.get(artifactKey);
                            if (!internalPaths.contains(sourcePath)) {
                                log.info("Swapping dependency JAR for open project source: {} -> {}", artifactKey, sourcePath);
                                internalPaths.add(sourcePath);
                            }
                            continue;
                        }

                        if (includeDependencies) {
                            String jarName = f.getName();
                            String baseName = getJarBaseName(jarName);

                            // Aggressive NetBeans Platform and Stub Filtering
                            boolean isNetBeansJar = jarName.startsWith("org-netbeans-")
                                    || jarName.startsWith("org-openide-")
                                    || jarName.startsWith("org-apache-netbeans-")
                                    || jarName.contains("nbstubs");

                            if (isNbm && isNetBeansJar) {
                                log.info("Skipping NetBeans Platform/Stub JAR in NBM project: {}", jarName);
                                continue;
                            }

                            boolean isDuplicate = existingPaths.contains(absolutePath) || existingBaseNames.contains(baseName);

                            if (!isDuplicate) {
                                if (!dependencyPaths.contains(absolutePath)) {
                                    dependencyPaths.add(absolutePath);
                                }
                            } else {
                                log.info("Skipping duplicate JAR (base name match: {}): {}", baseName, jarName);
                            }
                        }
                    }
                }
            }
        }

        log.info("Constructing classpath for project '{}' (NBM Mode: {})", projectPath, isNbm);
        log.info("Found {} internal/open project directories (e.g., target/classes):", internalPaths.size());
        for (String path : internalPaths) {
            log.info("  - {}", path);
        }

        if (includeDependencies) {
            log.info("Including {} unique resolved dependency JARs.", dependencyPaths.size());
        }

        List<String> finalPathElements = new ArrayList<>(internalPaths);
        finalPathElements.addAll(dependencyPaths);

        if (finalPathElements.isEmpty()) {
            throw new IllegalStateException("Could not resolve any classpath entries for project: " + projectPath);
        }

        String extraClassPath = String.join(File.pathSeparator, finalPathElements);

        return compileAndExecute(sourceCode, extraClassPath, compilerOptions);
    }

    /**
     * Pauses execution until the IDE's background indexer has finished and
     * allows 'Compile on Save' events to settle. This prevents race conditions
     * when trying to execute recently modified code.
     *
     * @param project The project being executed.
     * @param cosStatus The current Compile on Save status of the project.
     * @throws InterruptedException if the wait is interrupted.
     */
    private void waitForIde(Project project, String cosStatus) throws InterruptedException {
        if (SourceUtils.isScanInProgress()) {
            log("Waiting for IDE to finish background scanning/indexing...");
            while (SourceUtils.isScanInProgress()) {
                log("Indexer still running, waiting 500ms...");
                Thread.sleep(500);
            }
            log("IDE indexing finished.");
        }

        // Mandatory settling delay if Compile on Save is active to ensure .class files are written.
        if (cosStatus.startsWith("all") || cosStatus.equalsIgnoreCase("Enabled")) {
            log("Waiting 1000ms for 'Compile on Save' file system events to settle...");
            Thread.sleep(1000);
            log("CoS settling complete.");
        }
    }

    /**
     * Extracts the base name of a JAR file by removing the extension and
     * version-specific suffixes. Used for deduplicating classpath entries.
     *
     * @param filename The full filename of the JAR.
     * @return The normalized base name.
     */
    private static String getJarBaseName(String filename) {
        String name = filename.toLowerCase();
        if (name.endsWith(".jar")) {
            name = name.substring(0, name.length() - 4);
        }
        return name.replaceAll("-(?:[0-9]|release|snapshot).*", "");
    }
}
