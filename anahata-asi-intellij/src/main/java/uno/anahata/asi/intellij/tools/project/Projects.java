/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.projectRoots.JavaSdk;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.intellij.tools.project.context.ProjectContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;

import java.io.IOException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

/**
 * A toolkit for interacting with the IntelliJ IDEA Project APIs.
 * <p>
 * This toolkit acts as a bridge between the model-agnostic Anahata core framework
 * and the IntelliJ Platform, providing real-time visibility into open projects,
 * directory structures, and project lifecycle management operations.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for using IntelliJ project APIs.")
public class Projects extends AnahataToolkit {

    /**
     * Default constructor for the IntelliJ Projects toolkit.
     */
    public Projects() {
        super();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the list of active project child providers, implementing a
     * dynamic synchronization pattern that captures newly opened or closed
     * projects in real-time.
     * </p>
     *
     * @return A list of context providers.
     */
    @Override
    public synchronized List<ContextProvider> getChildrenProviders() {
        syncProjects();
        return childrenProviders;
    }

    /**
     * Synchronizes project context providers with currently open IDE projects.
     */
    private synchronized void syncProjects() {
        Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
        List<String> currentPaths = new ArrayList<>();
        for (Project p : openProjects) {
            String path = p.getBasePath();
            if (path != null) {
                String absPath = Path.of(path).toAbsolutePath().toString();
                currentPaths.add(absPath);
                if (getProjectProvider(absPath).isEmpty()) {
                    ProjectContextProvider pcp = new ProjectContextProvider(this, p);
                    childrenProviders.add(pcp);
                    log.info("Added ProjectContextProvider for IntelliJ project: {}", pcp.getName());
                }
            }
        }
        childrenProviders.removeIf(cp -> {
            if (cp instanceof ProjectContextProvider pcp) {
                if (!currentPaths.contains(pcp.getProjectPath())) {
                    log.info("Removing ProjectContextProvider for closed IntelliJ project at: {}", pcp.getProjectPath());
                    pcp.getFlattenedHierarchy(false).forEach(child -> child.setProviding(false));
                    return true;
                }
            }
            return false;
        });
    }

    /**
     * Returns a project context provider by path.
     *
     * @param projectPath The absolute path of the project.
     * @return An Optional containing the provider.
     */
    public java.util.Optional<ProjectContextProvider> getProjectProvider(String projectPath) {
        return childrenProviders.stream()
                .filter(cp -> cp instanceof ProjectContextProvider)
                .map(cp -> (ProjectContextProvider) cp)
                .filter(pcp -> pcp.getProjectPath().equals(projectPath))
                .findFirst();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the master instruction guide on how to resolve files, folders,
     * and structures within the active IntelliJ IDE workspace.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
            "The Projects toolkit allows you to inspect and interact with the open projects in IntelliJ IDEA.\n" +
            "You can use it to explore file structures, list open projects, and perform project operations.\n" +
            "Use the directory structure provided in the RAG message to resolve files and use the Resources toolkit to read/write them."
        );
    }

    /**
     * {@inheritDoc}
     * <p>
     * Scans all open projects in the IntelliJ Platform, extracts their base paths,
     * and appends their physical file-system trees to the RAG message to provide
     * full workspace observability.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        StringBuilder sb = new StringBuilder();
        sb.append("## IntelliJ IDE Project Environment\n");

        Project[] openProjects = ProjectManager.getInstance().getOpenProjects();
        if (openProjects.length == 0) {
            sb.append("- **Open Projects**: None\n");
        } else {
            sb.append("- **Open Projects**:\n");
            for (Project project : openProjects) {
                String name = project.getName();
                String basePath = project.getBasePath();
                sb.append("  * **").append(name).append("**: `").append(basePath).append("`\n");
                
                if (basePath != null) {
                    sb.append("    * **Structure**:\n");
                    try {
                        appendDirectoryStructure(Path.of(basePath), sb, "      ");
                    } catch (Exception e) {
                        sb.append("      * [Error loading structure: ").append(e.getMessage()).append("]\n");
                    }
                }
            }
        }
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Recursively appends a clean, machine-readable directory structure to the builder.
     * <p>
     * Implements smart filtering to skip heavy target, out, and hidden folders, and 
     * utilizes a depth control threshold to prevent context-window token blowup.
     * </p>
     * 
     * @param dir The active directory path.
     * @param sb The string builder workspace.
     * @param indent The prefix indent spacer.
     * @throws IOException if directory reading fails.
     */
    private void appendDirectoryStructure(Path dir, StringBuilder sb, String indent) throws IOException {
        try (Stream<Path> stream = Files.list(dir)) {
            List<Path> paths = stream
                .filter(p -> {
                    String name = p.getFileName().toString();
                    return !name.startsWith(".") && !name.equals("target") && !name.equals("out");
                })
                .sorted((p1, p2) -> {
                    boolean d1 = Files.isDirectory(p1);
                    boolean d2 = Files.isDirectory(p2);
                    if (d1 != d2) {
                        return d1 ? -1 : 1;
                    }
                    return p1.compareTo(p2);
                })
                .toList();

            for (Path path : paths) {
                String name = path.getFileName().toString();
                if (Files.isDirectory(path)) {
                    sb.append(indent).append("- 📂 `").append(name).append("/`\n");
                    // Restrict deep recursion to prevent token waste
                    if (indent.length() < 12) {
                        appendDirectoryStructure(path, sb, indent + "  ");
                    }
                } else {
                    long size = Files.size(path);
                    sb.append(indent).append("- 📄 `").append(name).append("` [")
                      .append(String.format("%.1f KB", size / 1024.0)).append("]\n");
                }
            }
        }
    }

    /**
     * Returns a list of absolute paths of all currently open IntelliJ projects.
     * 
     * @return List of project base paths.
     */
    @AgiTool("Returns a list of absolute paths of all currently open IntelliJ projects.")
    public List<String> getOpenProjects() {
        List<String> paths = new ArrayList<>();
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            String basePath = project.getBasePath();
            if (basePath != null) {
                paths.add(basePath);
            }
        }
        return paths;
    }

    /**
     * Closes specific open projects in the IDE.
     * 
     * @param projectPaths A list of absolute paths of the projects to close.
     */
    @AgiTool("Closes one or more open projects in the IDE.")
    public void closeProjects(
            @AgiToolParam("A list of absolute paths of the projects to close.") List<String> projectPaths) {
        ProjectManager pm = ProjectManager.getInstance();
        for (Project project : pm.getOpenProjects()) {
            String basePath = project.getBasePath();
            if (basePath != null && projectPaths.contains(basePath)) {
                log.info("Closing IntelliJ project: {}", basePath);
                pm.closeAndDispose(project);
            }
        }
    }

    /**
     * Opens a project directory programmatically in the IntelliJ IDE.
     * <p>
     * Performs a thread-safe operation on the Event Dispatch Thread (EDT)
     * and uses progressive API resolution with reflection to guarantee
     * compatibility across different IntelliJ platform versions.
     * </p>
     * 
     * @param projectPath The absolute path of the project directory to open.
     * @return A status message describing the outcome of the open operation.
     */
    @AgiTool("Opens a project directory programmatically in the IntelliJ IDE.")
    public String openProject(
            @AgiToolParam("The absolute path of the project directory to open.") String projectPath) {
        log.info("Opening IntelliJ project: {}", projectPath);
        Path path = Path.of(projectPath);
        if (!Files.exists(path) || !Files.isDirectory(path)) {
            return "Error: Project directory not found at " + projectPath;
        }

        final List<String> result = new ArrayList<>();
        try {
            javax.swing.SwingUtilities.invokeAndWait(() -> {
                try {
                    // Try the modern ProjectUtil.openOrImport
                    try {
                        java.lang.reflect.Method m = Class.forName("com.intellij.ide.impl.ProjectUtil")
                                .getMethod("openOrImport", Path.class);
                        Object proj = m.invoke(null, path);
                        if (proj != null) {
                            result.add("Success: Opened project at " + projectPath);
                        } else {
                            result.add("Error: Failed to open project at " + projectPath);
                        }
                    } catch (Exception ex) {
                        // Fallback to ProjectManager
                        ProjectManager pm = ProjectManager.getInstance();
                        try {
                            java.lang.reflect.Method m = pm.getClass().getMethod("openProject", Path.class);
                            m.invoke(pm, path);
                            result.add("Success: Opened project via ProjectManager at " + projectPath);
                        } catch (Exception ex2) {
                            java.lang.reflect.Method m = pm.getClass().getMethod("loadAndOpenProject", String.class);
                            m.invoke(pm, projectPath);
                            result.add("Success: Opened project via loadAndOpenProject at " + projectPath);
                        }
                    }
                } catch (Exception e) {
                    log.error("Failed to open project in EDT: " + projectPath, e);
                    result.add("Error: Failed to open project: " + e.getMessage());
                }
            });
        } catch (Exception e) {
            log.error("EDT execution failed during project open: " + projectPath, e);
            result.add("Error: Thread execution failed: " + e.getMessage());
        }

        return result.isEmpty() ? "Error: Operation not completed" : result.get(0);
    }

    /**
     * Toggles the context provider state for a specific project.
     * <p>
     * Locates the appropriate provider by its canonical path and updates its
     * activation state.
     * </p>
     *
     * @param projectPath The absolute path of the project.
     * @param enabled Whether to enable the context provider.
     */
    @AgiTool("Enables or disables the top level project context provider (overview and anahata.md) for a specific project.")
    public void setProjectProviderEnabled(
            @AgiToolParam("The absolute path of the project.") String projectPath,
            @AgiToolParam("Whether to enable the context provider.") boolean enabled) {
        getProjectProvider(projectPath).ifPresent(pcp -> {
            pcp.setProviding(enabled);
            log.info("Project context for {} set to: {}", projectPath, enabled);
        });
    }

    /**
     * Compiles a project and returns a synchronous build result (error/warning counts).
     * <p>
     * Mirrors the NetBeans {@code Projects.invokeAction} build/rebuild capability using
     * IntelliJ's {@link CompilerManager}. Compilation is triggered on the EDT and this
     * method blocks on a latch off the EDT until the asynchronous build completes, so the
     * model receives the actual outcome rather than a fire-and-forget acknowledgement.
     * </p>
     *
     * @param projectPath the absolute path of the open project to build.
     * @param rebuild     {@code true} to force a full rebuild; {@code false} for an incremental make.
     * @return a human-readable build summary.
     * @throws AgiToolException if the project is not open or the build is interrupted.
     */
    @AgiTool("Compiles (make) or rebuilds an open project and returns the error/warning counts.")
    public String buildProject(
            @AgiToolParam("The absolute path of the open project to build.") String projectPath,
            @AgiToolParam("True to force a full rebuild; false for an incremental make.") boolean rebuild) throws AgiToolException {

        Project project = findProjectByPath(projectPath);
        if (project == null) {
            throw new AgiToolException("Project is not open: " + projectPath);
        }

        // Auto-configure SDK if not set
        ensureProjectSdkConfigured(project);

        CountDownLatch latch = new CountDownLatch(1);
        AtomicReference<String> summary = new AtomicReference<>("Build did not report a result.");
        ApplicationManager.getApplication().invokeLater(() -> {
            CompilerManager compilerManager = CompilerManager.getInstance(project);
            CompileScope scope = compilerManager.createProjectCompileScope(project);
            com.intellij.openapi.compiler.CompileStatusNotification callback = (aborted, errors, warnings, context) -> {
                summary.set((aborted ? "Build aborted. " : "Build finished. ")
                        + "Errors: " + errors + ", Warnings: " + warnings + ".");
                latch.countDown();
            };
            if (rebuild) {
                compilerManager.rebuild(callback);
            } else {
                compilerManager.make(scope, callback);
            }
        });

        try {
            if (!latch.await(20, TimeUnit.MINUTES)) {
                return "Build of " + projectPath + " is still running after 20 minutes (see the Build tool window).";
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AgiToolException("Build interrupted for: " + projectPath);
        }
        log(summary.get());
        return summary.get();
    }

    /**
     * Persists all unsaved editor documents across the IDE to disk.
     * <p>
     * Useful before a build or an external tool run so on-disk state matches the editor.
     * </p>
     *
     * @return a confirmation message.
     */
    @AgiTool("Saves all unsaved editor documents across the IDE to disk.")
    public String saveAllDocuments() {
        ApplicationManager.getApplication().invokeAndWait(() ->
                FileDocumentManager.getInstance().saveAllDocuments());
        return "Saved all documents.";
    }

    /**
     * Resolves an open {@link Project} by its canonical base path.
     *
     * @param projectPath the absolute base path.
     * @return the matching open project, or {@code null} if none is open at that path.
     */
    private Project findProjectByPath(String projectPath) {
        String target = Path.of(projectPath).toAbsolutePath().toString();
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            String basePath = project.getBasePath();
            if (basePath != null && Path.of(basePath).toAbsolutePath().toString().equals(target)) {
                return project;
            }
        }
        return null;
    }

    /**
     * Returns information about the currently configured Project SDK for an open project.
     *
     * @param projectPath The absolute path of the open project.
     * @return A formatted summary of the configured SDK, or an unconfigured warning.
     * @throws AgiToolException if the project is not open.
     */
    @AgiTool("Returns the currently configured Project SDK for an open project.")
    public String getProjectSdk(
            @AgiToolParam("The absolute path of the open project.") String projectPath) throws AgiToolException {
        Project project = findProjectByPath(projectPath);
        if (project == null) {
            throw new AgiToolException("Project is not open: " + projectPath);
        }

        Sdk sdk = ProjectRootManager.getInstance(project).getProjectSdk();
        if (sdk == null) {
            return "Project SDK is NOT configured for " + project.getName() + " (" + projectPath + ").";
        }

        return "Project SDK for " + project.getName() + ":\n" +
                "  - Name: " + sdk.getName() + "\n" +
                "  - Version: " + (sdk.getVersionString() != null ? sdk.getVersionString() : "unknown") + "\n" +
                "  - Home Path: " + sdk.getHomePath() + "\n" +
                "  - Type: " + sdk.getSdkType().getName();
    }

    /**
     * Lists all JDKs registered in IntelliJ's ProjectJdkTable as well as suggested system JDK paths.
     *
     * @return A formatted list of available SDKs and detected system JDK paths.
     */
    @AgiTool("Lists all configured SDKs in IntelliJ and available system JDK home paths.")
    public String listAvailableSdks() {
        StringBuilder sb = new StringBuilder();
        sb.append("## Registered IntelliJ SDKs\n");

        Sdk[] allJdks = ProjectJdkTable.getInstance().getAllJdks();
        if (allJdks.length == 0) {
            sb.append("- None registered in ProjectJdkTable.\n");
        } else {
            for (Sdk sdk : allJdks) {
                sb.append("- **").append(sdk.getName()).append("** (")
                  .append(sdk.getVersionString() != null ? sdk.getVersionString() : "unknown").append(")\n")
                  .append("  * Home Path: `").append(sdk.getHomePath()).append("`\n")
                  .append("  * Type: ").append(sdk.getSdkType().getName()).append("\n");
            }
        }

        sb.append("\n## Detected System JDK Home Paths\n");
        try {
            JavaSdk javaSdk = JavaSdk.getInstance();
            java.util.Collection<String> suggested = javaSdk.suggestHomePaths();
            if (suggested.isEmpty()) {
                sb.append("- None auto-detected by JavaSdk.\n");
            } else {
                for (String path : suggested) {
                    String version = javaSdk.getVersionString(path);
                    sb.append("- `").append(path).append("` (Version: ").append(version != null ? version : "unknown").append(")\n");
                }
            }
        } catch (Throwable t) {
            sb.append("- Error querying suggested home paths: ").append(t.getMessage()).append("\n");
        }

        String javaHome = System.getProperty("java.home");
        if (javaHome != null) {
            sb.append("- Running IDE Runtime: `").append(javaHome).append("`\n");
        }

        return sb.toString();
    }

    /**
     * Configures the project SDK for an open project.
     * <p>
     * If an existing registered SDK name is supplied (e.g. {@code "21"} or {@code "corretto-25"}),
     * it is attached directly to the project. If a directory path to a JDK home is supplied,
     * the JDK is first registered in {@link ProjectJdkTable} and then attached to the project.
     * </p>
     *
     * @param projectPath        The absolute path of the open project.
     * @param sdkNameOrHomePath  The registered SDK name or absolute path to a JDK home directory.
     * @return A confirmation message describing the configured SDK.
     * @throws AgiToolException if the project is not open or the SDK could not be configured.
     */
    @AgiTool("Configures the project SDK for an open project, either by registered SDK name or by JDK home directory path.")
    public String setProjectSdk(
            @AgiToolParam("The absolute path of the open project.") String projectPath,
            @AgiToolParam("The registered SDK name or absolute path to a JDK home directory.") String sdkNameOrHomePath) throws AgiToolException {

        Project project = findProjectByPath(projectPath);
        if (project == null) {
            throw new AgiToolException("Project is not open: " + projectPath);
        }

        if (sdkNameOrHomePath == null || sdkNameOrHomePath.isBlank()) {
            throw new AgiToolException("SDK name or home path cannot be blank.");
        }

        AtomicReference<Sdk> selectedSdk = new AtomicReference<>();
        ProjectJdkTable jdkTable = ProjectJdkTable.getInstance();

        // 1. Check if it matches an existing registered SDK name
        Sdk existing = jdkTable.findJdk(sdkNameOrHomePath.trim());
        if (existing != null) {
            selectedSdk.set(existing);
        } else {
            // 2. Treat as directory path
            Path path = Path.of(sdkNameOrHomePath.trim());
            if (!Files.exists(path) || !Files.isDirectory(path)) {
                throw new AgiToolException("SDK home path does not exist or is not a directory: " + sdkNameOrHomePath);
            }

            JavaSdk javaSdk = JavaSdk.getInstance();
            String homePathStr = path.toAbsolutePath().toString();
            String suggestedName = javaSdk.suggestSdkName(null, homePathStr);
            if (suggestedName == null || suggestedName.isBlank()) {
                suggestedName = "JDK-" + path.getFileName().toString();
            }

            // Ensure unique name in ProjectJdkTable
            String finalName = suggestedName;
            int counter = 1;
            while (jdkTable.findJdk(finalName) != null && !jdkTable.findJdk(finalName).getHomePath().equals(homePathStr)) {
                finalName = suggestedName + " (" + (++counter) + ")";
            }

            Sdk existingByPath = null;
            for (Sdk s : jdkTable.getAllJdks()) {
                if (homePathStr.equals(s.getHomePath())) {
                    existingByPath = s;
                    break;
                }
            }

            if (existingByPath != null) {
                selectedSdk.set(existingByPath);
            } else {
                final String sdkName = finalName;
                ApplicationManager.getApplication().invokeAndWait(() -> {
                    ApplicationManager.getApplication().runWriteAction(() -> {
                        Sdk newJdk = javaSdk.createJdk(sdkName, homePathStr, false);
                        jdkTable.addJdk(newJdk);
                        selectedSdk.set(newJdk);
                        log.info("Registered new JDK in ProjectJdkTable: {} -> {}", sdkName, homePathStr);
                    });
                });
            }
        }

        Sdk sdkToApply = selectedSdk.get();
        if (sdkToApply == null) {
            throw new AgiToolException("Failed to resolve or create SDK for: " + sdkNameOrHomePath);
        }

        ApplicationManager.getApplication().invokeAndWait(() -> {
            ApplicationManager.getApplication().runWriteAction(() -> {
                ProjectRootManager.getInstance(project).setProjectSdk(sdkToApply);
                log.info("Set project SDK for {} to {}", project.getName(), sdkToApply.getName());
            });
        });

        String msg = "Successfully configured Project SDK for " + project.getName() + ": " +
                sdkToApply.getName() + " (" + (sdkToApply.getVersionString() != null ? sdkToApply.getVersionString() : "") +
                ") at " + sdkToApply.getHomePath();
        log(msg);
        return msg;
    }

    /**
     * Automatically discovers the best available JDK on the system and assigns it as the Project SDK.
     *
     * @param projectPath The absolute path of the open project.
     * @return A status message describing the configured SDK.
     * @throws AgiToolException if the project is not open or no JDK can be found.
     */
    @AgiTool("Automatically detects and configures the best available JDK for the project if not already set.")
    public String autoConfigureProjectSdk(
            @AgiToolParam("The absolute path of the open project.") String projectPath) throws AgiToolException {
        Project project = findProjectByPath(projectPath);
        if (project == null) {
            throw new AgiToolException("Project is not open: " + projectPath);
        }

        Sdk sdk = ensureProjectSdkConfigured(project);
        if (sdk == null) {
            throw new AgiToolException("Unable to auto-detect or configure a JDK for project: " + projectPath);
        }

        return "Project SDK for " + project.getName() + " is configured: " +
                sdk.getName() + " (" + (sdk.getVersionString() != null ? sdk.getVersionString() : "") +
                ") at " + sdk.getHomePath();
    }

    /**
     * Ensures that the specified project has a valid Project SDK attached, auto-detecting and registering
     * one if needed.
     *
     * @param project The open project to inspect.
     * @return The active or newly configured Sdk, or {@code null} if no JDK could be found.
     */
    private Sdk ensureProjectSdkConfigured(Project project) {
        ProjectRootManager rootManager = ProjectRootManager.getInstance(project);
        Sdk existing = rootManager.getProjectSdk();
        if (existing != null) {
            return existing;
        }

        log.info("Project SDK is unconfigured for {}. Attempting auto-detection...", project.getName());
        ProjectJdkTable jdkTable = ProjectJdkTable.getInstance();
        Sdk[] allJdks = jdkTable.getAllJdks();

        // 1. Prefer an already registered Java SDK
        JavaSdk javaSdk = JavaSdk.getInstance();
        for (Sdk s : allJdks) {
            if (s.getSdkType() instanceof JavaSdk) {
                ApplicationManager.getApplication().invokeAndWait(() -> {
                    ApplicationManager.getApplication().runWriteAction(() -> {
                        rootManager.setProjectSdk(s);
                    });
                });
                log.info("Auto-assigned existing JDK '{}' to project {}", s.getName(), project.getName());
                return s;
            }
        }

        // 2. Look for suggested home paths
        try {
            java.util.Collection<String> suggested = javaSdk.suggestHomePaths();
            for (String homePath : suggested) {
                if (Files.exists(Path.of(homePath))) {
                    String name = javaSdk.suggestSdkName(null, homePath);
                    if (name == null || name.isBlank()) {
                        name = "JDK-" + Path.of(homePath).getFileName().toString();
                    }
                    final String finalName = name;
                    AtomicReference<Sdk> created = new AtomicReference<>();
                    ApplicationManager.getApplication().invokeAndWait(() -> {
                        ApplicationManager.getApplication().runWriteAction(() -> {
                            Sdk newSdk = javaSdk.createJdk(finalName, homePath, false);
                            jdkTable.addJdk(newSdk);
                            rootManager.setProjectSdk(newSdk);
                            created.set(newSdk);
                        });
                    });
                    if (created.get() != null) {
                        log.info("Auto-registered and assigned detected JDK '{}' ({}) to project {}",
                                finalName, homePath, project.getName());
                        return created.get();
                    }
                }
            }
        } catch (Throwable t) {
            log.warn("Error auto-detecting JDK paths: {}", t.getMessage());
        }

        // 3. Fallback to host running JVM (e.g. JBR)
        String javaHome = System.getProperty("java.home");
        if (javaHome != null && Files.exists(Path.of(javaHome))) {
            try {
                String name = "IDE-JBR-" + System.getProperty("java.specification.version", "runtime");
                AtomicReference<Sdk> created = new AtomicReference<>();
                ApplicationManager.getApplication().invokeAndWait(() -> {
                    ApplicationManager.getApplication().runWriteAction(() -> {
                        Sdk newSdk = javaSdk.createJdk(name, javaHome, false);
                        jdkTable.addJdk(newSdk);
                        rootManager.setProjectSdk(newSdk);
                        created.set(newSdk);
                    });
                });
                if (created.get() != null) {
                    log.info("Auto-registered and assigned host JBR '{}' ({}) to project {}",
                            name, javaHome, project.getName());
                    return created.get();
                }
            } catch (Throwable t) {
                log.warn("Error configuring host JBR fallback: {}", t.getMessage());
            }
        }

        return null;
    }
}
