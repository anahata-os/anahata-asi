/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.intellij.tools.project.context.ProjectContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolParam;

import java.io.IOException;
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
}
