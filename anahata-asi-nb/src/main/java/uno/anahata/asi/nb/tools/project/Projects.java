/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;
import java.util.Set;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.java.queries.SourceLevelQuery;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.modules.maven.api.NbMavenProject;
import org.netbeans.spi.project.ActionProvider;
import org.netbeans.spi.project.SubprojectProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.FileUtil;
import org.openide.util.Lookup;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.nb.tools.project.context.ProjectContextProvider;
import uno.anahata.asi.nb.tools.maven.DependencyScope;
import uno.anahata.asi.nb.tools.maven.Maven;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.nb.tools.project.nb.AnahataProjectAnnotator;

/**
 * A toolkit for interacting with the NetBeans Project APIs.
 * It provides tools for managing open projects, retrieving project overviews,
 * and invoking project-level actions.
 * <p>
 * This toolkit also acts as a {@link ContextProvider}, managing a hierarchy of
 * {@link ProjectContextProvider}s for all open projects in the IDE.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for using netbeans project apis.")
public class Projects extends AnahataToolkit implements PropertyChangeListener {

    /** 
     * A flag to track if the toolkit is already listening to global project changes.
     * This is used for lazy initialization of the {@link OpenProjects} listener.
     * It is distinct from the 'providing' flag, which controls context injection.
     */
    private transient boolean listening = false;


    // --- PUBLIC STATIC UTILITIES ---

    /**
     * Convenient entry point for other tools to find an open project by its directory path.
     * 
     * @param projectDirectoryPath The absolute path to the project directory.
     * @return The {@link Project} instance.
     * @throws Exception if the project is not found or not open.
     */
    public static Project findOpenProject(String projectDirectoryPath) throws Exception {
        FileObject dir = FileUtil.toFileObject(new File(projectDirectoryPath));
        if (dir == null) {
            throw new Exception("Project directory not found: " + projectDirectoryPath);
        }
        for (Project p : OpenProjects.getDefault().getOpenProjects()) {
            if (p.getProjectDirectory().equals(dir)) {
                return p;
            }
        }
        throw new Exception("Project not found or is not open: " + projectDirectoryPath);
    }

    /**
     * Gets a list of supported NetBeans Actions for a given project.
     * 
     * @param projectPath The absolute path of the project.
     * @return An array of action names.
     * @throws Exception if the project is not found.
     */
    public static String[] getSupportedActions(String projectPath) throws Exception {
        Project p = Projects.findOpenProject(projectPath);
        ActionProvider ap = p.getLookup().lookup(ActionProvider.class);
        return ap != null ? ap.getSupportedActions() : new String[0];
    }

    // --- AI TOOL METHODS ---

    /**
     * Returns a list of absolute paths for all currently open projects in the IDE.
     * 
     * @return A list of project paths.
     */
    @AiTool("Returns a List of absolute paths for all currently open projects.")
    public List<String> getOpenProjects() {
        List<String> projectPaths = new ArrayList<>();
        for (Project project : OpenProjects.getDefault().getOpenProjects()) {
            projectPaths.add(project.getProjectDirectory().getPath());
        }
        return projectPaths;
    }

    /**
     * Returns the absolute path of the current 'Main Project' in the IDE.
     * 
     * @return The main project path, or null if none is set.
     */
    @AiTool("Returns the absolute path of the current 'Main Project' in the IDE, or null if none is set.")
    public String getMainProject() {
        Project p = OpenProjects.getDefault().getMainProject();
        return p != null ? p.getProjectDirectory().getPath() : null;
    }

    /**
     * Sets a specific open project as the 'Main Project' in the IDE.
     * 
     * @param projectPath The absolute path of the project to set as main.
     * @throws Exception if the project is not found.
     */
    @AiTool("Sets a specific open project as the 'Main Project'.")
    public void setMainProject(@AiToolParam("The absolute path of the project to set as main.") String projectPath) throws Exception {
        Project p = findOpenProject(projectPath);
        OpenProjects.getDefault().setMainProject(p);
    }

    /**
     * Closes one or more open projects in the IDE.
     * 
     * @param projectPaths A list of absolute paths of the projects to close.
     * @throws Exception if any project is not found.
     */
    @AiTool("Closes one or more open projects.")
    public void closeProjects(@AiToolParam("A list of absolute paths of the projects to close.") List<String> projectPaths) throws Exception {
        List<Project> toClose = new ArrayList<>();
        for (String path : projectPaths) {
            toClose.add(findOpenProject(path));
        }
        OpenProjects.getDefault().close(toClose.toArray(new Project[0]));
    }

    /**
     * Opens a project in the IDE and waits for the operation to complete.
     * 
     * @param projectPath The absolute path or relative folder name.
     * @param openSubprojects Whether to automatically open all subprojects (modules).
     * @return A success or error message.
     * @throws Exception if an error occurs during opening.
     */
    @AiTool("Opens a project in the IDE, waiting for the asynchronous open operation to complete. This tool prefers the full absolute path as the project path.")
    public String openProject(
            @AiToolParam("The absolute path to the project (recommended) or the folder name relative to NetBeansProjects.") String projectPath,
            @AiToolParam("Whether to automatically open all subprojects (e.g. child modules in a Maven parent).") boolean openSubprojects) throws Exception {
        File projectDir;
        if (new File(projectPath).isAbsolute()) {
            projectDir = new File(projectPath);
        } else {
            String projectsFolderPath = System.getProperty("user.home") + File.separator + "NetBeansProjects";
            projectDir = new File(projectsFolderPath, projectPath);
        }

        if (!projectDir.exists() || !projectDir.isDirectory()) {
            return "Error: Project directory not found at " + projectDir.getAbsolutePath();
        }

        FileObject projectFob = FileUtil.toFileObject(FileUtil.normalizeFile(projectDir));
        if (projectFob == null) {
            return "Error: Could not find project directory: " + projectDir.getAbsolutePath();
        }

        Project projectToOpen = ProjectManager.getDefault().findProject(projectFob);
        if (projectToOpen == null) {
            return "Error: Could not find a project in the specified directory: " + projectDir.getAbsolutePath();
        }

        boolean alreadyOpen = false;
        for (Project p : OpenProjects.getDefault().getOpenProjects()) {
            if (p.getProjectDirectory().equals(projectFob)) {
                alreadyOpen = true;
                break;
            }
        }

        if (!alreadyOpen) {
            final CountDownLatch latch = new CountDownLatch(1);
            final PropertyChangeListener listener = (PropertyChangeEvent evt) -> {
                if (OpenProjects.PROPERTY_OPEN_PROJECTS.equals(evt.getPropertyName())) {
                    for (Project p : OpenProjects.getDefault().getOpenProjects()) {
                        if (p.equals(projectToOpen)) {
                            latch.countDown();
                            break;
                        }
                    }
                }
            };

            OpenProjects.getDefault().addPropertyChangeListener(listener);

            try {
                OpenProjects.getDefault().open(new Project[]{projectToOpen}, false, true);
                if (!latch.await(30, TimeUnit.SECONDS)) {
                    return "Error: Timed out after 30 seconds waiting for project '" + projectPath + "' to open.";
                }
            } finally {
                OpenProjects.getDefault().removePropertyChangeListener(listener);
            }
        }

        if (openSubprojects) {
            openSubprojects(projectPath);
            return "Success: Project '" + projectPath + "' and its subprojects opened successfully.";
        }

        return "Success: Project '" + projectPath + "' opened successfully.";
    }

    /**
     * Opens all subprojects of a given project.
     * 
     * @param projectPath The absolute path of the parent project.
     * @throws Exception if the project is not found.
     */
    @AiTool("Opens all subprojects of a given project.")
    public void openSubprojects(@AiToolParam("The absolute path of the parent project.") String projectPath) throws Exception {
        Project parent = findOpenProject(projectPath);
        SubprojectProvider spp = parent.getLookup().lookup(SubprojectProvider.class);
        if (spp != null) {
            Set<? extends Project> subprojects = spp.getSubprojects();
            if (!subprojects.isEmpty()) {
                OpenProjects.getDefault().open(subprojects.toArray(new Project[0]), false, true);
            }
        }
    }

    /**
     * Gets a structured, context-aware overview of a project, including its source tree and metadata.
     * 
     * @param projectPath The absolute path of the project.
     * @return A {@link ProjectOverview} object containing the project's structure and status.
     */
    @AiTool("Gets a structured, context-aware overview of a project, including root files, source tree, the in-context status of each file and a list of supported NetBeans Actions.")
    @SneakyThrows
    public ProjectOverview getOverview(@AiToolParam("The absolute path of the project.") String projectPath) {
        Project target = findOpenProject(projectPath);

        ProjectInformation info = ProjectUtils.getInformation(target);
        FileObject root = target.getProjectDirectory();
        List<String> actions = Collections.emptyList();
        ActionProvider ap = target.getLookup().lookup(ActionProvider.class);
        if (ap != null) {
            actions = Arrays.asList(ap.getSupportedActions());
        }

        List<ProjectFile> rootFiles = new ArrayList<>();
        List<String> rootFolderNames = new ArrayList<>();
        List<SourceFolder> sourceFolders = new ArrayList<>();

        for (FileObject child : root.getChildren()) {
            if (child.isFolder()) {
                rootFolderNames.add(child.getNameExt());
            } else {
                ProjectFile pf = createProjectFile(child);
                rootFiles.add(pf);
            }
        }

        Sources sources = ProjectUtils.getSources(target);
        List<SourceGroup> allSourceGroups = new ArrayList<>();
        allSourceGroups.addAll(Arrays.asList(sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA)));
        allSourceGroups.addAll(Arrays.asList(sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_RESOURCES)));

        for (SourceGroup group : allSourceGroups) {
            FileObject srcRoot = group.getRootFolder();
            sourceFolders.add(buildSourceFolderTree(srcRoot, group.getDisplayName()));
        }
        
        // Project-agnostic properties
        String javaSourceLevel = SourceLevelQuery.getSourceLevel(target.getProjectDirectory());

        // Maven-specific properties
        List<DependencyScope> mavenDeclaredDependencies = null;
        String javaTargetLevel = null;
        String sourceEncoding = null;
        String packaging = null;
        
        NbMavenProject nbMavenProject = target.getLookup().lookup(NbMavenProject.class);
        if (nbMavenProject != null) {
            // Get declared dependencies
            List<DependencyScope> temp = Maven.getDeclaredDependencies(projectPath);
            if (temp != null && !temp.isEmpty()) {
                mavenDeclaredDependencies = temp;
            }

            // Get compiler properties from the Maven model
            org.apache.maven.project.MavenProject rawMvnProject = nbMavenProject.getMavenProject();
            packaging = rawMvnProject.getPackaging();
            
            javaSourceLevel = rawMvnProject.getProperties().getProperty("maven.compiler.release");
            if (javaSourceLevel == null) {
                javaSourceLevel = rawMvnProject.getProperties().getProperty("maven.compiler.source");
            }
            
            javaTargetLevel = rawMvnProject.getProperties().getProperty("maven.compiler.target");
            sourceEncoding = rawMvnProject.getProperties().getProperty("project.build.sourceEncoding");
        }

        return new ProjectOverview(
                root.getNameExt(),
                info.getDisplayName(),                
                root.getPath(),
                packaging,
                rootFiles,
                rootFolderNames,
                sourceFolders,
                actions,
                mavenDeclaredDependencies,
                javaSourceLevel,
                javaTargetLevel,
                sourceEncoding
        );
    }

    /**
     * Enables or disables the project context provider (overview and anahata.md) for a specific project.
     * 
     * @param projectPath The absolute path of the project.
     * @param enabled {@code true} to enable, {@code false} to disable.
     */
    @AiTool("Enables or disables the project context provider (overview and anahata.md) for a specific project.")
    public void setProjectProviderEnabled(
            @AiToolParam("The absolute path of the project.") String projectPath, 
            @AiToolParam("Whether to enable the context provider.") boolean enabled) {
        getProjectProvider(projectPath).ifPresent(pcp -> {
            pcp.setProviding(enabled);
            log.info("Project context for {} set to: {}", projectPath, enabled);
            try {
                Project p = findOpenProject(projectPath);
                AnahataProjectAnnotator.fireRefreshAll(p);
            } catch (Exception ex) {
                log.warn("Failed to find project for refresh: " + projectPath, ex);
                AnahataProjectAnnotator.fireRefreshAll(null);
            }
        });
    }

    /**
     * Invokes a NetBeans project action asynchronously.
     * 
     * @param projectPath The absolute path of the project.
     * @param action The action name (e.g., 'build', 'run').
     * @throws Exception if the action is not supported or enabled.
     */
    @AiTool("Invokes ('Fires and forgets') a NetBeans Project supported Action (like 'run' or 'build')  on a given open Project (via ActionProvider).\n"
            + "\n\nThis method is always asynchronous by design. (regardless of whether you specify the asynchronous parameter or not)"
            + "as this tool does not return any values nor you can ensure that the action finished when this tool returns."
            + "\nUse Maven.runGoals or JVM tools or any other synchronous tools if you need to ensure the action succeeded or the action you require produces an output you need")
    public void invokeAction(
            @AiToolParam("The absolute path of the project.") String projectPath,
            @AiToolParam("The action to invoke") String action) throws Exception {
        Project project = findOpenProject(projectPath);
        ActionProvider ap = project.getLookup().lookup(ActionProvider.class);
        if (ap == null) {
            throw new IllegalArgumentException(project + " does not have ActionProvider");
        }

        Lookup context = project.getLookup();

        if (ap.isActionEnabled(action, context)) {
            ap.invokeAction(action, context);
        } else {
            String[] supportedActions = ap.getSupportedActions();
            boolean isSupported = Arrays.asList(supportedActions).contains(action);
            if (isSupported) {
                throw new IllegalArgumentException("The '" + action + "' action is supported but not currently enabled for project '" + project + "'.");
            } else {
                throw new IllegalArgumentException("The '" + action + "' action is not supported by project '" + project + "'. Supported actions are: " + String.join(", ", supportedActions));
            }
        }
    }

    /**
     * Lists all known preferences for a given project.
     * 
     * @param projectPath The project absolute path.
     * @return A string containing the preferences.
     * @throws Exception if the project is not found.
     */
    @AiTool("Lists all known preferences for a given project.")
    public String listAllKnownPreferences(@AiToolParam("The absolute path of the project.") String projectPath) throws Exception {
        Project project = findOpenProject(projectPath);
        if (project == null) {
            return "Project not found: " + projectPath;
        }
        StringBuilder sb = new StringBuilder();
        Class<?>[] contextClasses = new Class<?>[]{
            org.netbeans.api.project.ProjectUtils.class,};

        for (Class<?> ctx : contextClasses) {
            sb.append("Preferences for context: ").append(ctx.getName()).append("\n");
            Preferences prefs = ProjectUtils.getPreferences(project, ctx, true);
            try {
                String[] keys = prefs.keys();
                if (keys.length == 0) {
                    sb.append("  (no preferences)\n");
                } else {
                    for (String key : keys) {
                        sb.append("  ").append(key)
                                .append(" = ").append(prefs.get(key, "<no value>")).append("\n");
                    }
                }
            } catch (BackingStoreException e) {
                sb.append("  Failed to read preferences: ").append(e.getMessage()).append("\n");
            }
            sb.append("\n");
        }

        return sb.toString();
    }

    // --- CONTEXT PROVIDER LOGIC ---

    /**
     * {@inheritDoc}
     * Overridden to lazily initialize the project change listener and sync the provider hierarchy.
     * 
     * @return The list of child context providers (one per open project).
     */
    @Override
    public synchronized List<ContextProvider> getChildrenProviders() {
        if (!listening) {
            OpenProjects.getDefault().addPropertyChangeListener(this);
            syncProjects();
            listening = true;
        }
        return childrenProviders;
    }

    /**
     * Synchronizes the list of {@link ProjectContextProvider}s with the currently open projects in the IDE.
     * This method is called whenever the set of open projects changes.
     */
    private synchronized void syncProjects() {
        Project[] openProjects = OpenProjects.getDefault().getOpenProjects();
        List<String> currentPaths = new ArrayList<>();
        
        for (Project p : openProjects) {
            String path = p.getProjectDirectory().getPath();
            currentPaths.add(path);
            
            if (getProjectProvider(path).isEmpty()) {
                ProjectContextProvider pcp = new ProjectContextProvider(this, p);
                childrenProviders.add(pcp);
                log.info("Added ProjectContextProvider for: {}", pcp.getName());
            }
        }
        
        // Remove providers for closed projects
        childrenProviders.removeIf(cp -> {
            if (cp instanceof ProjectContextProvider pcp) {
                if (!currentPaths.contains(pcp.getProjectPath())) {
                    log.info("Removing ProjectContextProvider for closed project at: {}", pcp.getProjectPath());
                    // Ensure resources are cleaned up and badges are removed
                    pcp.getFlattenedHierarchy(false).forEach(child -> child.setProviding(false));
                    return true;
                }
            }
            return false;
        });
    }

    /**
     * Finds a {@link ProjectContextProvider} for a specific project path.
     * 
     * @param projectPath The absolute path of the project.
     * @return An Optional containing the provider if found.
     */
    public Optional<ProjectContextProvider> getProjectProvider(String projectPath) {
        return childrenProviders.stream()
                .filter(cp -> cp instanceof ProjectContextProvider)
                .map(cp -> (ProjectContextProvider) cp)
                .filter(pcp -> pcp.getProjectPath().equals(projectPath))
                .findFirst();
    }

    /**
     * {@inheritDoc}
     * Listens for changes in the set of open projects and triggers a sync.
     * 
     * @param evt The property change event from {@link OpenProjects}.
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if (OpenProjects.PROPERTY_OPEN_PROJECTS.equals(evt.getPropertyName())) {
            syncProjects();
        }
    }

    // --- PRIVATE HELPERS ---

    /**
     * Recursively builds a tree structure of source folders and files.
     * 
     * @param folder The root folder to start from.
     * @param displayName The display name for the root folder.
     * @return A {@link SourceFolder} representing the tree.
     * @throws FileStateInvalidException if the file state is invalid.
     */
    private SourceFolder buildSourceFolderTree(FileObject folder, String displayName) throws FileStateInvalidException {
        if (!folder.isFolder()) {
            throw new IllegalArgumentException("FileObject must be a folder: " + folder.getPath());
        }

        List<ProjectFile> files = new ArrayList<>();
        List<SourceFolder> subfolders = new ArrayList<>();

        for (FileObject child : folder.getChildren()) {
            if (child.isFolder()) {
                subfolders.add(buildSourceFolderTree(child, child.getNameExt()));
            } else {
                files.add(createProjectFile(child));
            }
        }

        long recursiveSize = files.stream().mapToLong(ProjectFile::getSize).sum()
                + subfolders.stream().mapToLong(SourceFolder::getRecursiveSize).sum();
        
        String folderName = folder.getNameExt();
        String finalDisplayName = folderName.equals(displayName) ? null : displayName;

        return new SourceFolder(finalDisplayName, folder.getPath(), recursiveSize, files.isEmpty() ? null : files, subfolders.isEmpty() ? null: subfolders);
    }

    /**
     * Creates a {@link ProjectFile} metadata object for a given {@link FileObject}.
     * It checks the AI context to determine the file's current status (e.g., [IC], [STALE]).
     * 
     * @param fo The file object.
     * @return The project file metadata.
     * @throws FileStateInvalidException if the file state is invalid.
     */
    private ProjectFile createProjectFile(FileObject fo) throws FileStateInvalidException {
        String path = fo.getPath();
        String status = null;
        Optional<? extends AbstractPathResource<?, ?>> resOpt = getResourceManager().findByPath(path);
        if (resOpt.isPresent()) {
            AbstractPathResource<?, ?> res = resOpt.get();
            if (!res.exists()) {
                status = "[DELETED]";
            } else {
                try {
                    if (res.isStale()) {
                        status = "[STALE]";
                    } else {
                        status = "[IC]";
                    }
                } catch (IOException e) {
                    log.error("Error checking staleness for: " + path, e);
                    status = "[IC]";
                }
            }
        }
        
        return new ProjectFile(
                fo.getNameExt(),
                fo.getSize(),
                fo.lastModified().getTime(),
                status,
                path
        );
    }
}
