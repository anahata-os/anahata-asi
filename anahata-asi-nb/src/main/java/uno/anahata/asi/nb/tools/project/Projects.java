/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.Set;
import javax.tools.Diagnostic;
import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.java.queries.SourceLevelQuery;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.spi.project.AuxiliaryConfiguration;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.api.project.ui.OpenProjects;
import org.netbeans.modules.maven.api.NbMavenProject;
import org.netbeans.modules.parsing.spi.indexing.ErrorsCache;
import org.netbeans.spi.project.ActionProvider;
import org.netbeans.spi.project.SubprojectProvider;
import org.netbeans.spi.project.ui.ProjectProblemsProvider;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.util.Lookup;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.nb.tools.project.context.ProjectContextProvider;
import uno.anahata.asi.nb.tools.maven.Maven;
import uno.anahata.asi.nb.tools.maven.DependencyScope;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.nb.tools.project.alerts.JavacAlert;
import uno.anahata.asi.nb.tools.project.alerts.ProjectAlert;
import uno.anahata.asi.nb.tools.project.alerts.ProjectDiagnostics;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * A toolkit for interacting with the NetBeans Project APIs.
 * <p>
 * This toolkit acts as a global ContextProvider that manages a hierarchy of 
 * ProjectContextProviders, one for each open project in the IDE.
 * </p>
 * <p>
 * It uses Canonical Paths for all registrations and lookups to ensure consistency 
 * across physical and virtual (MasterFS) FileObject proxies.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for using netbeans project apis.")
public class Projects extends AnahataToolkit implements PropertyChangeListener {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
            "### Compile on Save (CoS) Management:\n" +
            "NetBeans Maven projects determine the 'Compile on Save' status using a tiered priority system. " +
            "When a user asks to change this setting, you should offer these options:\n" +
            "1. **Project POM**: Add/update `<netbeans.compile.on.save>all</netbeans.compile.on.save>` in the project's `pom.xml` properties.\n" +
            "2. **Parent POM**: If it's a multi-module project, you can set it in the parent POM to apply it to all modules.\n" +
            "3. **IDE Override**: Use the `setCompileOnSaveOverride` tool. This writes to `nb-configuration.xml` and is the same as using the IDE's 'Project Properties' dialog. " +
            "This override ALWAYS wins over POM properties.\n\n" +
            "**Strategy**: If the project is currently 'Disabled' via an override, changing the POM will have no effect until the override is removed or changed."
        );
    }

    /**
     * Populates the system message with an overview of the IDE project environment.
     * 
     * @param ragMessage The target message.
     */
    @Override
    public void populateMessage(uno.anahata.asi.model.core.RagMessage ragMessage) {
        String projectsFolder = getNetBeansProjectsFolder();
        StringBuilder sb = new StringBuilder();
        sb.append("## IDE Project Environment\n");
        sb.append("- **NetBeansProjects Folder**: ").append(projectsFolder).append("\n");
        
        List<String> folderNames = listAvailableProjectFolders();
        sb.append("- **Available Project Folders**: ").append(folderNames).append("\n");
        
        List<String> openProjects = getOpenProjects();
        sb.append("- **Current Open Projects**: ").append(openProjects).append("\n");
        
        String mainProject = getMainProject();
        sb.append("- **Current Main Project**: ").append(mainProject != null ? mainProject : "None").append("\n");
        
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Lists all project folders found in the user's NetBeansProjects directory.
     * 
     * @return A list of project folder names.
     */
    //@AiTool("Lists all project folders found in the user's NetBeansProjects directory.")
    public List<String> listAvailableProjectFolders() {
        String projectsFolder = getNetBeansProjectsFolder();
        File dir = new File(projectsFolder);
        if (dir.exists() && dir.isDirectory()) {
            File[] subDirs = dir.listFiles(File::isDirectory);
            if (subDirs != null) {
                return Arrays.stream(subDirs)
                        .map(File::getName)
                        .sorted()
                        .collect(java.util.stream.Collectors.toList());
            }
        }
        return Collections.emptyList();
    }

    private transient boolean listening = false;

    /**
     * Finds an open project by its directory path.
     *
     * @param projectDirectoryPath The absolute path to the project directory.
     * @return The Project instance.
     * @throws Exception if the project is not found or not open.
     */
    public static Project findOpenProject(String projectDirectoryPath) throws Exception {
        FileObject dir = FileUtil.toFileObject(new File(projectDirectoryPath));
        if (dir == null) {
            throw new Exception("Project directory not found: " + projectDirectoryPath);
        }
        for (Project p : OpenProjects.getDefault().getOpenProjects()) {
            if (getCanonicalPath(p.getProjectDirectory()).equals(projectDirectoryPath)) {
                return p;
            }
        }
        throw new Exception("Project not found or is not open: " + projectDirectoryPath);
    }

    /**
     * Returns a list of absolute paths for all currently open projects.
     *
     * @return A list of project paths.
     */
    //@AiTool("Returns a List of absolute paths for all currently open projects.")
    public List<String> getOpenProjects() {
        List<String> projectPaths = new ArrayList<>();
        for (Project project : OpenProjects.getDefault().getOpenProjects()) {
            projectPaths.add(getCanonicalPath(project.getProjectDirectory()));
        }
        return projectPaths;
    }

    /**
     * Returns the absolute path of the current 'Main Project'.
     *
     * @return The main project path, or null if none is set.
     */
    //@AiTool("Returns the absolute path of the current 'Main Project' in the IDE, or null if none is set.")
    public String getMainProject() {
        Project p = OpenProjects.getDefault().getMainProject();
        return p != null ? getCanonicalPath(p.getProjectDirectory()) : null;
    }

    /**
     * Sets a specific open project as the 'Main Project'.
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
     * Closes one or more open projects.
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
     * Opens a project in the IDE and waits for completion.
     *
     * @param projectPath The absolute path or relative folder name.
     * @param openSubprojects Whether to automatically open all subprojects.
     * @return A success or error message.
     * @throws Exception if an error occurs.
     */
    @AiTool("Opens a project in the IDE, waiting for the asynchronous open operation to complete. This tool prefers the full absolute path as the project path.")
    public String openProject(
            @AiToolParam("The absolute path to the project (recommended) or the folder name relative to NetBeansProjects.") String projectPath,
            @AiToolParam("Whether to automatically open all subprojects (e.g. child modules in a Maven parent).") boolean openSubprojects) throws Exception {
        File projectDir;
        if (new File(projectPath).isAbsolute()) {
            projectDir = new File(projectPath);
        } else {
            String projectsFolderPath = getNetBeansProjectsFolder();
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
     * Gets a structured overview of a project.
     * 
     * @param projectPath The path to the project.
     * @return Project metadata and actions.
     * @throws Exception if project not found.
     */
    /*
    @AiTool("Gets a structured, context-aware overview of a project, including metadata, supported actions, and declared dependencies.\n"
            + "WARNING: DO NOT call this tool if the 'Project Overview' context provider for this project is already enabled ('providing').")
    */
    public ProjectOverview getOverview(@AiToolParam("The absolute path of the project.") String projectPath) throws Exception {
        Project target = findOpenProject(projectPath);

        ProjectInformation info = ProjectUtils.getInformation(target);
        FileObject root = target.getProjectDirectory();
        List<String> actions = Collections.emptyList();
        ActionProvider ap = target.getLookup().lookup(ActionProvider.class);
        if (ap != null) {
            actions = Arrays.asList(ap.getSupportedActions());
        }

        String javaSourceLevel = SourceLevelQuery.getSourceLevel(target.getProjectDirectory());
        List<DependencyScope> mavenDeclaredDependencies = null;
        String javaTargetLevel = null;
        String sourceEncoding = null;
        String packaging = null;

        NbMavenProject nbMavenProject = target.getLookup().lookup(NbMavenProject.class);
        if (nbMavenProject != null) {
            List<DependencyScope> temp = Maven.getDeclaredDependencies(projectPath);
            if (temp != null && !temp.isEmpty()) {
                mavenDeclaredDependencies = temp;
            }

            org.apache.maven.project.MavenProject rawMvnProject = nbMavenProject.getMavenProject();
            packaging = rawMvnProject.getPackaging();
            javaSourceLevel = rawMvnProject.getProperties().getProperty("maven.compiler.release");
            if (javaSourceLevel == null) {
                javaSourceLevel = rawMvnProject.getProperties().getProperty("maven.compiler.source");
            }
            javaTargetLevel = rawMvnProject.getProperties().getProperty("maven.compiler.target");
            sourceEncoding = rawMvnProject.getProperties().getProperty("project.build.sourceEncoding");
        }

        String compileOnSave = "Unknown";
        try {
            compileOnSave = isCompileOnSaveEnabled(target);
        } catch (Exception e) {
            log.debug("Failed to read compile.on.save status for project: " + projectPath, e);
        }

        String htmlDisplayName = null;
        try {
            org.openide.nodes.Node node = org.openide.loaders.DataObject.find(root).getNodeDelegate();
            htmlDisplayName = node.getHtmlDisplayName();
        } catch (Exception e) {
            log.debug("Failed to get HTML display name for project root", e);
        }

        return new ProjectOverview(
                root.getNameExt(),
                info.getDisplayName(),
                htmlDisplayName,
                getCanonicalPath(root),
                packaging,
                actions,
                mavenDeclaredDependencies,
                javaSourceLevel,
                javaTargetLevel,
                sourceEncoding,
                compileOnSave
        );
    }

    /**
     * Checks the effective 'Compile on Save' status for a project using tiered logic.
     * 
     * @param project The project to check.
     * @return A descriptive string of the status (e.g., "all (IDE Override)", "none (Maven Property)").
     */
    public String isCompileOnSaveEnabled(Project project) {
        // 1. Priority 1: Auxiliary Configuration (nb-configuration.xml)
        AuxiliaryConfiguration aux = project.getLookup().lookup(AuxiliaryConfiguration.class);
        if (aux != null) {
            org.w3c.dom.Element el = aux.getConfigurationFragment("properties", "http://www.netbeans.org/ns/maven-properties-data/1", true);
            if (el != null) {
                org.w3c.dom.NodeList nodeList = el.getElementsByTagName("netbeans.compile.on.save");
                if (nodeList.getLength() > 0) {
                    return nodeList.item(0).getTextContent().trim() + " (IDE Override)";
                }
            }
        }

        // 2. Priority 2: Maven Properties (POM)
        NbMavenProject nbMvn = project.getLookup().lookup(NbMavenProject.class);
        if (nbMvn != null) {
            String mvnProp = nbMvn.getMavenProject().getProperties().getProperty("netbeans.compile.on.save");
            if (mvnProp != null) {
                return mvnProp.trim() + " (Maven Property)";
            }
            return "all (Maven Default)";
        }

        return "Enabled"; // Fallback for non-Maven projects
    }

    /**
     * Sets the 'Compile on Save' override in the project's nb-configuration.xml.
     * This override takes precedence over any properties defined in the pom.xml.
     * 
     * @param projectPath The absolute path of the project.
     * @param enabled Whether to enable (all) or disable (none).
     * @throws Exception on failure.
     */
    @AiTool("Sets the 'Compile on Save' override in nb-configuration.xml. This 'shared' configuration is what the IDE's Properties dialog manages and it overrides values in the pom.xml.")
    public void setCompileOnSaveOverride(
            @AiToolParam("The absolute path of the project.") String projectPath,
            @AiToolParam("Whether to enable ('all') or disable ('none') Compile on Save.") boolean enabled) throws Exception {
        Project project = findOpenProject(projectPath);
        String value = enabled ? "all" : "none";

        // 1. Write the override to nb-configuration.xml using AuxiliaryConfiguration
        AuxiliaryConfiguration aux = project.getLookup().lookup(AuxiliaryConfiguration.class);
        if (aux != null) {
            String ns = "http://www.netbeans.org/ns/maven-properties-data/1";
            org.w3c.dom.Element props = aux.getConfigurationFragment("properties", ns, true);
            
            if (props == null) {
                props = org.openide.xml.XMLUtil.createDocument("properties", ns, null, null).getDocumentElement();
            }

            org.w3c.dom.NodeList nl = props.getElementsByTagName("netbeans.compile.on.save");
            org.w3c.dom.Element cosElem;
            if (nl.getLength() > 0) {
                cosElem = (org.w3c.dom.Element) nl.item(0);
            } else {
                cosElem = props.getOwnerDocument().createElementNS(ns, "netbeans.compile.on.save");
                props.appendChild(cosElem);
            }
            cosElem.setTextContent(value);
            aux.putConfigurationFragment(props, true);
        }

        log.info("Compile on Save override for {} set to: {} (in nb-configuration.xml)", projectPath, value);
    }

    /**
     * Gets the file and folder structure of a project.
     * 
     * @param projectPath The path to the project.
     * @return File structure DTO.
     * @throws Exception if project not found.
     */
    /*
    @AiTool("Gets the file and folder structure of a project, including root files and a detailed source tree.\n"
            + "WARNING: DO NOT call this tool if the 'Project Files' context provider for this project is already enabled ('providing').")
    */
    public ProjectFiles getProjectFiles(@AiToolParam("The absolute path of the project.") String projectPath) throws Exception {
        Project target = findOpenProject(projectPath);
        FileObject root = target.getProjectDirectory();

        List<ProjectFile> rootFiles = new ArrayList<>();
        List<String> rootFolderNames = new ArrayList<>();
        List<SourceFolder> sourceFolders = new ArrayList<>();

        for (FileObject child : root.getChildren()) {
            if (child.isFolder()) {
                rootFolderNames.add(child.getNameExt());
            } else {
                rootFiles.add(createProjectFile(child));
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

        return new ProjectFiles(rootFiles, rootFolderNames, sourceFolders);
    }

    /**
     * Retrieves all Java types defined in the project.
     * 
     * @param projectPath The path to the project.
     * @return List of components.
     * @throws Exception if project not found.
     */
    /*
    @AiTool("Retrieves all Java types (classes, interfaces, enums, records) defined in the project.\n"
            + "WARNING: DO NOT call this tool if the 'Project Components' context provider for this project is already enabled ('providing').")
    */
    public List<ProjectComponent> getProjectComponents(@AiToolParam("The absolute path to the project.") String projectPath) throws Exception {
        Project project = findOpenProject(projectPath);
        List<ProjectComponent> components = new ArrayList<>();

        Sources sources = ProjectUtils.getSources(project);
        SourceGroup[] groups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);

        for (SourceGroup sg : groups) {
            ClasspathInfo cpInfo = ClasspathInfo.create(sg.getRootFolder());
            ClassIndex index = cpInfo.getClassIndex();
            Set<ElementHandle<javax.lang.model.element.TypeElement>> allTypes = index.getDeclaredTypes("", ClassIndex.NameKind.PREFIX, EnumSet.of(ClassIndex.SearchScope.SOURCE));
            for (ElementHandle<javax.lang.model.element.TypeElement> handle : allTypes) {
                components.add(ProjectComponent.builder()
                        .fqn(handle.getQualifiedName())
                        .kind(handle.getKind())
                        .build());
            }
        }
        components.sort((c1, c2) -> c1.getFqn().compareToIgnoreCase(c2.getFqn()));
        return components;
    }

    /**
     * Performs a live scan of a specific project for alerts.
     * 
     * @param projectPath The path to the project.
     * @return Diagnostics DTO.
     * @throws Exception if project not found.
     */
    /*
    @AiTool("Performs a live scan of a specific project to find all high-level project problems and Java source file errors/warnings.\n"
            + "WARNING: DO NOT call this tool if the 'Project Alerts' context provider for this project is already enabled ('providing').")
    */
    public ProjectDiagnostics getProjectAlerts(@AiToolParam("The absolute path of the project to scan.") String projectPath) throws Exception {
        Project targetProject = findOpenProject(projectPath);
        ProjectDiagnostics projectDiags = new ProjectDiagnostics(ProjectUtils.getInformation(targetProject).getDisplayName());

        List<FileObject> filesInError = findFilesInError(targetProject);

        if (!filesInError.isEmpty()) {
            List<CompletableFuture<Void>> futures = new ArrayList<>();
            for (FileObject fo : filesInError) {
                CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                    try {
                        JavaSource javaSource = JavaSource.forFileObject(fo);
                        if (javaSource != null) {
                            javaSource.runUserActionTask(controller -> {
                                controller.toPhase(JavaSource.Phase.RESOLVED);
                                List<? extends Diagnostic> diagnostics = controller.getDiagnostics();
                                for (Diagnostic d : diagnostics) {
                                    projectDiags.addJavacAlert(new JavacAlert(
                                            fo.getPath(),
                                            d.getKind().toString(),
                                            (int) d.getLineNumber(),
                                            (int) d.getColumnNumber(),
                                            d.getMessage(null)
                                    ));
                                }
                            }, true);
                        }
                    } catch (IOException e) {
                        projectDiags.addJavacAlert(new JavacAlert(fo.getPath(), "ERROR", -1, -1, "Error processing file: " + e.getMessage()));
                    }
                });
                futures.add(future);
            }
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        }

        ProjectProblemsProvider problemProvider = targetProject.getLookup().lookup(ProjectProblemsProvider.class);
        if (problemProvider != null) {
            Collection<? extends ProjectProblemsProvider.ProjectProblem> problems = problemProvider.getProblems();
            for (ProjectProblemsProvider.ProjectProblem problem : problems) {
                projectDiags.addProjectAlert(new ProjectAlert(
                        problem.getDisplayName(),
                        problem.getDescription(),
                        "PROJECT",
                        problem.getSeverity().toString(),
                        problem.isResolvable()
                ));
            }
        }
        return projectDiags;
    }

    /**
     * Identifies files within a project that currently have compilation errors.
     *
     * @param project The project to scan.
     * @return A list of FileObjects representing the files in error.
     */
    public static List<FileObject> findFilesInError(Project project) {
        List<FileObject> results = new ArrayList<>();
        Sources sources = ProjectUtils.getSources(project);
        SourceGroup[] groups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);
        for (SourceGroup sg : groups) {
            FileObject root = sg.getRootFolder();
            URL rootUrl = root.toURL();
            try {
                Collection<? extends URL> files = ErrorsCache.getAllFilesInError(rootUrl);
                if (files != null) {
                    for (URL url : files) {
                        FileObject fo = URLMapper.findFileObject(url);
                        if (fo != null && !fo.isFolder() && "text/x-java".equals(fo.getMIMEType())) {
                            results.add(fo);
                        }
                    }
                }
            } catch (Exception e) {
                log.warn("Error querying ErrorsCache for root: " + root.getPath(), e);
            }
        }
        return results;
    }

    /**
     * Enables or disables the project context provider.
     * 
     * @param projectPath Path to the project.
     * @param enabled Status.
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
                FilesContextActionLogic.fireRefreshRecursive(p.getProjectDirectory());
            } catch (Exception ex) {
                // Ignore
            }
        });
    }

    /**
     * Synchronously executes a NetBeans Project action.
     * 
     * @param projectPath Project path.
     * @param action Action name.
     * @throws Exception if project or action not found.
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
            throw new IllegalArgumentException("The '" + action + "' action is not supported or enabled for project '" + project + "'.");
        }
    }

    /**
     * Lists all known preferences for a given project.
     * 
     * @param projectPath The path to the project.
     * @return String representation of preferences.
     * @throws Exception if project not found.
     */
    //@AiTool("Lists all known preferences for a given project.")
    /*
    public String listAllKnownPreferences(@AiToolParam("The absolute path of the project.") String projectPath) throws Exception {
        Project project = findOpenProject(projectPath);
        if (project == null) return "Project not found: " + projectPath;
        StringBuilder sb = new StringBuilder();
        Class<?>[] contextClasses = new Class<?>[]{ ProjectUtils.class };
        for (Class<?> ctx : contextClasses) {
            sb.append("Preferences for context: ").append(ctx.getName()).append("\n");
            Preferences prefs = ProjectUtils.getPreferences(project, ctx, true);
            try {
                String[] keys = prefs.keys();
                if (keys.length == 0) sb.append("  (no preferences)\n");
                else {
                    for (String key : keys) sb.append("  ").append(key).append(" = ").append(prefs.get(key, "<no value>")).append("\n");
                }
            } catch (BackingStoreException e) {
                sb.append("  Failed to read preferences: ").append(e.getMessage()).append("\n");
            }
            sb.append("\n");
        }
        return sb.toString();
    }
    */

    /**
     * Returns the child context providers (one per open project).
     * 
     * @return List of project providers.
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
     * Synchronizes project providers using Canonical Paths.
     */
    private synchronized void syncProjects() {
        Project[] openProjects = OpenProjects.getDefault().getOpenProjects();
        List<String> currentPaths = new ArrayList<>();
        for (Project p : openProjects) {
            String path = getCanonicalPath(p.getProjectDirectory());
            currentPaths.add(path);
            if (getProjectProvider(path).isEmpty()) {
                ProjectContextProvider pcp = new ProjectContextProvider(this, p);
                childrenProviders.add(pcp);
                log.info("Added ProjectContextProvider for: {}", pcp.getName());
            }
        }
        childrenProviders.removeIf(cp -> {
            if (cp instanceof ProjectContextProvider pcp) {
                if (!currentPaths.contains(pcp.getProjectPath())) {
                    log.info("Removing ProjectContextProvider for closed project at: {}", pcp.getProjectPath());
                    pcp.getFlattenedHierarchy(false).forEach(child -> child.setProviding(false));
                    return true;
                }
            }
            return false;
        });
    }

    /**
     * Finds a provider by its canonical project path.
     * 
     * @param projectPath The canonical path.
     * @return Optional provider.
     */
    public Optional<ProjectContextProvider> getProjectProvider(String projectPath) {
        return childrenProviders.stream()
                .filter(cp -> cp instanceof ProjectContextProvider)
                .map(cp -> (ProjectContextProvider) cp)
                .filter(pcp -> pcp.getProjectPath().equals(projectPath))
                .findFirst();
    }

    /**
     * Gets the canonical path for a FileObject.
     * 
     * @param fo The file object.
     * @return The canonical path string.
     */
    public static String getCanonicalPath(FileObject fo) {
        if (fo == null) return null;
        File f = FileUtil.toFile(fo);
        if (f != null) {
            try { return f.getCanonicalPath(); } catch (IOException e) {}
        }
        return fo.getPath();
    }

    /**
     * Listens for global project changes.
     * 
     * @param evt PropertyChangeEvent.
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if (OpenProjects.PROPERTY_OPEN_PROJECTS.equals(evt.getPropertyName())) {
            syncProjects();
        }
    }

    private SourceFolder buildSourceFolderTree(FileObject folder, String displayName) throws FileStateInvalidException {
        if (!folder.isFolder()) throw new IllegalArgumentException("FileObject must be a folder: " + folder.getPath());
        List<ProjectFile> files = new ArrayList<>();
        List<SourceFolder> subfolders = new ArrayList<>();
        for (FileObject child : folder.getChildren()) {
            if (child.isFolder()) subfolders.add(buildSourceFolderTree(child, child.getNameExt()));
            else files.add(createProjectFile(child));
        }
        long recursiveSize = files.stream().mapToLong(ProjectFile::getSize).sum() + subfolders.stream().mapToLong(SourceFolder::getRecursiveSize).sum();
        String folderName = folder.getNameExt();
        String finalDisplayName = folderName.equals(displayName) ? null : displayName;
        return new SourceFolder(finalDisplayName, folder.getPath(), recursiveSize, files.isEmpty() ? null : files, subfolders.isEmpty() ? null : subfolders);
    }

    private ProjectFile createProjectFile(FileObject fo) throws FileStateInvalidException {
        String annotatedName = null;
        try {
            org.openide.nodes.Node node = org.openide.loaders.DataObject.find(fo).getNodeDelegate();
            String html = node.getHtmlDisplayName();
            if (html != null) annotatedName = html.replaceAll("<[^>]*>", "").trim();
        } catch (Exception e) {}
        return new ProjectFile(fo.getNameExt(), annotatedName, fo.getSize(), fo.lastModified().getTime(), fo.getPath());
    }

    private String getNetBeansProjectsFolder() {
        File f = org.netbeans.spi.project.ui.support.ProjectChooser.getProjectsFolder();
        return f != null ? f.getAbsolutePath() : System.getProperty("user.home") + File.separator + "NetBeansProjects";
    }
}
