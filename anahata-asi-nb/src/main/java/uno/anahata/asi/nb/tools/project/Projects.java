/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.nb.tools.project;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
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
import uno.anahata.asi.agi.Agi;
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
import uno.anahata.asi.nb.tools.project.context.ProjectStructureContextProvider;

/**
 * A toolkit for interacting with the NetBeans Project APIs.
 * <p>
 * This toolkit acts as a global ContextProvider that manages a hierarchy of 
 * ProjectContextProviders, one for each open project in the IDE. It uses 
 * Canonical Paths for all registrations and lookups to ensure consistency 
 * across physical and virtual (MasterFS) FileObject proxies.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for using netbeans project apis.")
public class Projects extends AnahataToolkit implements PropertyChangeListener {

    /** Flag indicating if the toolkit is currently listening for global IDE project changes. */
    private transient boolean listening = false;
    
    /**
     * Rebinds the toolkit to the current agi instance.
     * <p>
     * Overrides the base rebind logic to trigger a lazy initialization of all 
     * project child providers and fires a UI refresh for all open projects. 
     * This ensures that IDE annotations and context status are correctly 
     * reflected immediately upon session start or restoration.
     * </p>
     */
    @Override
    public void rebind() {
        super.rebind();
        // Force initialization of children to ensure UI annotations reflect context immediately
        //getChildrenProviders();
        
        // Trigger UI refresh for all open projects to show context badges immediately
        for (String path : getOpenProjects()) {
            try {
                Project p = findOpenProject(path);
                FilesContextActionLogic.fireRefreshRecursive(p.getProjectDirectory());
            } catch (Exception e) {
                log.debug("Failed to resolve project during rebind: {}", path);
            }
        }
    }

    /**
     * Returns the mandatory system instructions for managing Compile on Save.
     * <p>
     * Provides a Markdown-formatted guide on how to handle project property 
     * overrides, explaining the priority of IDE configuration over POM properties.
     * </p>
     * 
     * @return A list containing the CoS management instructions.
     * @throws Exception on internal error.
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
            "Fqn / path resulutions: use the **" + ProjectStructureContextProvider.class.getName()+ "** of each project to fetch project sources by fqn in CodeModel or to work our th path to source files With NbFiles.loadTextFile:\n" +
            "\n\nCompile on Save (CoS) Management:\n" +
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
     * Populates the RAG message with a high-level overview of the IDE environment.
     * <p>
     * Injects a Markdown summary including the projects folder, available folders, 
     * open projects, and the current main project. This allows the model to 
     * understand the scope of the user's workspace.
     * </p>
     * 
     * @param ragMessage The target RAG message.
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
     * Ensures that the anahata.md file exists in the project root.
     * <p>
     * If the file is missing, it creates a default template. The template 
     * logic is aware of Maven multi-module structures and injects appropriate 
     * architectural notes for parent and sub-module projects.
     * </p>
     * 
     * @param project The project to check.
     * @return The FileObject for anahata.md.
     * @throws IOException if creation fails.
     */
    public static FileObject ensureAnahataMdExists(Project project) throws IOException {
        FileObject root = project.getProjectDirectory();
        FileObject md = root.getFileObject("anahata.md");
        if (md != null) {
            return md;
        }

        ProjectInformation info = ProjectUtils.getInformation(project);
        md = root.createData("anahata.md");
        try (Writer writer = new OutputStreamWriter(md.getOutputStream())) {
            writer.write("# Project Instructions: " + info.getDisplayName() + "\n\n");
            writer.write("This file contains project-specific system instructions for the **" + info.getDisplayName() + "** project.\n");
            
            NbMavenProject mvn = project.getLookup().lookup(NbMavenProject.class);
            if (mvn != null) {
                org.apache.maven.project.MavenProject raw = mvn.getMavenProject();
                if (raw.getModules() != null && !raw.getModules().isEmpty()) {
                    writer.write("\n**Note**: This is a **Parent/Aggregator** project. Instructions here should focus on the overall architecture and shared standards for all modules.\n");
                } else if (raw.getParent() != null) {
                    writer.write("\n**Note**: This is a **Sub-module** of **" + raw.getParent().getArtifactId() + "**. These instructions are intended to extend the shared context provided by the parent project's `anahata.md`.\n");
                }
            }
        }
        return md;
    }

    /**
     * Lists all directories within the user's NetBeansProjects folder.
     * <p>
     * Scans the system's project folder and returns a sorted list of 
     * names for all subdirectories.
     * </p>
     * 
     * @return A list of directory names.
     */
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

    /**
     * Locates an open project instance by its directory path.
     * <p>
     * Performs a lookup across all currently open projects in the IDE using 
     * canonical path comparison to avoid issues with symlinks or virtual file proxies.
     * </p>
     * 
     * @param projectDirectoryPath The absolute path to the project.
     * @return The Project instance.
     * @throws Exception if the project is not found or is closed.
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
     * Returns a list of absolute paths for all projects currently open in the IDE.
     * <p>
     * Iterates through the IDE's open project list and captures the canonical 
     * path for each project's root directory.
     * </p>
     * 
     * @return A list of canonical project paths.
     */
    public List<String> getOpenProjects() {
        List<String> projectPaths = new ArrayList<>();
        for (Project project : OpenProjects.getDefault().getOpenProjects()) {
            projectPaths.add(getCanonicalPath(project.getProjectDirectory()));
        }
        return projectPaths;
    }

    /**
     * Returns the canonical path of the current NetBeans Main Project.
     * <p>
     * Queries the IDE for the main project and resolves its directory to 
     * an absolute canonical path.
     * </p>
     * 
     * @return The main project path, or null if none is set.
     */
    public String getMainProject() {
        Project p = OpenProjects.getDefault().getMainProject();
        return p != null ? getCanonicalPath(p.getProjectDirectory()) : null;
    }

    /**
     * Sets the specified open project as the IDE's 'Main Project'.
     * <p>
     * Resolves the project by its path and updates the NetBeans project 
     * management UI state.
     * </p>
     * 
     * @param projectPath The absolute path of the project to set as main.
     * @throws Exception if the project is not open.
     */
    @AiTool("Sets a specific open project as the 'Main Project'.")
    public void setMainProject(@AiToolParam("The absolute path of the project to set as main.") String projectPath) throws Exception {
        Project p = findOpenProject(projectPath);
        OpenProjects.getDefault().setMainProject(p);
    }

    /**
     * Closes the specified projects in the IDE.
     * <p>
     * Resolves multiple paths to project instances and performs a bulk 
     * close operation through the IDE controller.
     * </p>
     * 
     * @param projectPaths A list of absolute paths of the projects to close.
     * @throws Exception if any path does not resolve to an open project.
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
     * Opens a NetBeans project and optionally its subprojects.
     * <p>
     * Handles both absolute and relative paths. It uses a CountDownLatch and 
     * an IDE property listener to synchronize the asynchronous open operation 
     * with the tool execution, ensuring the project is fully loaded before 
     * returning.
     * </p>
     * 
     * @param projectPath The path to the project directory.
     * @param openSubprojects Whether to automatically open all subprojects.
     * @return A status message indicating success or timeout.
     * @throws Exception on internal error or directory failure.
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
     * Opens all sub-modules for a given parent project.
     * <p>
     * Queries the project's {@link SubprojectProvider} and initiates the 
     * opening of all discovered subprojects in a single batch.
     * </p>
     * 
     * @param projectPath The absolute path of the parent project.
     * @throws Exception if the parent project is not open.
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
     * Generates a structured overview of a project's metadata and environment.
     * <p>
     * Collects comprehensive data including packaging type, supported actions, 
     * Java versions, source encoding, and 'Compile on Save' status. It also 
     * fetches a list of declared Maven dependencies.
     * </p>
     * 
     * @param projectPath The absolute path of the project.
     * @return A {@link ProjectOverview} DTO.
     * @throws Exception if the project is not found or is closed.
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
     * Determines the effective 'Compile on Save' status for a project.
     * <p>
     * Follows the NetBeans priority hierarchy: 1. IDE Configuration overrides, 
     * 2. Maven properties in the POM, 3. Maven default settings.
     * </p>
     * 
     * @param project The project to check.
     * @return A status string indicating the value and its source.
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
     * Configures a permanent 'Compile on Save' override for a project.
     * <p>
     * Writes the netbeans.compile.on.save property to the project's 
     * nb-configuration.xml file. This ensures the setting persists across 
     * IDE restarts and takes precedence over POM properties.
     * </p>
     * 
     * @param projectPath The absolute path of the project.
     * @param enabled Whether to enable ('all') or disable ('none') Compile on Save.
     * @throws Exception on internal write error.
     */
    @AiTool("Sets the 'Compile on Save' override in nb-configuration.xml. This 'shared' configuration is what the IDE's Properties dialog manages and it overrides values in the pom.xml.")
    public void setCompileOnSaveOverride(
            @AiToolParam("The absolute path of the project.") String projectPath,
            @AiToolParam("Whether to enable ('all') or disable ('none') Compile on Save.") boolean enabled) throws Exception {
        Project project = findOpenProject(projectPath);
        String value = enabled ? "all" : "none";

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
     * Generates a structural overview of all files and source folders in a project.
     * <p>
     * Scans the project root for files and traverses all registered Java 
     * and Resource source groups to build a detailed DTO tree.
     * </p>
     * 
     * @param projectPath The absolute path of the project.
     * @return A {@link ProjectFiles} DTO.
     * @throws Exception if project not open.
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
     * Performs a comprehensive diagnostic scan of a project.
     * <p>
     * Aggregates compilation errors from the NetBeans ErrorsCache and high-level 
     * IDE project problems (like missing SDKs or broken dependencies) into a 
     * single diagnostic report.
     * </p>
     * 
     * @param projectPath The absolute path of the project to scan.
     * @return A {@link ProjectDiagnostics} report.
     * @throws Exception if project not open.
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
     * Locates all Java source files with compilation errors in the project.
     * <p>
     * Scans all Java source groups and uses the parsing cache to identify 
     * files currently flagged with errors.
     * </p>
     * 
     * @param project The target project.
     * @return A list of FileObjects in error.
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
     * Toggles the context provider state for a specific project.
     * <p>
     * Locates the appropriate provider by its canonical path and updates its 
     * activation state. Triggers a recursive visual refresh of project icons 
     * in the NetBeans UI.
     * </p>
     * 
     * @param projectPath The absolute path of the project.
     * @param enabled Whether to enable the context provider.
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
                log.debug("Failed to refresh project icons after toggle: {}", projectPath);
            }
        });
    }

    /**
     * Executes a project-level action.
     * <p>
     * Uses the project's action provider to trigger an asynchronous task. 
     * This is suitable for actions like 'run', 'clean', or 'build'.
     * </p>
     * 
     * @param projectPath The absolute path of the project.
     * @param action The action to invoke.
     * @throws Exception if the project is not open or the action is unsupported.
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
     * Returns the list of active project child providers.
     * <p>
     * Implements a lazy-initialization pattern. On the first call, it registers 
     * as a listener for global IDE project state changes and synchronizes 
     * the initial provider list.
     * </p>
     * 
     * @return A list of context providers.
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
     * Syncs project context providers with currently open IDE projects.
     * <p>
     * Uses canonical paths to detect newly opened projects and stale 
     * providers for closed projects. It maintains the hierarchical context 
     * structure by adding or pruning providers as needed.
     * </p>
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
     * Returns a project context provider by path.
     * <p>
     * Filters the active provider hierarchy for a project context provider 
     * that matches the specified canonical path.
     * </p>
     * 
     * @param projectPath The canonical path.
     * @return An Optional containing the provider.
     */
    public Optional<ProjectContextProvider> getProjectProvider(String projectPath) {
        return childrenProviders.stream()
                .filter(cp -> cp instanceof ProjectContextProvider)
                .map(cp -> (ProjectContextProvider) cp)
                .filter(pcp -> pcp.getProjectPath().equals(projectPath))
                .findFirst();
    }

    /**
     * Resolves a FileObject to its canonical physical path.
     * <p>
     * Attempts to normalize the file and resolve it via standard Java NIO 
     * canonical path resolution. Falls back to the standard path string if 
     * the resolution fails.
     * </p>
     * 
     * @param fo The FileObject.
     * @return The canonical path string.
     */
    public static String getCanonicalPath(FileObject fo) {
        if (fo == null) {
            return null;
        }
        File f = FileUtil.toFile(fo);
        if (f != null) {
            try { 
                return f.getCanonicalPath(); 
            } catch (IOException e) {
                log.debug("Canonical path resolution failed for: {}", f.getPath());
            }
        }
        return fo.getPath();
    }

    /**
     * Listens for IDE global project state changes.
     * <p>
     * Triggers a synchronization of the context provider hierarchy whenever 
     * projects are opened or closed in the IDE.
     * </p>
     * 
     * @param evt The property change event from the IDE.
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if (OpenProjects.PROPERTY_OPEN_PROJECTS.equals(evt.getPropertyName())) {
            syncProjects();
        }
    }

    /**
     * Recursively builds a structural tree for a source folder.
     * <p>
     * Traverses children, classifying subfolders as SourceFolders and 
     * files as ProjectFiles, while calculating the recursive directory size.
     * </p>
     * 
     * @param folder The target folder.
     * @param displayName The display name (from SourceGroup).
     * @return A {@link SourceFolder} DTO.
     * @throws FileStateInvalidException if the filesystem is invalid.
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
        long recursiveSize = files.stream().mapToLong(ProjectFile::getSize).sum() + subfolders.stream().mapToLong(SourceFolder::getRecursiveSize).sum();
        String folderName = folder.getNameExt();
        String finalDisplayName = folderName.equals(displayName) ? null : displayName;
        return new SourceFolder(finalDisplayName, folder.getPath(), recursiveSize, files.isEmpty() ? null : files, subfolders.isEmpty() ? null : subfolders);
    }

    /**
     * Creates a {@link ProjectFile} DTO with IDE-specific name annotations.
     * <p>
     * Captures file metadata and attempts to extract HTML display names 
     * from the IDE's node delegate, stripping HTML tags to provide a 
     * clean annotated name.
     * </p>
     * 
     * @param fo The target file.
     * @return A {@link ProjectFile} DTO.
     * @throws FileStateInvalidException if the filesystem is invalid.
     */
    private ProjectFile createProjectFile(FileObject fo) throws FileStateInvalidException {
        String annotatedName = null;
        try {
            org.openide.nodes.Node node = org.openide.loaders.DataObject.find(fo).getNodeDelegate();
            String html = node.getHtmlDisplayName();
            if (html != null) {
                annotatedName = html.replaceAll("<[^>]*>", "").trim();
            }
        } catch (Exception e) {
            log.debug("Failed to extract HTML display name for file: {}", fo.getPath());
        }
        return new ProjectFile(fo.getNameExt(), annotatedName, fo.getSize(), fo.lastModified().getTime(), fo.getPath());
    }

    /**
     * Returns the root NetBeansProjects directory.
     * <p>
     * Queries the NetBeans project chooser for the standardized projects 
     * folder location, falling back to a default folder in the user's home directory.
     * </p>
     * 
     * @return Absolute path to the projects folder.
     */
    private String getNetBeansProjectsFolder() {
        File f = org.netbeans.spi.project.ui.support.ProjectChooser.getProjectsFolder();
        return f != null ? f.getAbsolutePath() : System.getProperty("user.home") + File.separator + "NetBeansProjects";
    }
}
