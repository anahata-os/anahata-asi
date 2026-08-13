/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.maven;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.vfs.VirtualFile;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.idea.maven.execution.MavenRunner;
import org.jetbrains.idea.maven.execution.MavenRunnerParameters;
import org.jetbrains.idea.maven.indices.MavenArtifactSearchResult;
import org.jetbrains.idea.maven.indices.MavenArtifactSearcher;
import org.jetbrains.idea.maven.model.MavenArtifact;
import org.jetbrains.idea.maven.onlinecompletion.model.MavenRepositoryArtifactInfo;
import org.jetbrains.idea.maven.project.MavenProject;
import org.jetbrains.idea.maven.project.MavenProjectsManager;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.intellij.internal.JavaPsi;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * A toolkit for inspecting and building Maven projects through the IntelliJ IDEA Maven
 * integration.
 * <p>
 * This is the IntelliJ port of the NetBeans {@code Maven} toolkit. It uses
 * {@link MavenProjectsManager} to enumerate imported Maven projects and their resolved
 * dependencies, and {@link MavenRunner} to execute goals against a project's live
 * configuration. Goal execution is asynchronous in the platform; this toolkit awaits
 * completion on a latch so the model receives a definitive result. Build output streams to
 * the IDE's Maven Run console (the platform does not expose it as a return value here).
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for inspecting and building Maven projects in IntelliJ IDEA.")
public class Maven extends AnahataToolkit {

    /**
     * Constructs the Maven toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Maven() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Notes that projects must be imported as Maven projects and that goal output appears in
     * the IDE Maven console.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
                "The Maven toolkit inspects and builds Maven projects that are imported in IntelliJ. "
                + "Use getMavenProjects to discover them, getDependencies to inspect a project's resolved classpath, "
                + "and runGoals to execute Maven goals (output streams to the IDE Maven Run console).");
    }

    /**
     * Lists every imported Maven project across all open IntelliJ projects.
     *
     * @return a Markdown listing of Maven coordinates, packaging and directory.
     */
    @AgiTool("Lists all imported Maven projects (coordinates, packaging, directory) across open IntelliJ projects.")
    public String getMavenProjects() {
        StringBuilder sb = new StringBuilder("## Imported Maven Projects\n");
        boolean any = false;
        for (Project ideProject : ProjectManager.getInstance().getOpenProjects()) {
            for (MavenProject mp : MavenProjectsManager.getInstance(ideProject).getProjects()) {
                any = true;
                sb.append("- **").append(mp.getMavenId()).append("** [").append(mp.getPackaging()).append("] `")
                  .append(mp.getDirectory()).append("`\n");
            }
        }
        return any ? sb.toString() : "No imported Maven projects found in any open IntelliJ project.";
    }

    /**
     * Lists the resolved dependencies of the Maven project at the given path, grouped by scope.
     *
     * @param projectPath the absolute path of the project directory or its {@code pom.xml}.
     * @return a Markdown listing of resolved dependencies.
     * @throws AgiToolException if the path is not a recognized (imported) Maven project.
     */
    @AgiTool("Lists the resolved dependencies (grouped by scope) of the Maven project at the given path.")
    public String getDependencies(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath) throws AgiToolException {

        MavenProject mp = resolveMavenProject(projectPath);
        List<MavenArtifact> dependencies = mp.getDependencies();
        if (dependencies.isEmpty()) {
            return "No resolved dependencies for " + mp.getMavenId() + ".";
        }
        StringBuilder sb = new StringBuilder("## Resolved Dependencies: ").append(mp.getMavenId()).append("\n");
        for (MavenArtifact artifact : dependencies) {
            sb.append("- `").append(artifact.getGroupId()).append(":").append(artifact.getArtifactId())
              .append(":").append(artifact.getVersion()).append("` [").append(artifact.getScope()).append("]")
              .append(artifact.isResolved() ? "" : " (UNRESOLVED)").append("\n");
        }
        return sb.toString();
    }

    /**
     * Executes Maven goals against the project at the given path and awaits completion.
     * <p>
     * The goals run through the platform {@link MavenRunner}; this call blocks (off the EDT)
     * until the build finishes or a generous timeout elapses. Console output is shown in the
     * IDE's Maven Run tool window.
     * </p>
     *
     * @param projectPath the absolute path of the project directory or its {@code pom.xml}.
     * @param goals       the Maven goals to run (e.g. {@code clean}, {@code install}).
     * @param profiles    the profiles to activate, or {@code null}/empty for none.
     * @return a completion summary.
     * @throws AgiToolException if the project cannot be resolved or the build is interrupted.
     */
    @AgiTool("Executes Maven goals against a project and waits for completion (output shows in the IDE Maven console).")
    public String runGoals(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath,
            @AgiToolParam("The Maven goals to run, e.g. ['clean','install'].") List<String> goals,
            @AgiToolParam(value = "Profiles to activate, or empty for none.", required = false) List<String> profiles) throws AgiToolException {

        Object[] context = resolveMavenContext(projectPath);
        Project ideProject = (Project) context[0];
        MavenProject mp = (MavenProject) context[1];
        String workingDir = mp.getDirectory();

        MavenRunnerParameters params = new MavenRunnerParameters(
                true, workingDir, "pom.xml", goals,
                profiles != null ? profiles : Collections.emptyList());

        CountDownLatch latch = new CountDownLatch(1);
        ApplicationManager.getApplication().invokeLater(() -> {
            MavenRunner runner = MavenRunner.getInstance(ideProject);
            runner.run(params, runner.getSettings(), latch::countDown);
        });

        try {
            boolean finished = latch.await(15, TimeUnit.MINUTES);
            if (!finished) {
                return "Maven goals " + goals + " are still running after 15 minutes for " + projectPath
                        + " (see the Maven Run console).";
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AgiToolException("Interrupted while awaiting Maven goals for: " + projectPath);
        }
        log("Maven goals " + goals + " completed for " + projectPath);
        return "Maven goals " + goals + " completed for " + mp.getMavenId() + " (output in the IDE Maven Run console).";
    }

    /**
     * Searches the configured Maven repository index for artifacts matching a query.
     * <p>
     * Uses {@link MavenArtifactSearcher} against the index of the first open project. Results
     * depend on the repository index having been built/downloaded by the IDE; an empty result
     * usually means the index is not yet available.
     * </p>
     *
     * @param query      the search query (matched against groupId/artifactId).
     * @param maxResults the maximum number of results, or {@code null} for 50.
     * @return a Markdown listing of matching {@code groupId:artifactId:version} coordinates.
     * @throws AgiToolException if no project is open.
     */
    @AgiTool("Searches the Maven repository index for artifacts matching a query (requires the IDE's repository index).")
    public String searchMavenIndex(
            @AgiToolParam("The search query, matched against groupId/artifactId.") String query,
            @AgiToolParam(value = "The maximum number of results (default 50).", required = false) Integer maxResults) throws AgiToolException {

        Project[] open = ProjectManager.getInstance().getOpenProjects();
        if (open.length == 0) {
            throw new AgiToolException("No open project to search the Maven index against.");
        }
        Project ideProject = open[0];
        int max = maxResults != null ? maxResults : 50;

        List<MavenArtifactSearchResult> results = ReadAction.compute(() ->
                new MavenArtifactSearcher().search(ideProject, query, max));
        if (results.isEmpty()) {
            return "No index results for '" + query + "' (the repository index may not be built yet).";
        }

        StringBuilder sb = new StringBuilder("## Maven Index Results: '").append(query).append("'\n");
        for (MavenArtifactSearchResult result : results) {
            MavenRepositoryArtifactInfo info = result.getSearchResults();
            if (info != null) {
                sb.append("- `").append(info.getGroupId()).append(":").append(info.getArtifactId());
                if (info.getVersion() != null) {
                    sb.append(":").append(info.getVersion());
                }
                sb.append("`\n");
            }
        }
        return sb.toString();
    }

    /**
     * Adds a dependency to a project's {@code pom.xml} and triggers a Maven reimport.
     * <p>
     * The dependency element is spliced into the existing {@code <dependencies>} block (or a
     * new block is created before {@code </project>}) via a single undoable document edit,
     * the file is saved, and {@link MavenProjectsManager#forceUpdateAllProjectsOrFindAllAvailablePomFiles()}
     * refreshes the project model so the new artifact is resolved onto the classpath.
     * </p>
     *
     * @param projectPath the absolute path of the project directory or its {@code pom.xml}.
     * @param groupId     the dependency groupId.
     * @param artifactId  the dependency artifactId.
     * @param version     the dependency version.
     * @param scope       the Maven scope (e.g. {@code compile}, {@code test}), or {@code null} for default.
     * @return a confirmation message.
     * @throws AgiToolException if the pom cannot be resolved or edited.
     */
    @AgiTool("Adds a dependency to a project's pom.xml and triggers a Maven reimport.")
    public String addDependency(
            @AgiToolParam("The absolute path of the Maven project directory or its pom.xml.") String projectPath,
            @AgiToolParam("The dependency groupId.") String groupId,
            @AgiToolParam("The dependency artifactId.") String artifactId,
            @AgiToolParam("The dependency version.") String version,
            @AgiToolParam(value = "The Maven scope (compile/test/provided/runtime), or null for default.", required = false) String scope) throws AgiToolException {

        Object[] context = resolveMavenContext(projectPath);
        Project ideProject = (Project) context[0];
        MavenProject mp = (MavenProject) context[1];
        VirtualFile pomVf = mp.getFile();

        StringBuilder dep = new StringBuilder();
        dep.append("        <dependency>\n");
        dep.append("            <groupId>").append(groupId).append("</groupId>\n");
        dep.append("            <artifactId>").append(artifactId).append("</artifactId>\n");
        dep.append("            <version>").append(version).append("</version>\n");
        if (scope != null && !scope.isBlank()) {
            dep.append("            <scope>").append(scope).append("</scope>\n");
        }
        dep.append("        </dependency>\n");

        ApplicationManager.getApplication().invokeAndWait(() ->
                WriteCommandAction.runWriteCommandAction(ideProject, () -> {
                    Document document = FileDocumentManager.getInstance().getDocument(pomVf);
                    if (document == null) {
                        return;
                    }
                    String text = document.getText();
                    int closeDeps = text.lastIndexOf("</dependencies>");
                    if (closeDeps >= 0) {
                        document.insertString(closeDeps, dep.toString());
                    } else {
                        int closeProject = text.lastIndexOf("</project>");
                        String block = "    <dependencies>\n" + dep + "    </dependencies>\n";
                        document.insertString(closeProject >= 0 ? closeProject : text.length(), block);
                    }
                    FileDocumentManager.getInstance().saveDocument(document);
                }));

        MavenProjectsManager.getInstance(ideProject).forceUpdateAllProjectsOrFindAllAvailablePomFiles();
        log("Added dependency " + groupId + ":" + artifactId + ":" + version + " and triggered reimport.");
        return "Added " + groupId + ":" + artifactId + ":" + version + " to " + mp.getMavenId() + " and triggered a Maven reimport.";
    }

    /**
     * Resolves the {@link MavenProject} for a directory or pom path.
     *
     * @param projectPath the absolute project directory or pom path.
     * @return the resolved Maven project.
     * @throws AgiToolException if it is not a recognized imported Maven project.
     */
    private MavenProject resolveMavenProject(String projectPath) throws AgiToolException {
        return (MavenProject) resolveMavenContext(projectPath)[1];
    }

    /**
     * Resolves {@code [IntelliJ Project, MavenProject]} for a directory or pom path.
     *
     * @param projectPath the absolute project directory or pom path.
     * @return a two-element array of the host IntelliJ project and the Maven project.
     * @throws AgiToolException if the pom cannot be found or is not an imported Maven project.
     */
    private Object[] resolveMavenContext(String projectPath) throws AgiToolException {
        Path path = Path.of(projectPath);
        Path pom = Files.isDirectory(path) ? path.resolve("pom.xml") : path;
        VirtualFile pomVf = JavaPsi.findVirtualFile(pom.toString());
        if (pomVf == null) {
            throw new AgiToolException("pom.xml not found for: " + projectPath);
        }
        Project ideProject = JavaPsi.findHostProject(pomVf);
        if (ideProject == null) {
            throw new AgiToolException("No open IntelliJ project hosts: " + projectPath);
        }
        MavenProject mp = MavenProjectsManager.getInstance(ideProject).findProject(pomVf);
        if (mp == null) {
            throw new AgiToolException("Not a recognized/imported Maven project: " + projectPath);
        }
        return new Object[]{ideProject, mp};
    }
}
