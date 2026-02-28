/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import lombok.extern.slf4j.Slf4j;
import org.netbeans.api.project.Project;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.project.components.ProjectStructure2;

/**
 * Provides a unified, architecturally-aware view of a project's structure.
 * This provider leverages the ProjectStructure2 domain model to inject a 
 * hierarchical map of logical Java components and physical resources into 
 * the RAG message.
 * 
 * @author anahata
 */
@Slf4j
public class ProjectStructureContextProvider extends AbstractProjectContextProvider {

    /** 
     * Flag to control the level of detail in the rendered Markdown.
     * If true, renders an aggregate view of packages and folders.
     */
    private boolean summaryMode = false;

    /**
     * Constructs a new structure provider for a specific project.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project directory.
     */
    public ProjectStructureContextProvider(Projects projectsToolkit, String projectPath) {
        super("structure", "Project Structure", "Unified logical and physical project map", projectsToolkit, projectPath);
    }

    /**
     * Injected the project's structure map into the RAG message.
     * <p>
     * Implementation details:
     * 1. Resolves the NetBeans Project instance via the base class helper.
     * 2. Constructs a ProjectStructure2 domain object (performing a recursive scan).
     * 3. Triggers the domain object's self-rendering logic.
     * </p>
     * 
     * @param ragMessage The target RAG message.
     * @throws Exception if project resolution or scanning fails.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        Project project = getProject();
        if (project == null) {
            return;
        }
        ProjectStructure2 structure = new ProjectStructure2(project);
        
        StringBuilder sb = new StringBuilder();
        structure.renderMarkdown(sb, "  ", summaryMode);
        ragMessage.addTextPart(sb.toString());
    }
}
