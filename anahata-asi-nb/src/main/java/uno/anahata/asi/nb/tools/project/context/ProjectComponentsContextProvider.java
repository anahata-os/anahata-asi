/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.context;

import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.project.ProjectComponent;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * Provides a Java-centric view of a project by listing all its top-level types.
 * This provider injects a list of FQNs and their element kinds into the RAG message,
 * enabling the model to use FQN-based shortcuts for code exploration.
 * 
 * @author anahata
 */
@Slf4j
public class ProjectComponentsContextProvider extends BasicContextProvider {

    /** The maximum number of components to list in the RAG message. */
    private static final int MAX_COMPONENTS = 500;

    /** The parent Projects toolkit. */
    private final Projects projectsToolkit;
    
    /** The absolute path to the project directory. */
    private final String projectPath;

    /**
     * Constructs a new components provider for a specific project.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public ProjectComponentsContextProvider(Projects projectsToolkit, String projectPath) {
        super("components", "Project Components", "List of all Java types (FQNs) in the project");
        this.projectsToolkit = projectsToolkit;
        this.projectPath = projectPath;
        super.setProviding(false);
    }

    /**
     * {@inheritDoc}
     * Populates the RAG message with a Markdown list of project components.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        List<ProjectComponent> components = projectsToolkit.getProjectComponents(projectPath);
        ragMessage.addTextPart(generateMarkdown(components));
    }

    /**
     * Generates a Markdown string representing the project components.
     * 
     * @param components The list of project components.
     * @return A Markdown-formatted string.
     */
    private String generateMarkdown(List<ProjectComponent> components) {
        if (components.isEmpty()) {
            return "\n  ## Java Components\n    - No Java types found in this project.\n";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("\n  ## Java Components (FQNs)\n");
        
        int count = 0;
        for (ProjectComponent component : components) {
            if (count >= MAX_COMPONENTS) {
                sb.append("    - ... and ").append(components.size() - MAX_COMPONENTS).append(" more components (truncated for performance).\n");
                break;
            }
            sb.append("    - `").append(component.getFqn()).append("` (").append(component.getKind()).append(")\n");
            count++;
        }

        return sb.toString();
    }
}
