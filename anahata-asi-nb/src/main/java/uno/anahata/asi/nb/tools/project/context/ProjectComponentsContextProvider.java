/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.nb.tools.project.ProjectComponent;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

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
     * Injects the list of project Java components into the RAG message.
     * <p>
     * Implementation details:
     * Queries the Projects toolkit for all declared Java types in the project 
     * and appends them as a Markdown list. Truncates the list if it exceeds 
     * MAX_COMPONENTS for performance.
     * </p>
     * 
     * @param ragMessage The target RAG message.
     * @throws Exception if project components cannot be fetched.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        List<ProjectComponent> components = projectsToolkit.getProjectComponents(projectPath);
        ragMessage.addTextPart(generateMarkdown(components));
    }

    /**
     * Toggles providing status and triggers a UI refresh.
     * <p>
     * Implementation details:
     * Notifies the IDE that the project icon should be redrawn to reflect 
     * the new context state.
     * </p>
     * 
     * @param enabled New state.
     */
    @Override
    public void setProviding(boolean enabled) {
        boolean old = isProviding();
        super.setProviding(enabled);
        if (old != enabled && parent instanceof ProjectContextProvider pcp) {
            FilesContextActionLogic.fireRefreshRecursive(pcp.getProject().getProjectDirectory());
        }
    }

    /**
     * Generates a Markdown string representing the project components.
     * <p>
     * Implementation details:
     * Iterates through the component list and formats each as an FQN followed 
     * by its Java kind (Class, Interface, etc.).
     * </p>
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
