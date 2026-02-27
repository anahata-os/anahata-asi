/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb.tools.project.context;

import java.io.File;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.nb.tools.files.nb.NbFiles;
import uno.anahata.asi.nb.tools.project.Projects;
import uno.anahata.asi.nb.tools.files.nb.FilesContextActionLogic;

/**
 * Provides project-specific system instructions from the anahata.md file.
 * It manages a stateful {@link TextFileResource} that is injected into the 
 * {@link ContextPosition#SYSTEM_INSTRUCTIONS} position.
 * <p>
 * Toggling this provider also triggers a refresh of the project's IDE annotations.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
public class AnahataMdContextProvider extends BasicContextProvider {

    /** The parent Projects toolkit. */
    private final Projects projectsToolkit;
    
    /** The absolute path to the project directory. */
    private final String projectPath;
    
    /** The unique ID of the registered resource, used for unregistration. */
    private String registeredResourceId;

    /**
     * Constructs a new anahata.md provider for a specific project.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param projectPath The absolute path to the project.
     */
    public AnahataMdContextProvider(Projects projectsToolkit, String projectPath) {
        super("anahata-md", "anahata.md", "Project-specific system instructions from anahata.md");
        this.projectsToolkit = projectsToolkit;
        this.projectPath = projectPath;
        syncResource();
    }

    /**
     * {@inheritDoc}
     * Overridden to trigger a resource sync and project UI refresh when toggled.
     */
    @Override
    public void setProviding(boolean enabled) {
        boolean old = isProviding();
        super.setProviding(enabled);
        syncResource();
        if (old != enabled && parent instanceof ProjectContextProvider pcp) {
             FilesContextActionLogic.fireRefreshRecursive(pcp.getProject().getProjectDirectory());
        }
    }

    /**
     * Synchronizes the anahata.md file with the agi's resource manager.
     * If the provider is enabled and the file exists, it is registered as a 
     * SYSTEM_INSTRUCTIONS resource. Otherwise, it is unregistered.
     */
    private void syncResource() {
        File file = new File(projectPath, "anahata.md");
        
        if (isProviding() && file.exists()) {
            if (registeredResourceId == null) {
                try {
                    // Use the toolkit to load the resource correctly (handling NbTextFileResource if needed)
                    NbFiles filesToolkit = projectsToolkit.getToolkit(NbFiles.class);
                    TextFileResource resource = filesToolkit.loadTextFileInternal(file.getAbsolutePath(), null);
                    
                    // Force the position to SYSTEM_INSTRUCTIONS
                    resource.setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
                    this.registeredResourceId = resource.getId();
                    log.info("Registered anahata.md as SYSTEM_INSTRUCTIONS resource for project: {}", projectPath);
                } catch (Exception e) {
                    log.error("Failed to register anahata.md as resource", e);
                }
            }
        } else {
            if (registeredResourceId != null) {
                projectsToolkit.getResourceManager().unregister(registeredResourceId);
                log.info("Unregistered anahata.md resource for project: {}", projectPath);
                this.registeredResourceId = null;
            }
        }
    }
}
