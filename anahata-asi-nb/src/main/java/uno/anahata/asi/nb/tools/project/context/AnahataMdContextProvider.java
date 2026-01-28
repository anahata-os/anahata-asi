/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.context;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.nb.tools.project.Projects;

/**
 * Provides project-specific system instructions from the anahata.md file.
 * It manages a stateful {@link TextFileResource} that is injected into the 
 * {@link ContextPosition#SYSTEM_INSTRUCTIONS} position.
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
     * Overridden to trigger a resource sync when the provider is enabled or disabled.
     */
    @Override
    public void setProviding(boolean enabled) {
        super.setProviding(enabled);
        syncResource();
    }

    /**
     * Synchronizes the anahata.md file with the chat's resource manager.
     * If the provider is enabled and the file exists, it is registered as a 
     * SYSTEM_INSTRUCTIONS resource. Otherwise, it is unregistered.
     */
    private void syncResource() {
        Path path = Paths.get(projectPath, "anahata.md");
        
        if (isProviding() && Files.exists(path)) {
            if (registeredResourceId == null) {
                try {
                    TextFileResource resource = new TextFileResource(projectsToolkit.getResourceManager(), path);
                    // Force the position to SYSTEM_INSTRUCTIONS
                    resource.setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
                    resource.reload();
                    projectsToolkit.getResourceManager().register(resource);
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
