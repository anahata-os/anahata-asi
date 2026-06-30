/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.project.context;

import com.intellij.openapi.project.Project;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.intellij.tools.project.Projects;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;

/**
 * A hierarchical context provider for a specific IntelliJ project.
 * Consolidates metadata, actions, and project-specific instructions (anahata.md).
 * 
 * @author anahata
 */
@Slf4j
public class ProjectContextProvider extends AbstractProjectContextProvider {

    /**
     * Constructs a new project context provider.
     * 
     * @param projectsToolkit The parent Projects toolkit.
     * @param project The IntelliJ project instance.
     */
    public ProjectContextProvider(Projects projectsToolkit, Project project) {
        super(project.getBasePath(), 
              project.getName(), 
              "Root Project Context Provider for project: " + project.getName(),
              projectsToolkit,
              project.getBasePath());
        this.project = project;
        
        // Register with parent
        this.setParent(projectsToolkit);
        
        // Initialize structural children
        ProjectStructureContextProvider structure = new ProjectStructureContextProvider(projectsToolkit, projectPath);
        structure.setParent(this);
        children.add(structure);

        ProjectAlertsContextProvider alerts = new ProjectAlertsContextProvider(projectsToolkit, projectPath);
        alerts.setParent(this);
        children.add(alerts);

        // Sync anahata.md on creation
        syncMdResource();
    }

    @Override
    public String getName() {
        Project p = getProject();
        return p != null ? p.getName() : super.getName();
    }

    @Override
    public void setProviding(boolean enabled) {
        super.setProviding(enabled);
        syncMdResource();
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("\n# Project: ").append(getName()).append("\n");
        sb.append("  - Path: `").append(projectPath).append("`\n");
        
        Path pomPath = Path.of(projectPath).resolve("pom.xml");
        if (Files.exists(pomPath)) {
            sb.append("  - Build System: `Maven`\n");
            try {
                String pomContent = Files.readString(pomPath);
                if (pomContent.contains("<packaging>pom</packaging>")) {
                    sb.append("  - Packaging: `pom`\n");
                } else {
                    sb.append("  - Packaging: `jar`\n");
                }
            } catch (Exception e) {
                // Ignore
            }
        }
        sb.append("  - Java Version: JDK 21 (source)\n");
        ragMessage.addTextPart(sb.toString());
    }

    private void syncMdResource() {
        if (projectPath == null) return;
        String mdPath = Path.of(projectPath).resolve("anahata.md").toAbsolutePath().toString();
        Optional<Resource> existing = projectsToolkit.getAgi().getResourceManager().findByPath(mdPath);

        if (isProviding()) {
            if (existing.isEmpty()) {
                try {
                    Path path = Path.of(mdPath);
                    if (!Files.exists(path)) {
                        Files.writeString(path, "# Project Instructions: " + getName() + "\n\nThis file contains project-specific system instructions.\n");
                    }
                    
                    List<Resource> registered = projectsToolkit.getAgi().getResourceManager().registerPaths(
                        List.of(path), 
                        "added to context by user via project instructions sync"
                    );
                    
                    if (!registered.isEmpty()) {
                        Resource resource = registered.get(0);
                        resource.setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
                        log.info("Registered anahata.md as SYSTEM_INSTRUCTIONS for project: {}", projectPath);
                    }
                } catch (Exception e) {
                    log.error("Failed to sync anahata.md for project: " + projectPath, e);
                }
            } else {
                existing.get().setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
            }
        } else {
            existing.ifPresent(resource -> {
                projectsToolkit.getAgi().getResourceManager().unregister(resource.getId());
                log.info("Unregistered anahata.md for project: {}", projectPath);
            });
        }
    }
}