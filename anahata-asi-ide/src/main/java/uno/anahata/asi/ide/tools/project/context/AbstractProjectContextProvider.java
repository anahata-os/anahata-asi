/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.tools.project.context;

import uno.anahata.asi.ide.tools.project.ProjectOverview;
import uno.anahata.asi.ide.tools.project.AbstractProjects;
import uno.anahata.asi.ide.tools.project.ProjectStructureScope;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.context.BasicContextProvider;
import uno.anahata.asi.agi.context.ContextPosition;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.resource.Resource;

/**
 * Universal base class for project and module context providers across all IDE and desktop environments.
 * <p>
 * Manages physical project root directory paths, hierarchical scope inheritance (via {@link ProjectStructureScope}),
 * and lifecycle synchronization of project-specific system instructions ({@code anahata.md}).
 * </p>
 *
 * @author anahata
 */
@Slf4j
public abstract class AbstractProjectContextProvider extends BasicContextProvider {

    /**
     * The absolute canonical path to this project or module's root directory.
     */
    @Getter
    protected final String projectPath;

    /**
     * The parent Projects toolkit instance, or null if unassociated.
     */
    @Getter
    protected final AbstractProjects projectsToolkit;

    /**
     * Local structure scope override. If null, {@link #getEffectiveScope()} resolves hierarchically
     * from the parent provider chain.
     */
    @Getter
    protected ProjectStructureScope scope;

    /**
     * Sets the local structure scope override for this project or module.
     * Fires a {@code "projectStructureScope"} property change event.
     *
     * @param scope The new granularity scope settings, or null to inherit.
     */
    public void setScope(ProjectStructureScope scope) {
        ProjectStructureScope old = this.scope;
        this.scope = scope;
        propertyChangeSupport.firePropertyChange("projectStructureScope", old, scope);
    }

    /**
     * Constructs a new project or module context provider associated with a parent provider.
     * <p>
     * Immediately initializes the parent hierarchy reference so that {@link #getAgi()} and
     * {@link #getEffectiveScope()} resolve cleanly during construction without timing windows.
     * </p>
     *
     * @param id The unique identifier for this provider.
     * @param name The human-readable name.
     * @param description A brief description of the provided context.
     * @param parentProvider The parent context provider (e.g. {@link AbstractProjects} or a parent project node).
     * @param projectPath The absolute path to the project root directory.
     */
    public AbstractProjectContextProvider(String id, String name, String description, ContextProvider parentProvider, String projectPath) {
        super(id, name, description);
        this.parent = parentProvider;
        this.projectsToolkit = (parentProvider instanceof AbstractProjects ap) ? ap
                : (parentProvider instanceof AbstractProjectContextProvider app ? app.getProjectsToolkit() : null);
        this.projectPath = projectPath;
    }

    /**
     * Resolves the effective {@link ProjectStructureScope} hierarchically.
     * <p>
     * 1. Returns local {@code scope} if explicitly configured on this node.<br>
     * 2. Delegates to parent {@link AbstractProjectContextProvider} if nested (e.g. IntelliJ modules).<br>
     * 3. Falls back to {@link AbstractProjects#defaultScope} from the toolkit (e.g. NetBeans workspace default).<br>
     * 4. Defaults to standard {@link ProjectStructureScope}.
     * </p>
     *
     * @return The non-null effective {@link ProjectStructureScope}.
     */
    public ProjectStructureScope getEffectiveScope() {
        if (scope != null) {
            return scope;
        }
        if (getParentProvider() instanceof AbstractProjectContextProvider parent) {
            return parent.getEffectiveScope();
        }
        if (projectsToolkit != null && projectsToolkit.getDefaultScope() != null) {
            return projectsToolkit.getDefaultScope();
        }
        return new ProjectStructureScope();
    }

    /**
     * Generates or returns a structured {@link ProjectOverview} model for this project or module.
     *
     * @return The populated {@link ProjectOverview} DTO.
     */
    public abstract ProjectOverview getOverview();

    /**
     * {@inheritDoc}
     * <p>
     * Updates providing state and synchronizes the project's {@code anahata.md} instructions file.
     * </p>
     */
    @Override
    public void setProviding(boolean enabled) {
        super.setProviding(enabled);
        syncMdResource();
    }

    /**
     * Synchronizes the project or module's {@code anahata.md} instructions file with the session's resource
     * manager to reflect this provider's active state.
     * <p>
     * When providing, ensures {@code anahata.md} exists (creating a default stub if needed) and registers it
     * at the {@link ContextPosition#SYSTEM_INSTRUCTIONS} context position; when not providing, unregisters it.
     * </p>
     */
    protected void syncMdResource() {
        Agi agi = getAgi();

        Path path = Path.of(projectPath).resolve("anahata.md").toAbsolutePath();
        String mdPath = path.toString();
        Optional<Resource> existing = agi.getResourceManager().findByPath(mdPath);

        if (isProviding()) {
            if (existing.isEmpty()) {
                try {
                    if (!Files.exists(path)) {
                        Files.writeString(path, "# Project Instructions: " + getName() + "\n\nThis file contains project-specific system instructions.\n");
                    }
                    List<Resource> registered = agi.getResourceManager().registerPaths(
                            List.of(path),
                            "added to context by user via project instructions sync"
                    );
                    if (!registered.isEmpty()) {
                        Resource resource = registered.get(0);
                        resource.setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
                        log.info("Registered anahata.md as SYSTEM_INSTRUCTIONS for: {}", projectPath);
                    }
                } catch (Exception e) {
                    log.error("Failed to sync anahata.md for path: " + projectPath, e);
                }
            } else {
                existing.get().setContextPosition(ContextPosition.SYSTEM_INSTRUCTIONS);
            }
        } else {
            existing.ifPresent(resource -> {
                agi.getResourceManager().unregister(resource.getId());
                log.info("Unregistered anahata.md for path: {}", projectPath);
            });
        }
    }

    /**
     * Extracts the first sentence from a Javadoc or Markdown comment string,
     * stripping HTML tags, asterisks, and Markdown link/formatting markers.
     *
     * @param docComment The raw Javadoc or Markdown comment text.
     * @return The sanitized first sentence, or null if empty.
     */
    public static String extractFirstSentence(String docComment) {
        if (docComment == null || docComment.isBlank()) {
            return null;
        }
        // Strip leading Javadoc asterisks, block comment symbols, and Markdown /// markers
        String cleaned = docComment.replaceAll("(?m)^\\s*(/\\*+|\\*+|///+|//+)\\s*", "").trim();
        cleaned = cleaned.replaceAll("^/\\*+\\s*", "").trim();
        cleaned = cleaned.replaceAll("\\s*\\*/$", "").trim();

        // Convert Markdown links [text](url) -> text, and [reference] -> reference
        cleaned = cleaned.replaceAll("\\[([^\\]]+)\\]\\([^\\)]+\\)", "$1");
        cleaned = cleaned.replaceAll("\\[([^\\]]+)\\]", "$1");

        // Remove legacy HTML tags (<p>, <code>, <b>, etc.)
        cleaned = cleaned.replaceAll("<[^>]*>", "").trim();

        // Normalize whitespace
        cleaned = cleaned.replaceAll("\\s+", " ").trim();

        int dotIndex = -1;
        for (int i = 0; i < cleaned.length(); i++) {
            char c = cleaned.charAt(i);
            if (c == '.' && (i + 1 == cleaned.length() || Character.isWhitespace(cleaned.charAt(i + 1)))) {
                dotIndex = i;
                break;
            }
        }
        if (dotIndex != -1) {
            cleaned = cleaned.substring(0, dotIndex + 1);
        } else if (cleaned.length() > 120) {
            cleaned = cleaned.substring(0, 117) + "...";
        }
        return cleaned;
    }
}
