/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.alerts;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents a high-level project problem (e.g., missing dependency).
 */
@Schema(description = "Represents a high-level project problem (e.g., missing dependency).")
@Data
@AllArgsConstructor
public final class ProjectAlert {
    /**
     * The human-readable name of the project alert.
     */
    @Schema(description = "The display name of the problem.")
    private final String displayName;
    /**
     * A detailed explanation of the project problem.
     */
    @Schema(description = "A detailed description of the problem.")
    private final String description;
    /**
     * The category of the alert (e.g., Build, Runtime, Dependency).
     */
    @Schema(description = "The category of the problem.")
    private final String category;
    /**
     * The severity level of the alert (e.g., Error, Warning, Info).
     */
    @Schema(description = "The severity of the problem.")
    private final String severity;
    /**
     * Indicates whether the framework can attempt an automatic fix for this alert.
     */
    @Schema(description = "Whether the problem is programmatically resolvable.")
    private final boolean resolvable;
}
