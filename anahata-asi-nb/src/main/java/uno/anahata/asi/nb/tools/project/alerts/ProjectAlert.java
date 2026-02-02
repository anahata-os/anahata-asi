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
    @Schema(description = "The display name of the problem.")
    private final String displayName;
    @Schema(description = "A detailed description of the problem.")
    private final String description;
    @Schema(description = "The category of the problem.")
    private final String category;
    @Schema(description = "The severity of the problem.")
    private final String severity;
    @Schema(description = "Whether the problem is programmatically resolvable.")
    private final boolean resolvable;
}
