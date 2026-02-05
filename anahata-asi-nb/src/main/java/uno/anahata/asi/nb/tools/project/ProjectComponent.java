/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project;

import io.swagger.v3.oas.annotations.media.Schema;
import javax.lang.model.element.ElementKind;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Represents a key conceptual component of a project, like a major class or package.
 * 
 * @author Anahata
 */
@Schema(description = "Represents a key conceptual component of a project, like a major class or package.")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public final class ProjectComponent {

    @Schema(description = "The fully qualified name of the component.")
    private String fqn;

    @Schema(description = "The kind of element (CLASS, INTERFACE, ENUM, RECORD, etc.).")
    private ElementKind kind;
}
