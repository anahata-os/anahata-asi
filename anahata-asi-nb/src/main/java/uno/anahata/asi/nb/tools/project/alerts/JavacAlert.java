/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.project.alerts;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents a single Java compiler alert (error or warning).
 */
@Schema(description = "Represents a single Java compiler alert (error or warning).")
@Data
@AllArgsConstructor
public final class JavacAlert {
    @Schema(description = "The absolute path to the Java source file.")
    private final String filePath;
    @Schema(description = "The kind of alert (e.g., ERROR, WARNING).")
    private final String kind;
    @Schema(description = "The line number (1-based).")
    private final int lineNumber;
    @Schema(description = "The column number (1-based).")
    private final int columnNumber;
    @Schema(description = "The compiler message.")
    private final String message;
}
