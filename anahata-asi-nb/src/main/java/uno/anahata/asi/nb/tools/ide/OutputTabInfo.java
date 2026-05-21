/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents a logical snapshot of a tab in the NetBeans Output Window.
 * <p>
 * This DTO captures identifying information and current state (such as line count 
 * and running status) for a specific output stream.
 * </p>
 */
@Schema(description = "Represents information about a tab in the NetBeans Output Window.")
@Data
@AllArgsConstructor
public final class OutputTabInfo {
    /** The unique identifier of the output tab. */
    private final long id;
    /** The user-visible display name of the output tab. */
    private final String displayName;
    /** The size of the captured output content in characters. */
    private final int contentSize;
    /** The total line count of the output stream. */
    private final int totalLines;
    /** Whether the associated Maven or system process is still running. */
    private final boolean isRunning;
}
