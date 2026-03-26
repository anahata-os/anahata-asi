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
    private final long id;
    private final String displayName;
    private final int contentSize;
    private final int totalLines;
    private final boolean isRunning;
}
