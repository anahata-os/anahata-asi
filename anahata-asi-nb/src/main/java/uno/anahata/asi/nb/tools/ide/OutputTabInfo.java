/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents information about a tab in the NetBeans Output Window.
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
