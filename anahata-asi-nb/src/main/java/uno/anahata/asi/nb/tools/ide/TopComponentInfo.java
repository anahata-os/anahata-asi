/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.nb.tools.ide;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Represents detailed information about an open TopComponent (window) in the IDE.
 */
@Schema(description = "Represents detailed information about an open TopComponent (window) in the IDE.")
@Data
@AllArgsConstructor
public final class TopComponentInfo {
    private final String id;
    private final String name;
    private final boolean selected;
    private final String displayName;
    private final String htmlDisplayName;
    private final String tooltip;
    private final String className;
    private final String mode;
    private final String activatedNodes;
    private final String supportedActions;
    private final String filePath;
    private final String primaryFilePath;
    private final long sizeInBytes;
}
