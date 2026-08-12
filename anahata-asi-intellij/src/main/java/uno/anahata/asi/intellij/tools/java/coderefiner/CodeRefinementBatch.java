/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.coderefiner;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * An atomic batch of member-level {@link CodeRefinementIntent}s applied to a single Java
 * file by {@code BatchCodeRefiner#refine}.
 * <p>
 * All intents are applied against the same live PSI tree within one write command, so the
 * whole batch succeeds or fails together. This is the IntelliJ counterpart of the NetBeans
 * V4 {@code CodeRefinementBatch}.
 * </p>
 *
 * @author anahata
 */
@Data
public class CodeRefinementBatch {

    /**
     * The absolute path of the Java file to modify.
     */
    @Schema(description = "The absolute path of the Java file to modify.")
    private String filePath;

    /**
     * The ordered list of structural modifications to apply.
     */
    @Schema(description = "The ordered list of member-level modifications to apply atomically.")
    private List<CodeRefinementIntent> intents;

    /**
     * Whether to optimize imports (shorten FQNs, remove unused) after applying the intents.
     */
    @Schema(description = "Whether to optimize imports after applying the intents.")
    private boolean optimizeImports;

    /**
     * Whether to persist the file to disk after applying the intents.
     */
    @Schema(description = "Whether to save the file to disk after applying the intents.")
    private boolean save;
}
