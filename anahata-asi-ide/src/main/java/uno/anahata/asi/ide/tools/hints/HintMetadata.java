/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ide.tools.hints;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Universal DTO representing metadata for a registered inspection rule or hint type in an IDE catalog.
 *
 * @author anahata
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class HintMetadata {

    /**
     * The unique identifier of the hint or inspection rule.
     */
    private String id;

    /**
     * The user-visible display name of the hint.
     */
    private String displayName;

    /**
     * A description of what this hint checks and why.
     */
    private String description;

    /**
     * The category or folder where the hint belongs.
     */
    private String category;

    /**
     * The default or configured localized severity level of this hint.
     */
    private String severity;

    /**
     * Whether this hint is active in the IDE settings.
     */
    private boolean enabled;
}
