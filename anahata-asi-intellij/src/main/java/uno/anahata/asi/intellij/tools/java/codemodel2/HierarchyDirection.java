/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.codemodel2;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Specifies the traversal direction for type hierarchy exploration across JVM languages.
 *
 * @author anahata
 */
@Schema(description = "Traversal direction for type hierarchy exploration.")
public enum HierarchyDirection {

    /**
     * Navigates down the hierarchy: finds types that extend or implement the target type (subclasses and implementors).
     */
    @Schema(description = "Navigates down the hierarchy: finds subclasses and interface implementors.")
    SUBTYPES,

    /**
     * Navigates up the hierarchy: finds base classes and interfaces that the target type extends or implements.
     */
    @Schema(description = "Navigates up the hierarchy: finds extended superclasses and implemented interfaces.")
    SUPERTYPES
}
