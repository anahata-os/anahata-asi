/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.java.coderefiner;

/**
 * Placement of an inserted or moved member relative to its target class or an anchor
 * member, used by {@link CodeRefinementIntent}.
 *
 * @author anahata
 */
public enum RelativePosition {

    /** Insert as the first member of the target class body. */
    START,

    /** Insert as the last member of the target class body. */
    END,

    /** Insert immediately before the named anchor member. */
    BEFORE,

    /** Insert immediately after the named anchor member. */
    AFTER
}
