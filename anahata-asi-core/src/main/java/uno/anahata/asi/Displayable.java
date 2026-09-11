/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi;

/**
 * Common contract for domain entities and DTOs that provide a human-readable
 * display string for UI components (such as tab headers, list items, and chips).
 *
 * @author anahata
 */
public interface Displayable {

    /**
     * Returns the human-readable display value representing this entity in UI views.
     *
     * @return The display string.
     */
    String getDisplayValue();
}
