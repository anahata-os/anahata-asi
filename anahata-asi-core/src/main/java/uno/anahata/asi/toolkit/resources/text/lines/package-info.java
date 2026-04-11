/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the next-generation surgical precision engine for line-based 
 * text mutations.
 * <p>
 * This package implements an atomic editing model where file changes are 
 * decomposed into discrete semantic operations: insertions, replacements, 
 * and deletions.
 * </p>
 * <p>
 * Key Architectural Concepts:
 * </p>
 * <ul>
 *   <li><b>Coordinate Stability</b>: Uses a bottom-up application strategy (descending line numbers) 
 *       to ensure that individual edits do not shift the coordinates of subsequent operations.</li>
 *   <li><b>Semantic Intent</b>: Differentiates between "push-down" insertions and range replacements 
 *       to provide the ASI with maximum precision during code modification.</li>
 *   <li><b>Validation & Safety</b>: Includes a robust overlap detection system that prevents 
 *       conflicting edits within the same turn.</li>
 *   <li><b>Deterministic Ordering</b>: The {@link uno.anahata.asi.toolkit.resources.text.lines.SurgicalEditComparator} 
 *       ensures a predictable tie-breaking sequence when multiple edits target the same line.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.toolkit.resources.text.lines;
