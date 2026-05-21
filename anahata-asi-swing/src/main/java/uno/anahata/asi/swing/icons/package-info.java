/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * The central hub for all programmatically drawn icons within the Anahata ASI Swing UI.
 * <p>
 * This package provides a high-fidelity, provider-agnostic iconography system designed 
 * to give immediate visual feedback on the state of the AGI and its various components. 
 * As of the V2 architecture, all programmatic icons have been refactored to extend 
 * {@link uno.anahata.asi.swing.icons.AbstractAnahataIcon}, which standardizes dimension 
 * reporting and coordinate calculations.
 * </p>
 * 
 * <h2>Visual Philosophy</h2>
 * <p>
 * The icons in this package primarily leverage the authentic <b>F.C. Barcelona palette</b>:
 * </p>
 * <ul>
 *   <li><b>Barça Blue:</b> Used for core navigational and structural elements (e.g., {@link uno.anahata.asi.swing.icons.SendIcon}, {@link uno.anahata.asi.swing.icons.PulseIcon}).</li>
 *   <li><b>Barça Red:</b> Used for high-impact, multimodal, or destructive actions (e.g., {@link uno.anahata.asi.swing.icons.DeleteIcon}, {@link uno.anahata.asi.swing.icons.MicrophoneIcon}).</li>
 *   <li><b>Barça Yellow:</b> Used for state badges, labels, and confirmations (e.g., {@link uno.anahata.asi.swing.icons.OkIcon}, {@link uno.anahata.asi.swing.icons.SaveIcon}).</li>
 * </ul>
 * 
 * <h2>Key Components</h2>
 * <ul>
 *   <li><b>{@link uno.anahata.asi.swing.icons.AbstractAnahataIcon}:</b> The foundational base class enforcing square dimensions.</li>
 *   <li><b>{@link uno.anahata.asi.swing.icons.IconUtils}:</b> A utility class for scaling images and managing a global icon registry.</li>
 *   <li><b>{@link uno.anahata.asi.swing.icons.IconProvider}:</b> An interface allowing host-specific modules (like NetBeans) to inject authentic system icons into the ASI context views.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.swing.icons;
