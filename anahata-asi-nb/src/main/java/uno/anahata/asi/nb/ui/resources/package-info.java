/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the visual and interactive layer for NetBeans-specific resource handles.
 * <p>
 * This package bridges the gap between the platform's model-agnostic resource system 
 * and the high-fidelity UI capabilities of the NetBeans IDE. It ensures that 
 * file-based resources (wrapped in {@code NbHandle}) are rendered with 100% IDE 
 * fidelity, including syntax highlighting, line numbers, and editor-specific actions.
 * </p>
 * <p>
 * Key Components:
 * </p>
 * <ul>
 *   <li><b>Resource Strategy</b>: {@link uno.anahata.asi.nb.ui.resources.NbResourceUI} 
 *       specializes the default UI behaviors, injecting IDE-native navigation 
 *       and selection tools into the resource panels.</li>
 *   <li><b>High-Fidelity Viewing</b>: {@link uno.anahata.asi.nb.ui.resources.NetBeansTextResourceViewer} 
 *       implements a "Total Adoption" strategy, hosting official NetBeans editor 
 *       frames directly within the ASI tool windows.</li>
 *   <li><b>Metadata Visualization</b>: {@link uno.anahata.asi.nb.ui.resources.NbHandlePanel} 
 *       provides a visual inspection of IDE-specific metadata, such as file 
 *       validity and storage origin (e.g., archive vs. local filesystem).</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.ui.resources;
