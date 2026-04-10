/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the visual rendering layer for agentic file write operations within NetBeans.
 * <p>
 * This package implements a sophisticated UI architecture that integrates the 
 * Anahata ASI with the native NetBeans Diff module. It uses a decorator-based 
 * approach to overlay AI-generated metadata, comic-style comment bubbles, 
 * and surgical intent dashboards over standard side-by-side diff viewers.
 * </p>
 * <p>
 * Key Architectural Components:
 * </p>
 * <ul>
 *   <li><b>Agentic Decoration</b>: {@link uno.anahata.asi.nb.ui.render.DiffAnnotationsLayerUI} 
 *       uses a {@link javax.swing.JLayer} to paint annotations without disrupting 
 *       the internal component hierarchy of the NetBeans Diff module.</li>
 *   <li><b>Boundary-Aware Scrolling</b>: Solves the "Scroll Trap" problem by 
 *       redirecting mouse wheel events to the parent conversation when the diff 
 *       viewer reaches its boundaries.</li>
 *   <li><b>Document Synchronization</b>: {@link uno.anahata.asi.nb.ui.render.DiffStreamSource} 
 *       ensures that "Merge" actions and manual user edits are reflected back 
 *       into the live AI model state.</li>
 *   <li><b>Surgical Mapping</b>: {@link uno.anahata.asi.nb.ui.render.DiffCommentUtils} 
 *       provides consistent coordinate shifting to keep AI comments aligned 
 *       with proposed line changes.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.ui.render;
