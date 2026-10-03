/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides a hierarchical context-injection layer for NetBeans projects.
 * <p>
 * This package implements the bridging logic between the physical NetBeans 
 * {@code Project} model and the ASI's RAG-based context window. It allows the 
 * AI to maintain a "project-aware" conversation by injecting structural maps, 
 * file trees, and project-specific instructions (via {@code anahata.md}).
 * </p>
 * <p>
 * Architectural Components:
 * </p>
 * <ul>
 *   <li><b>Unified Provider</b>: {@link NbProjectContextProvider} 
 *       acts as the single cohesive provider combining project overview, compiler diagnostics, 
 *       AST structure, and project-specific instructions (via {@code anahata.md}).</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.tools.project.context;
