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
 *   <li><b>Context Abstraction</b>: {@link uno.anahata.asi.nb.tools.project.context.AbstractProjectContextProvider} 
 *       standardizes project resolution and IDE UI notification logic.</li>
 *   <li><b>Root Orchestration</b>: {@link uno.anahata.asi.nb.tools.project.context.ProjectContextProvider} 
 *       acts as the lifecycle manager for a project's context, including its associated instructions.</li>
 *   <li><b>Structural Providers</b>: Specialized providers like {@link uno.anahata.asi.nb.tools.project.context.ProjectStructureContextProvider} 
 *       and {@link uno.anahata.asi.nb.tools.project.context.ProjectFilesContextProvider} generate 
 *       high-fidelity Markdown representations of the project's internal state.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.tools.project.context;
