/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the URI-centric engine for the management and surgical mutation 
 * of multimodal resources.
 * <p>
 * This package is the foundational toolkit for the Anahata ASI resource lifecycle, 
 * enabling the bridge between RAG (Retrieval-Augmented Generation) context 
 * augmentation and persistent storage operations.
 * </p>
 * <p>
 * Key Architectural Concepts:
 * </p>
 * <ul>
 *   <li><b>Context Augmentation</b>: Seamless loading of local and remote resources 
 *       into the conversation history via URIs.</li>
 *   <li><b>Surgical Mutations</b>: Precision editing tools (Find/Replace, Full Updates) 
 *       that enforce optimistic locking via {@code lastModified} timestamps.</li>
 *   <li><b>Optimistic Integrity</b>: Ensuring that file modifications are only performed 
 *       on resources currently in context, preventing out-of-sync mutations.</li>
 *   <li><b>Actor Heritage</b>: Automatic identification of the model and tool responsible 
 *       for each resource registration and update.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.toolkit.resources;
