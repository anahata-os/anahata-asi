/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Swing-specific toolkit implementations and context abstractions for Anahata ASI.
 * <p>
 * This package provides toolkits that are tightly integrated with the Swing Event 
 * Dispatch Thread (EDT) and the host's graphical environment. It includes 
 * sophisticated context propagation mechanisms that allow code running on the 
 * EDT to maintain full access to the AI tool execution context (logging, 
 * attachments, etc.).
 * </p>
 * 
 * <h2>Key Components:</h2>
 * <ul>
 *   <li><b>{@link uno.anahata.asi.swing.toolkit.Screens}</b>: 
 *       Hardware-level display and window capture utility.</li>
 *   <li><b>{@link uno.anahata.asi.swing.toolkit.SwingJava}</b>: 
 *       An advanced Java execution toolkit that automatically injects Swing-specific 
 *       helpers into the model's generated code.</li>
 *   <li><b>{@link uno.anahata.asi.swing.toolkit.SwingToolContext}</b>: 
 *       The backbone of context-aware Swing execution, providing thread-local 
 *       capture and re-application across thread boundaries.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.swing.toolkit;
