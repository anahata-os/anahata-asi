/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Provides the core machinery for dynamic Java compilation and execution.
 * <p>
 * This package implements a high-reload JVM environment where source code 
 * provided by the model can be compiled and executed on-the-fly. It features 
 * a sophisticated {@link uno.anahata.asi.toolkit.java.Java} toolkit that 
 * manages a dual-phase classloading strategy: 
 * </p>
 * <ul>
 *   <li><b>Parent-First:</b> For critical framework infrastructure (Agi, 
 *   ToolContext, etc.) to prevent class identity mismatches.</li>
 *   <li><b>Child-First:</b> For project-specific classes to enable 
 *   instant hot-reloading of logic without restarting the JVM.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.toolkit.java;
