/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Provides the core framework for desktop java -aware agentic tool execution.
 * <p>
 * This package defines the **Desktop-Aware Tool Execution** pattern, which enables 
 * background-running AI tools to interact safely and reactively with the 
 * Swing Event Dispatch Thread (EDT) or the JavaFX application thread.
 * </p>
 * <p>
 * The primary orchestration is handled by {@link uno.anahata.asi.swing.agi.tool.DesktopAgiTool}, 
 * which provides the necessary hooks for context-aware UI synchronization 
 * during complex agentic workflows.
 * </p>
 * 
 * @author anahata
 */
package uno.anahata.asi.swing.agi.tool;
