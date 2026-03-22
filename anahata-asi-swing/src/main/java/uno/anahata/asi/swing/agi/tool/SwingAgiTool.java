/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.tool;

import java.util.concurrent.Callable;
import uno.anahata.asi.swing.toolkit.SwingToolContext;

/**
 * The foundational base class for all Swing-based agentic tools.
 * <p>
 * This class extends {@link SwingToolContext} to bridge the gap between background 
 * AI tool execution and the Swing Event Dispatch Thread (EDT). It provides 
 * built-in support for context-aware UI updates via {@code runInEdt} and 
 * {@code runInEdtAndWait} helpers.
 * </p>
 * <p>
 * All Swing-aware tools must implement the {@link #call()} method to define 
 * their core logic.
 * </p>
 * 
 * @author anahata
 */
public abstract class SwingAgiTool extends SwingToolContext implements Callable<Object>{

}
