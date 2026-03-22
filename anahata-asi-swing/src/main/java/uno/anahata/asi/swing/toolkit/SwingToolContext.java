/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.toolkit;

import java.lang.reflect.InvocationTargetException;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 * An advanced execution context that bridges the gap between background tool execution 
 * and the Swing Event Dispatch Thread (EDT).
 * <p>
 * This class provides the critical infrastructure for <b>Context Propagation</b>. 
 * Since the AI toolchain relies on {@link ThreadLocal} state for logging and 
 * attachments, moving execution to the EDT would normally break these links. 
 * {@code SwingToolContext} solves this by capturing the state and re-applying it 
 * within the EDT task scope.
 * </p>
 * 
 * @author anahata
 */
public class SwingToolContext extends ToolContext {
    /**
     * Executes a task on the EDT in a non-blocking manner while maintaining 
     * full context awareness.
     * <p>
     * This is the preferred way for tools to perform UI updates without 
     * freezing the AI execution thread.
     * </p>
     * 
     * @param runnable The UI-bound task to execute.
     */
    public void runInEdt(Runnable runnable) {
        final JavaMethodToolResponse response = JavaMethodToolResponse.getCurrent();
        SwingUtils.runInEDT(() -> {
            if (response != null) {
                JavaMethodToolResponse.setCurrent(response);
            }
            try {
                runnable.run();
            } finally {
                JavaMethodToolResponse.setCurrent(null);
            }
        });
    }

    /**
     * Executes a task on the EDT and blocks the current thread until the 
     * task is complete, maintaining full context awareness throughout.
     * <p>
     * Use this method when subsequent tool logic depends on the UI having 
     * reached a specific state (e.g., waiting for a dialog to close or 
     * a component to render).
     * </p>
     * 
     * @param runnable The UI-bound task to execute.
     * @throws InterruptedException if the background thread is interrupted during wait.
     * @throws InvocationTargetException if the EDT task throws an unhandled exception.
     */
    public void runInEdtAndWait(Runnable runnable) throws InterruptedException, InvocationTargetException {
        final JavaMethodToolResponse response = JavaMethodToolResponse.getCurrent();
        SwingUtils.runInEDTAndWait(() -> {
            if (response != null) {
                JavaMethodToolResponse.setCurrent(response);
            }
            try {
                runnable.run();
            } finally {
                JavaMethodToolResponse.setCurrent(null);
            }
        });
    }

}
