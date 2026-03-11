/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.swing.toolkit;

import java.lang.reflect.InvocationTargetException;
import uno.anahata.asi.agi.tool.ToolContext;
import uno.anahata.asi.agi.tool.spi.java.JavaMethodToolResponse;
import uno.anahata.asi.swing.internal.SwingUtils;

/**
 *
 * @author pablo
 */
public class SwingToolContext extends ToolContext {
    /**
     * Executes the given runnable on the Event Dispatch Thread (EDT).
     * <p>
     * This method automatically captures the current tool execution context 
     * (response, logs, etc.) and propagates it to the EDT, allowing 
     * {@code log()}, {@code error()}, and {@code addAttachment()} to work 
     * correctly inside the runnable.
     * </p>
     * 
     * @param runnable The code to execute on the EDT.
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
     * Executes the given runnable on the Event Dispatch Thread (EDT) and waits 
     * for its completion.
     * <p>
     * This method automatically captures the current tool execution context 
     * (response, logs, etc.) and propagates it to the EDT, allowing 
     * {@code log()}, {@code error()}, and {@code addAttachment()} to work 
     * correctly inside the runnable.
     * </p>
     * 
     * @param runnable The code to execute on the EDT.
     * @throws InterruptedException if the current thread is interrupted while waiting.
     * @throws InvocationTargetException if an exception occurs during execution of the runnable.
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
