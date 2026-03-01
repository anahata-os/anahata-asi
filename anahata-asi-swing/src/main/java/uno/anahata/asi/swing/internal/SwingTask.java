/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.internal;

import java.awt.Component;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import javax.swing.SwingWorker;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * A robust, specialized {@link SwingWorker} implementation for executing 
 * background tasks with integrated lifecycle management and UI feedback.
 * 
 * <p>SwingTask provides a clean, functional API for handling the transition 
 * between the background execution thread and the Event Dispatch Thread (EDT), 
 * with built-in support for success/error callbacks and automatic error 
 * dialog presentation.</p>
 * 
 * @param <T> The result type produced by the background task.
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class SwingTask<T> extends SwingWorker<T, Void> {
    
    /** The parent component for centering error dialogs. Can be null. */
    private Component owner;
    /** A descriptive name for the task, used in log messages and dialogs. */
    private String taskName;
    /** The actual logic to execute in the background thread. */
    private Callable<T> backgroundTask;
    /** Callback triggered on the EDT upon successful completion. */
    private Consumer<T> onDone;
    /** Callback triggered on the EDT if an exception occurs. */
    private Consumer<Exception> onError;    
    /** Whether to automatically display a modal error dialog on failure. */
    private boolean showError;

    /**
     * Constructs a fully configured SwingTask.
     * 
     * @param owner The parent component.
     * @param taskName The task name.
     * @param backgroundTask The background logic.
     * @param onDone Success callback.
     * @param onError Error callback.
     * @param showError Whether to show dialog on error.
     */
    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError, boolean showError) {
        this.owner = owner;
        this.taskName = taskName;
        this.backgroundTask = backgroundTask;
        this.onDone = onDone;
        this.onError = onError;
        this.showError = showError;
    }

    /**
     * Constructs a SwingTask with default error dialog visibility enabled.
     * 
     * @param owner The parent component.
     * @param taskName The task name.
     * @param backgroundTask The background logic.
     * @param onDone Success callback.
     * @param onError Error callback.
     */
    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        this(owner, taskName, backgroundTask, onDone, onError, true);
    }

    /**
     * Constructs a SwingTask with only a success callback.
     * 
     * @param owner The parent component.
     * @param taskName The task name.
     * @param backgroundTask The background logic.
     * @param onDone Success callback.
     */
    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask, Consumer<T> onDone) {
        this(owner, taskName, backgroundTask, onDone, null, true);
    }

    /**
     * Constructs a SwingTask with no explicit callbacks.
     * 
     * @param owner The parent component.
     * @param taskName The task name.
     * @param backgroundTask The background logic.
     */
    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask) {
        this(owner, taskName, backgroundTask, null, null, true);
    }

    /**
     * Executes the background logic in a worker thread.
     * 
     * @return The task result.
     * @throws Exception if execution fails.
     */
    @Override
    protected T doInBackground() throws Exception {
        return backgroundTask.call();
    }

    /**
     * Handles task finalization on the Event Dispatch Thread (EDT).
     * This method manages the execution of success/error callbacks and 
     * handles standard worker exceptions (Interruption, ExecutionException).
     */
    @Override
    protected void done() {
        try {
            T result = get();
            if (onDone != null) {
                onDone.accept(result);
            }
        } catch (InterruptedException | ExecutionException e) {
            if (showError) {
                SwingUtils.showException(owner, taskName, "An error occurred during background task " + taskName, e);
            }
            if (onError != null) {
                onError.accept(e);
            }
        }
    }
}
