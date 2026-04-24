/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.internal;

import java.awt.Component;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.FutureTask;
import java.util.concurrent.CancellationException;
import java.util.function.Consumer;
import javax.swing.SwingUtilities;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.swing.AbstractAsiContainerPanel;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.components.ExceptionDialog;

/**
 * A robust, high-performance background task execution unit for the Anahata ASI.
 * <p>
 * Unlike standard {@code SwingWorker}, {@code SwingTask} executes on the 
 * professional, named thread pools managed by the {@link Agi} or 
 * {@link AbstractAsiContainer}. This ensures perfect isolation, diagnostic 
 * clarity (thread names match sessions), and eliminates contention with 
 * generic JDK background tasks.
 * </p>
 * <p>
 * <b>Lifecycle:</b> Call {@link #start()} to register the task and begin 
 * execution. Success and error callbacks are guaranteed to run on the 
 * Event Dispatch Thread (EDT).
 * </p>
 * 
 * @param <T> The result type produced by the background task.
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class SwingTask<T> {
    
    /** The parent component for positioning modal dialogs. */
    private Component owner;
    /** The Agi session this task belongs to, if any. */
    private Agi agi;
    /** The container this task belongs to. */
    private AbstractAsiContainer container;
    /** The executor service used for background execution. */
    private final ExecutorService executor;
    /** A human-readable identifier for the task. */
    private String taskName;
    /** The functional core of the task. */
    private Callable<T> backgroundTask;
    /** The EDT callback for successful results. */
    private Consumer<T> onDone;
    /** The EDT callback for failures. */
    private Consumer<Exception> onError;    
    /** Flag for automatic ExceptionDialog presentation. */
    private boolean showError;
    
    /** Internal future managing the task state. */
    private FutureTask<T> futureTask;

    /**
     * Constructs a SwingTask bound to a specific Agi session.
     * Uses the Agi's dedicated thread pool.
     * 
     * @param agiPanel The parent Agi UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     * @param onDone Success callback (EDT).
     * @param onError Error callback (EDT).
     * @param showError Whether to show error dialog.
     */
    public SwingTask(AgiPanel agiPanel, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError, boolean showError) {
        this.owner = agiPanel;
        this.agi = agiPanel.getAgi();
        this.container = agi.getConfig().getAsiContainer();
        this.executor = agi.getExecutor();
        this.taskName = taskName;
        this.backgroundTask = backgroundTask;
        this.onDone = onDone;
        this.onError = onError;
        this.showError = showError;
    }

    /**
     * Constructs a SwingTask bound to a specific Agi session with default settings.
     * 
     * @param agiPanel The parent Agi UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     * @param onDone Success callback (EDT).
     * @param onError Error callback (EDT).
     */
    public SwingTask(AgiPanel agiPanel, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        this(agiPanel, taskName, backgroundTask, onDone, onError, true);
    }

    /**
     * Constructs a SwingTask bound to a specific Agi session with only a success callback.
     * 
     * @param agiPanel The parent Agi UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     * @param onDone Success callback (EDT).
     */
    public SwingTask(AgiPanel agiPanel, String taskName, Callable<T> backgroundTask, Consumer<T> onDone) {
        this(agiPanel, taskName, backgroundTask, onDone, null, true);
    }

    /**
     * Constructs a minimal SwingTask bound to a specific Agi session.
     * 
     * @param agiPanel The parent Agi UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     */
    public SwingTask(AgiPanel agiPanel, String taskName, Callable<T> backgroundTask) {
        this(agiPanel, taskName, backgroundTask, null, null, true);
    }

    /**
     * Constructs a SwingTask bound to the global Container dashboard.
     * Uses the container's shared infrastructure thread pool.
     * 
     * @param containerPanel The parent container dashboard UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     * @param onDone Success callback (EDT).
     * @param onError Error callback (EDT).
     * @param showError Whether to show error dialog.
     */
    public SwingTask(AbstractAsiContainerPanel containerPanel, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError, boolean showError) {
        this.owner = containerPanel;
        this.container = containerPanel.getAsiContainer();
        this.executor = container.getExecutor();
        this.taskName = taskName;
        this.backgroundTask = backgroundTask;
        this.onDone = onDone;
        this.onError = onError;
        this.showError = showError;
    }

    /**
     * Constructs a SwingTask bound to the global Container dashboard.
     * 
     * @param containerPanel The parent container dashboard UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     * @param onDone Success callback (EDT).
     * @param onError Error callback (EDT).
     */
    public SwingTask(AbstractAsiContainerPanel containerPanel, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        this(containerPanel, taskName, backgroundTask, onDone, onError, true);
    }

    /**
     * Constructs a SwingTask bound to the global Container dashboard with only a success callback.
     * 
     * @param containerPanel The parent container dashboard UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     * @param onDone Success callback (EDT).
     */
    public SwingTask(AbstractAsiContainerPanel containerPanel, String taskName, Callable<T> backgroundTask, Consumer<T> onDone) {
        this(containerPanel, taskName, backgroundTask, onDone, null, true);
    }

    /**
     * Constructs a SwingTask bound to the global Container dashboard with default settings.
     * 
     * @param containerPanel The parent container dashboard UI.
     * @param taskName Descriptive name.
     * @param backgroundTask Logic to run in background.
     */
    public SwingTask(AbstractAsiContainerPanel containerPanel, String taskName, Callable<T> backgroundTask) {
        this(containerPanel, taskName, backgroundTask, null, null, true);
    }

    /** 
     * Starts the task execution on the designated professional executor.
     * Registers with {@link SwingTaskManager} for UI observability.
     */
    public void start() {
        SwingTaskManager.getInstance().taskStarted(this);
        
        this.futureTask = new FutureTask<>(backgroundTask) {
            @Override
            protected void done() {
                SwingUtilities.invokeLater(() -> handleCompletion());
            }
        };
        
        executor.submit(futureTask);
    }

    /** 
     * Propagates results or errors to the EDT callbacks.
     */
    private void handleCompletion() {
        try {
            T result = futureTask.get();
            if (onDone != null) {
                onDone.accept(result);
            }
        } catch (CancellationException ce) {
            log.info("Task '{}' was cancelled.", taskName);
        } catch (InterruptedException | ExecutionException e) {
            log.error("Execution error in task: {}", taskName, e);
            if (showError) {
                ExceptionDialog.show(owner, taskName, "An error occurred during background task: " + taskName, e);
            }
            if (onError != null) {
                onError.accept(e instanceof ExecutionException ? (Exception) e.getCause() : e);
            }
        } finally {
            SwingTaskManager.getInstance().taskFinished(this);
        }
    }

    /** 
     * Cancels the background task.
     * @param mayInterruptIfRunning whether to interrupt the thread.
     */
    public void cancel(boolean mayInterruptIfRunning) {
        if (futureTask != null) {
            futureTask.cancel(mayInterruptIfRunning);
        }
    }
    
    /** @return true if the task was cancelled. */
    public boolean isCancelled() {
        return futureTask != null && futureTask.isCancelled();
    }
    
    /** @return true if the task has completed. */
    public boolean isDone() {
        return futureTask != null && futureTask.isDone();
    }
}
