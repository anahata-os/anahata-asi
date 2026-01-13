
package uno.anahata.ai.swing.internal;

import java.awt.Component;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import javax.swing.SwingWorker;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Getter
@Setter
public class SwingTask<T> extends SwingWorker<T, Void> {
    private Component owner;
    private String taskName;
    private Callable<T> backgroundTask;
    private Consumer<T> onDone;
    private Consumer<Exception> onError;    
    private boolean showError;

    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError, boolean showError) {
        this.owner = owner;
        this.taskName = taskName;
        this.backgroundTask = backgroundTask;
        this.onDone = onDone;
        this.onError = onError;
        this.showError = showError;
    }

    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        this(owner, taskName, backgroundTask, onDone, onError, true);
    }

    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask, Consumer<T> onDone) {
        this(owner, taskName, backgroundTask, onDone, null, true);
    }

    public SwingTask(Component owner, String taskName, Callable<T> backgroundTask) {
        this(owner, taskName, backgroundTask, null, null, true);
    }

    @Override
    protected T doInBackground() throws Exception {
        return backgroundTask.call();
    }

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