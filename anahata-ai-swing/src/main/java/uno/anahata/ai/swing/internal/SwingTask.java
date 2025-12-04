
package uno.anahata.ai.swing.internal;

import java.util.concurrent.Callable;
import java.util.function.Consumer;
import javax.swing.SwingWorker;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class SwingTask<T> extends SwingWorker<T, Void> {
    private final Callable<T> backgroundTask;
    private final Consumer<T> onDone;
    private final Consumer<Exception> onError;

    private SwingTask(Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        this.backgroundTask = backgroundTask;
        this.onDone = onDone;
        this.onError = onError;
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
        } catch (Exception e) {
            // Unwrap the InvocationTargetException if present
            Throwable cause = e.getCause() != null ? e.getCause() : e;
            log.error("Error in background task", cause);
            if (onError != null) {
                onError.accept((Exception) cause);
            }
        }
    }

    public static <T> void run(Callable<T> backgroundTask, Consumer<T> onDone, Consumer<Exception> onError) {
        new SwingTask<>(backgroundTask, onDone, onError).execute();
    }
}
