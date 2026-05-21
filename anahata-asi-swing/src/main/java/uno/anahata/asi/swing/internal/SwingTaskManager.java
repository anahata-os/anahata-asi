/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.internal;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;

/**
 * A centralized registry for tracking active {@link SwingTask} instances.
 * <p>
 * This manager provides real-time observability into background activities 
 * initiated by the Swing UI. It allows components like the {@code StatusPanel} 
 * to display progress and status messages for non-AI tasks (e.g., token 
 * calculation, file system operations).
 * </p>
 * 
 * @author anahata
 */
public class SwingTaskManager extends BasicPropertyChangeSource {
    
    /**
     * The global singleton manager instance.
     */
    private static final SwingTaskManager INSTANCE = new SwingTaskManager();

    /**
     * Gets the singleton instance of the task manager.
     * @return The task manager instance.
     */
    public static SwingTaskManager getInstance() {
        return INSTANCE;
    }

    /**
     * The thread-safe concurrent list tracking all uncompleted background actions.
     */
    private final List<SwingTask<?>> activeTasks = new CopyOnWriteArrayList<>();

    /**
     * Private constructor preventing instantiation from other packages.
     */
    private SwingTaskManager() {
    }

    /**
     * Registers a task as started.
     * @param task The task that started.
     */
    public void taskStarted(SwingTask<?> task) {
        if (task != null) {
            activeTasks.add(task);
            propertyChangeSupport.firePropertyChange("activeTasks", null, activeTasks);
        }
    }

    /**
     * Unregisters a task as finished.
     * @param task The task that finished.
     */
    public void taskFinished(SwingTask<?> task) {
        if (activeTasks.remove(task)) {
            propertyChangeSupport.firePropertyChange("activeTasks", null, activeTasks);
        }
    }

    /**
     * Gets an unmodifiable list of currently active swing tasks.
     * @return The list of active tasks.
     */
    public List<SwingTask<?>> getActiveTasks() {
        return java.util.Collections.unmodifiableList(activeTasks);
    }
}
