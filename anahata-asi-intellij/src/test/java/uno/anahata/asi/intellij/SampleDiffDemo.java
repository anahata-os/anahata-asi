/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * Sample domain service demonstrating native IntelliJ syntax highlighting,
 * code diff visualization, and real-time editing within the Anahata ASI tool panel.
 *
 * @author anahata
 */
@Slf4j
@Getter
public class SampleDiffDemo {

    /**
     * The unique name identifier for this service instance.
     */
    private final String serviceName;

    /**
     * In-memory list of registered task identifiers.
     */
    private final List<String> activeTasks;

    /**
     * Constructs a new sample service instance.
     *
     * @param serviceName the unique name for this service.
     * @param activeTasks initial collection of tasks.
     */
    public SampleDiffDemo(String serviceName, List<String> activeTasks) {
        this.serviceName = serviceName;
        this.activeTasks = activeTasks;
    }

    /**
     * Thread-safe execution counter tracking processed runs.
     */
    private final AtomicInteger executionCounter = new AtomicInteger(0);

    /**
     * Executes the task processing workflow, logging each active task item
     * with real-time atomic execution counter tracking.
     *
     * @return the total number of tasks processed.
     */
    public int processTasks() {
        int runNumber = executionCounter.incrementAndGet();
        log.info("Processing tasks for service: {} (Run #{})", serviceName, runNumber);
        int count = 0;
        for (String task : activeTasks) {
            log.info("Executing task #{}: {}", count + 1, task);
            count++;
        }
        return count;
    }

    /**
     * Resets the execution counter back to zero.
     */
    public void resetCounter() {
        executionCounter.set(0);
        log.info("Execution counter reset for service: {}", serviceName);
    }
}
