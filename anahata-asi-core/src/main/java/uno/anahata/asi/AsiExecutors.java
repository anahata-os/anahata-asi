/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.concurrent.BasicThreadFactory;

/**
 * Centralized factory for creating named, daemon thread pools used across the 
 * Anahata ASI framework. 
 * <p>Infrastructure Note: These pools serve both the global {@link AbstractAsiContainer} 
 * (for management tasks) and individual {@link uno.anahata.asi.agi.Agi} 
 * sessions (for lifecycle and tool execution).</p>
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class AsiExecutors {

    /**
     * Creates a new cached thread pool specifically for managing the 
     * lifecycle of a single intelligence (Agi) session. 
     * <p>Implementation details: Threads are marked as daemon to ensure they 
     * do not block the shutdown of the host application (NetBeans, etc.).</p>
     * @param threadPreffix The prefix for naming threads in the pool.
     * @return A new {@link java.util.concurrent.ExecutorService}.
     */
    public static ExecutorService newCachedThreadPoolExecutor(String threadPreffix) {
        BasicThreadFactory factory = new BasicThreadFactory.Builder()
                .namingPattern("anahata-asi-" + threadPreffix + "-thread-%d")
                .daemon(true)
                .priority(Thread.NORM_PRIORITY)
                .build();
        return Executors.newCachedThreadPool(factory);
    }

    /**
     * Creates a new scheduled thread pool for background monitoring and periodic tasks.
     *
     * @param threadPreffix The unique identifier for the task, used in the thread name.
     * @param corePoolSize The number of threads to keep in the pool.
     * @return A new scheduled thread pool ExecutorService.
     */
    public static ScheduledExecutorService newScheduledThreadPool(String threadPreffix, int corePoolSize) {
        BasicThreadFactory factory = new BasicThreadFactory.Builder()
                .namingPattern("anahata-asi-" + threadPreffix + "-scheduled-%d")
                .daemon(true)
                .priority(Thread.NORM_PRIORITY)
                .build();
        return Executors.newScheduledThreadPool(corePoolSize, factory);
    }
}
