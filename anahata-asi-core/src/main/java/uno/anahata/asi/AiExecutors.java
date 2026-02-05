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
 * Centralized factory for creating named, daemon thread pools used by the Anahata ASI framework.
 * 
 * @author anahata
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class AiExecutors {

    /**
     * Creates a new cached thread pool specifically for managing the lifecycle of a single chat session.
     * Threads created by this executor are daemon threads to prevent them from blocking application shutdown.
     *
     * @param threadPreffix The unique identifier for the chat session, used in the thread name.
     * @return A new cached thread pool ExecutorService.
     */
    public static ExecutorService newCachedThreadPoolExecutor(String threadPreffix) {
        BasicThreadFactory factory = new BasicThreadFactory.Builder()
                .namingPattern("anahata-ai-" + threadPreffix + "-thread-%d")
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
                .namingPattern("anahata-ai-" + threadPreffix + "-scheduled-%d")
                .daemon(true)
                .priority(Thread.NORM_PRIORITY)
                .build();
        return Executors.newScheduledThreadPool(corePoolSize, factory);
    }
}
