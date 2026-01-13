/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.concurrent.BasicThreadFactory;

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
}
