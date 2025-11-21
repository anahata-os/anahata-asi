/*
 * Copyright 2025 Anahata.
 *
 * Licensed under the Anahata Software License (ASL) V2.0;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://github.com/pablo-anahata/anahata-ai-parent/blob/main/LICENSE
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Força Barça!
 */
package uno.anahata.ai;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.concurrent.BasicThreadFactory;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class AnahataExecutors {

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
