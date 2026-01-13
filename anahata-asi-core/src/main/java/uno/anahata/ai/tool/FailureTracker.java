/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;

/**
 * Tracks repeated failures of specific tool calls to temporarily block them
 * from execution, preventing infinite loops or excessive resource consumption
 * when a model repeatedly calls a broken function.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@RequiredArgsConstructor
public class FailureTracker {
    private static final int MAX_FAILURES = 3;
    private static final long BLOCK_DURATION_MS = 5 * 60 * 1000; // 5 minutes

    @NonNull
    private final Chat chat;

    private final Map<String, FailureInfo> failureMap = new ConcurrentHashMap<>();

    /**
     * Records a failure for a given tool call.
     * @param toolName The name of the tool that failed.
     * @param exception The exception that occurred.
     */
    public void recordFailure(String toolName, Throwable exception) {
        failureMap.compute(toolName, (key, info) -> {
            if (info == null) {
                info = new FailureInfo(toolName);
            }
            info.recordFailure();
            
            if (info.getFailureCount() >= MAX_FAILURES) {
                info.setBlockedUntil(Instant.now().toEpochMilli() + BLOCK_DURATION_MS);
                log.error("Tool '{}' has failed {} times and is now blocked until {}.", 
                          toolName, MAX_FAILURES, Instant.ofEpochMilli(info.getBlockedUntil()));
            }
            return info;
        });
    }

    /**
     * Checks if a tool is currently blocked from execution.
     * @param toolName The name of the tool to check.
     * @return true if the tool is blocked, false otherwise.
     */
    public boolean isBlocked(String toolName) {
        FailureInfo info = failureMap.get(toolName);
        if (info == null) {
            return false;
        }
        
        if (info.isBlocked()) {
            if (info.getBlockedUntil() > Instant.now().toEpochMilli()) {
                log.warn("Tool '{}' is currently blocked until {}.", toolName, Instant.ofEpochMilli(info.getBlockedUntil()));
                return true;
            } else {
                // Block time expired, reset the tracker
                failureMap.remove(toolName);
                return false;
            }
        }
        return false;
    }

    @Getter
    private static class FailureInfo {
        private final String toolName;
        private int failureCount = 0;
        private long blockedUntil = 0; // Epoch milliseconds

        public FailureInfo(String toolName) {
            this.toolName = toolName;
        }

        public void recordFailure() {
            this.failureCount++;
        }

        public boolean isBlocked() {
            return blockedUntil > 0;
        }

        public void setBlockedUntil(long blockedUntil) {
            this.blockedUntil = blockedUntil;
        }
    }
}
