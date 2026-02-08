/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.toolkit;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SystemUtils;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for interacting with the host operating system and JVM.
 * <p>
 * This toolkit provides tools for process management, system information, 
 * and environment introspection.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for host-level operations and system information.")
public class Host extends AnahataToolkit {

    /** {@inheritDoc} */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        StringBuilder sb = new StringBuilder("### Host System\n");
        sb.append("- **OS**: ").append(SystemUtils.OS_NAME).append(" (").append(SystemUtils.OS_VERSION).append(")\n");
        sb.append("- **Architecture**: ").append(SystemUtils.OS_ARCH).append("\n");
        sb.append("- **Java Version**: ").append(SystemUtils.JAVA_VERSION).append("\n");
        sb.append("- **Available Processors**: ").append(Runtime.getRuntime().availableProcessors()).append("\n");
        
        long maxMemory = Runtime.getRuntime().maxMemory();
        long totalMemory = Runtime.getRuntime().totalMemory();
        long freeMemory = Runtime.getRuntime().freeMemory();
        long usedMemory = totalMemory - freeMemory;
        
        sb.append("- **Memory (JVM)**: ")
          .append(usedMemory / 1024 / 1024).append("MB used / ")
          .append(totalMemory / 1024 / 1024).append("MB total / ")
          .append(maxMemory / 1024 / 1024).append("MB max\n");
          
        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Lists all active processes on the host system.
     * 
     * @return A list of process information strings.
     */
    @AiTool("Lists all active processes on the host system.")
    public List<String> listProcesses() {
        List<String> results = new ArrayList<>();
        ProcessHandle.allProcesses().forEach(p -> {
            StringBuilder sb = new StringBuilder();
            sb.append("PID: ").append(p.pid());
            p.info().command().ifPresent(c -> sb.append(" [Cmd: ").append(c).append("]"));
            p.info().user().ifPresent(u -> sb.append(" [User: ").append(u).append("]"));
            results.add(sb.toString());
        });
        return results;
    }

    /**
     * Terminates a specific process by its PID.
     * 
     * @param pid The process ID to terminate.
     * @param force Whether to use a forced shutdown (kill -9).
     * @return A status message.
     */
    @AiTool("Terminates a specific process by its PID.")
    public String killProcess(
            @AiToolParam("The process ID to terminate.") long pid,
            @AiToolParam("Whether to use a forced shutdown.") boolean force
    ) {
        long myPid = ProcessHandle.current().pid();
        if (pid == myPid) {
            return "Error: Cannot kill the current process (PID " + pid + ").";
        }

        Optional<ProcessHandle> handle = ProcessHandle.of(pid);
        if (handle.isPresent()) {
            ProcessHandle p = handle.get();
            boolean success = force ? p.destroyForcibly() : p.destroy();
            return success ? "Termination signal sent to PID " + pid : "Failed to terminate PID " + pid;
        }
        return "Process with PID " + pid + " not found.";
    }
}
