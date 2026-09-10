/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.toolkit;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SystemUtils;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;

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
@AgiToolkit("A toolkit for host-level operations and system information.")
public class Host extends AnahataToolkit {

    /** {@inheritDoc} */
    @Override
    public void populateMessage(RagMessage ragMessage) {
        // System identity, CPU cores, and JVM/Host memory are centrally provided by CoreContextProvider.
    }

    /**
     * Lists all active processes on the host system.
     * 
     * @return A list of process information strings.
     */
    @AgiTool("Lists all active processes on the host system.")
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
    @AgiTool("Terminates a specific process by its PID.")
    public String killProcess(
            @AgiToolParam("The process ID to terminate.") long pid,
            @AgiToolParam("Whether to use a forced shutdown.") boolean force
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
