/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.toolkit.shell;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import lombok.AllArgsConstructor;
import lombok.Getter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.model.tool.java.JavaMethodToolResponse;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A tool provider that allows the AI model to execute commands in the local
 * shell. It supports Bash (Unix), CMD (Windows), and PowerShell (Windows).
 * <p>
 * This tool is powerful and should be used with caution. It captures standard
 * output, standard error, and execution metadata.
 * </p>
 * 
 * @author anahata
 */
@AiToolkit("A toolkit for running shell commands")
public class Shell extends AnahataToolkit {

    /**
     * Defines the supported shell types.
     */
    public enum ShellType {
        /** Bourne-again shell (Unix/Linux/macOS). */
        BASH,
        /** Windows Command Prompt (Legacy). */
        CMD,
        /** Windows PowerShell (Modern, Object-Oriented). */
        POWERSHELL,
        /** Standard Unix shell. */
        SH
    }

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append(" Host Environment Variables\n");
        Map<String, String> sortedEnv = new TreeMap<>(System.getenv());
        sortedEnv.forEach((k, v) -> sb.append("- **").append(k).append("**: ").append(v).append("\n"));
        return List.of(sb.toString());
    }

    /**
     * Executes a shell command using the appropriate system shell.
     *
     * @param command The shell command to execute.
     * @param type The type of shell to use. If null, it will be auto-detected based on the OS.
     * @return A {@link ShellExecutionResult} containing the exit code and output.
     * @throws Exception if the command fails to start or execution is interrupted.
     */
    @AiTool("Runs a shell command using the specified or auto-detected shell and forwards the stdout to the tool's output and the stderr to the tool's error log")
    public ShellExecutionResult runAndWait(
            @AiToolParam("The command to run") String command,
            @AiToolParam("The type of shell to use (BASH, CMD, POWERSHELL, SH). If null, it defaults to POWERSHELL on Windows and BASH on Unix.") ShellType type) throws Exception {
        
        Thread currentThread = Thread.currentThread();
        log(String.format("[Shell] runAndWait started on thread: %s (ID: %d)", currentThread.getName(), currentThread.getId()));

        ShellExecutionResult result = new ShellExecutionResult();
        String os = System.getProperty("os.name").toLowerCase();
        boolean isWindows = os.contains("win");

        ShellType effectiveType = type;
        if (effectiveType == null) {
            effectiveType = isWindows ? ShellType.POWERSHELL : ShellType.BASH;
        }

        List<String> pbArgs = new ArrayList<>();
        switch (effectiveType) {
            case POWERSHELL -> {
                pbArgs.add("powershell.exe");
                pbArgs.add("-Command");
                pbArgs.add(command);
            }
            case CMD -> {
                pbArgs.add("cmd.exe");
                pbArgs.add("/c");
                pbArgs.add(command);
            }
            case BASH -> {
                pbArgs.add("bash");
                pbArgs.add("-c");
                pbArgs.add(command);
            }
            case SH -> {
                pbArgs.add("sh");
                pbArgs.add("-c");
                pbArgs.add(command);
            }
        }

        ProcessBuilder pb = new ProcessBuilder(pbArgs);
        pb.redirectErrorStream(false);

        Process process = pb.start();
        log("Process started (" + effectiveType + "): " + process);

        String pid = "unknown";
        try {
            pid = "" + process.pid();
        } catch (UnsupportedOperationException e) {
            log("Could not get process id: " + e);
        }

        ExecutorService executor = getAgi().getExecutor();
        JavaMethodToolResponse response = getResponse();
        
        Future<String> stdoutFuture = executor.submit(new StreamGobbler(response, process.getInputStream(), false));
        Future<String> stderrFuture = executor.submit(new StreamGobbler(response, process.getErrorStream(), true));

        int exitCode = process.waitFor();
        log("Process exited with exitCode: " + exitCode);

        String output = stdoutFuture.get();
        String error = stderrFuture.get();

        result.setProcessToString(process.toString());
        result.setProcessId(pid);
        result.setExitCode(exitCode);
        result.setStdOut(output);

        return result;
    }

    /**
     * A background task that consumes an InputStream and forwards its content 
     * to the tool's log or error stream.
     */
    @AllArgsConstructor
    private class StreamGobbler implements Callable<String> {

        /** The tool response to which logs will be added. */
        private JavaMethodToolResponse response;
        /** The input stream to consume. */
        private final InputStream is;
        /** Whether this stream represents standard error. */
        private boolean error;

        /** {@inheritDoc} */
        @Override
        public String call() throws IOException {
            Thread currentThread = Thread.currentThread();
            String streamName = error ? "STDERR" : "STDOUT";
            response.addLog(String.format("[Gobbler-%s] Started on thread: %s (ID: %d)", streamName, currentThread.getName(), currentThread.getId()));

            StringBuilder sb = new StringBuilder();
            try (InputStream in = is) {
                byte[] buffer = new byte[1024];
                int len;
                while ((len = in.read(buffer)) != -1) {
                    String line = new String(buffer, 0, len, StandardCharsets.UTF_8);
                    if (error) {
                        response.addError(line);
                    } else {
                        //response.addLog(line);
                    }
                    sb.append(line);
                }
            }
            response.addLog(String.format("[Gobbler-%s] Finished.", streamName));
            return sb.toString();
        }
    }
}
