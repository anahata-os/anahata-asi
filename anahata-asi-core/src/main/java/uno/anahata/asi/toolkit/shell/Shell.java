/* Licensed under the Apache License, Version 2.0 */
package uno.anahata.asi.toolkit.shell;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import lombok.AllArgsConstructor;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.tool.java.JavaMethodToolResponse;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A tool provider that allows the AI model to execute commands in the local
 * shell (bash on Unix, cmd.exe on Windows).
 * <p>
 * This tool is powerful and should be used with caution. It captures standard
 * output, standard error, and execution metadata.
 * </p>
 * 
 * @author anahata
 */
@AiToolkit("A toolkit for running shell commands")
public class Shell extends AnahataToolkit {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions(Chat chat) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append("### Host Environment Variables\n");
        Map<String, String> sortedEnv = new TreeMap<>(System.getenv());
        sortedEnv.forEach((k, v) -> sb.append("- **").append(k).append("**: ").append(v).append("\n"));
        return List.of(sb.toString());
    }

    /**
     * Executes a shell command using the appropriate system shell.
     *
     * @param command The shell command to execute.
     * @return A {@link ShellExecutionResult} containing the exit code and output.
     * @throws Exception if the command fails to start or execution is interrupted.
     */
    @AiTool("Runs a shell command using the system's default shell (cmd.exe on Windows, bash on Unix) and forwards the stdout to the tool's output and the stderr to the tool's error log")
    public ShellExecutionResult runAndWait(@AiToolParam("The command to run") String command) throws Exception {
        ShellExecutionResult result = new ShellExecutionResult();

        String os = System.getProperty("os.name").toLowerCase();
        ProcessBuilder pb;
        if (os.contains("win")) {
            pb = new ProcessBuilder("cmd.exe", "/c", command);
        } else {
            pb = new ProcessBuilder("bash", "-c", command);
        }
        
        pb.redirectErrorStream(false);

        Process process = pb.start();
        log("Process started: " + process);

        String pid = "unknown";
        try {
            pid = "" + process.pid();
        } catch (UnsupportedOperationException e) {
            log("Could not get process id: " + e);
            // PID not available on some JVMs
        }

        ExecutorService executor = getChat().getExecutor();
        log("Using executor service: " + executor);
        JavaMethodToolResponse response = getResponse();
        Future<String> stdoutFuture = executor.submit(new StreamGobbler(response, process.getInputStream(), false));
        log("Submitted output gobbler to executor service: " + stdoutFuture);
        Future<String> stderrFuture = executor.submit(new StreamGobbler(response, process.getErrorStream(), true));
        log("Submitted error gobbler to executor service: " + stderrFuture);

        log("Calling process.waitFor()");
        int exitCode = process.waitFor();
        log("Process exited with exitCode: " + exitCode);

        String output = stdoutFuture.get();
        String error = stderrFuture.get();

        result.setProcessToString(process.toString());
        result.setProcessId(pid);
        result.setExitCode(exitCode);
        result.setStdOut(output);
        //result.setStdErr(error);

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
            return sb.toString();
        }
    }
}
