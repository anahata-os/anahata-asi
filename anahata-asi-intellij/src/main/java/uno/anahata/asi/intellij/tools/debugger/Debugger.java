/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.tools.debugger;

import com.intellij.execution.Executor;
import com.intellij.execution.ProgramRunnerUtil;
import com.intellij.execution.RunManager;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.messages.MessageBusConnection;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebugSessionListener;
import com.intellij.xdebugger.XDebuggerManager;
import com.intellij.xdebugger.XDebuggerManagerListener;
import com.intellij.xdebugger.XExpression;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.breakpoints.XBreakpoint;
import com.intellij.xdebugger.breakpoints.XBreakpointManager;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import com.intellij.xdebugger.breakpoints.XLineBreakpointType;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.XCompositeNode;
import com.intellij.xdebugger.frame.XFullValueEvaluator;
import com.intellij.xdebugger.frame.XStackFrame;
import com.intellij.xdebugger.frame.XValue;
import com.intellij.xdebugger.frame.XValueChildrenList;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.XValuePlace;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

import javax.swing.Icon;
import java.nio.file.Path;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * A toolkit for driving the IntelliJ debugger (breakpoints, debug sessions, expression evaluation).
 * <p>
 * The IntelliJ counterpart of the NetBeans debugger integration. It is built entirely on the
 * platform-agnostic {@code com.intellij.xdebugger} API rather than the language-specific
 * (Java/Kotlin) debugger implementations, mirroring the design of the {@code Gradle} toolkit: line
 * breakpoints are created against whichever registered {@link XLineBreakpointType} accepts a given
 * location, so the toolkit works for any JVM language the running IDE supports without compiling
 * against those plugins' classes. Because {@code xdebugger} lives in
 * {@code com.intellij.modules.platform} and concrete breakpoint types are contributed by
 * {@code com.intellij.modules.java} (both already declared in {@code plugin.xml}), no additional
 * plugin dependency is required.
 * </p>
 * <p>
 * The XDebugger frame/value APIs are asynchronous (results arrive on the debugger's own threads via
 * callbacks); the evaluation and variable-inspection tools bridge that back to the synchronous
 * tool-call model with bounded {@link CountDownLatch} waits, so a hung debuggee cannot block a tool
 * call indefinitely.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A toolkit for managing breakpoints, debug sessions and expression evaluation in the IntelliJ debugger.")
public class Debugger extends AnahataToolkit {

    /**
     * The default bounded wait, in seconds, for asynchronous debugger operations.
     */
    private static final int DEFAULT_TIMEOUT_SECONDS = 30;

    /**
     * Constructs the Debugger toolkit (instantiated reflectively via its public no-arg constructor).
     */
    public Debugger() {
    }

    // ------------------------------------------------------------------------------------------------
    // Breakpoints
    // ------------------------------------------------------------------------------------------------

    /**
     * Lists every line breakpoint currently configured across all open projects.
     *
     * @return a Markdown listing of line breakpoints (file, line, enabled state, condition).
     */
    @AgiTool("Lists all line breakpoints configured across open projects.")
    public String listBreakpoints() {
        StringBuilder sb = new StringBuilder("## Breakpoints\n");
        boolean any = false;
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            XBreakpointManager mgr = XDebuggerManager.getInstance(project).getBreakpointManager();
            for (XBreakpoint<?> bp : mgr.getAllBreakpoints()) {
                if (bp instanceof XLineBreakpoint<?> line) {
                    any = true;
                    XExpression condition = line.getConditionExpression();
                    sb.append("- `").append(line.getPresentableFilePath()).append(":").append(line.getLine() + 1)
                            .append("` [").append(line.getType().getTitle()).append("]")
                            .append(line.isEnabled() ? "" : " (disabled)")
                            .append(condition != null ? " when `" + condition.getExpression() + "`" : "")
                            .append(" (").append(project.getName()).append(")\n");
                }
            }
        }
        return any ? sb.toString() : "No line breakpoints are configured in any open project.";
    }

    /**
     * Sets a line breakpoint at the given source location.
     *
     * @param filePath the absolute path of the source file.
     * @param line     the 1-based line number on which to place the breakpoint.
     * @return a confirmation message, or a message explaining why no breakpoint could be placed.
     * @throws AgiToolException if the file cannot be resolved or no project hosts it.
     */
    @AgiTool("Sets a line breakpoint at the given file and 1-based line number.")
    public String setLineBreakpoint(
            @AgiToolParam("The absolute path of the source file.") String filePath,
            @AgiToolParam("The 1-based line number for the breakpoint.") int line) throws AgiToolException {

        VirtualFile file = resolveFile(filePath);
        Project project = resolveProjectForFile(file);
        int zeroLine = line - 1;
        AtomicReference<String> result = new AtomicReference<>();

        ApplicationManager.getApplication().invokeAndWait(() -> {
            XBreakpointManager mgr = XDebuggerManager.getInstance(project).getBreakpointManager();
            XLineBreakpointType<?> type = findLineBreakpointType(file, zeroLine, project);
            if (type == null) {
                result.set("No breakpoint can be placed at " + filePath + ":" + line
                        + " (no registered breakpoint type accepts this location).");
                return;
            }
            addLineBreakpoint(mgr, type, file, zeroLine);
            result.set("Set breakpoint at `" + filePath + ":" + line + "` [" + type.getTitle() + "].");
        });

        String message = result.get();
        log(message);
        return message;
    }

    /**
     * Removes any line breakpoint at the given source location.
     *
     * @param filePath the absolute path of the source file.
     * @param line     the 1-based line number.
     * @return a confirmation message reporting how many breakpoints were removed.
     * @throws AgiToolException if the file cannot be resolved or no project hosts it.
     */
    @AgiTool("Removes any line breakpoint at the given file and 1-based line number.")
    public String removeLineBreakpoint(
            @AgiToolParam("The absolute path of the source file.") String filePath,
            @AgiToolParam("The 1-based line number of the breakpoint to remove.") int line) throws AgiToolException {

        VirtualFile file = resolveFile(filePath);
        Project project = resolveProjectForFile(file);
        int zeroLine = line - 1;
        String fileUrl = file.getUrl();
        AtomicReference<Integer> removed = new AtomicReference<>(0);

        ApplicationManager.getApplication().invokeAndWait(() -> {
            XBreakpointManager mgr = XDebuggerManager.getInstance(project).getBreakpointManager();
            int count = 0;
            for (XBreakpoint<?> bp : mgr.getAllBreakpoints()) {
                if (bp instanceof XLineBreakpoint<?> lineBp
                        && lineBp.getLine() == zeroLine
                        && fileUrl.equals(lineBp.getFileUrl())) {
                    mgr.removeBreakpoint(lineBp);
                    count++;
                }
            }
            removed.set(count);
        });

        String message = "Removed " + removed.get() + " breakpoint(s) at `" + filePath + ":" + line + "`.";
        log(message);
        return message;
    }

    /**
     * Removes all line breakpoints across all open projects.
     *
     * @return a confirmation message reporting how many breakpoints were removed.
     */
    @AgiTool("Removes all line breakpoints across open projects.")
    public String clearAllBreakpoints() {
        AtomicReference<Integer> removed = new AtomicReference<>(0);
        ApplicationManager.getApplication().invokeAndWait(() -> {
            int count = 0;
            for (Project project : ProjectManager.getInstance().getOpenProjects()) {
                XBreakpointManager mgr = XDebuggerManager.getInstance(project).getBreakpointManager();
                for (XBreakpoint<?> bp : mgr.getAllBreakpoints()) {
                    if (bp instanceof XLineBreakpoint<?> lineBp) {
                        mgr.removeBreakpoint(lineBp);
                        count++;
                    }
                }
            }
            removed.set(count);
        });
        String message = "Cleared " + removed.get() + " line breakpoint(s).";
        log(message);
        return message;
    }

    // ------------------------------------------------------------------------------------------------
    // Sessions & launch
    // ------------------------------------------------------------------------------------------------

    /**
     * Lists the active debug sessions across all open projects and their current state.
     *
     * @return a Markdown listing of active debug sessions.
     */
    @AgiTool("Lists the active debug sessions and their current state (paused/running, current location).")
    public String listDebugSessions() {
        StringBuilder sb = new StringBuilder("## Debug Sessions\n");
        boolean any = false;
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            XDebuggerManager manager = XDebuggerManager.getInstance(project);
            XDebugSession current = manager.getCurrentSession();
            for (XDebugSession session : manager.getDebugSessions()) {
                any = true;
                sb.append("- **").append(session.getSessionName()).append("**")
                        .append(session == current ? " (current)" : "")
                        .append(" — ").append(describeState(session))
                        .append(" (").append(project.getName()).append(")\n");
            }
        }
        return any ? sb.toString() : "No active debug sessions.";
    }

    /**
     * Launches a run configuration under the debugger and (optionally) waits for it to pause or stop.
     *
     * @param name           the exact run configuration name.
     * @param timeoutSeconds optional bounded wait, in seconds, for the session to pause at a
     *                       breakpoint or terminate; {@code 0} or {@code null} launches asynchronously.
     * @return a structured summary of the debug launch outcome.
     * @throws AgiToolException if no configuration with that name exists or the wait is interrupted.
     */
    @AgiTool("Launches a run configuration under the debugger; optionally waits for it to hit a breakpoint or terminate.")
    public String debugRunConfiguration(
            @AgiToolParam("The exact name of the run configuration to debug.") String name,
            @AgiToolParam(value = "Optional bounded wait in seconds for the session to pause or stop. 0 or null launches asynchronously.", required = false) Integer timeoutSeconds) throws AgiToolException {

        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            RunnerAndConfigurationSettings settings = RunManager.getInstance(project).findConfigurationByName(name);
            if (settings != null) {
                return debugConfiguration(project, settings, timeoutSeconds);
            }
        }
        throw new AgiToolException("No run configuration named: " + name);
    }

    /**
     * Debugs a resolved configuration, optionally bridging the asynchronous session lifecycle
     * (start → pause/stop) back to a bounded synchronous wait.
     *
     * @param project        the host project.
     * @param settings       the configuration to debug.
     * @param timeoutSeconds optional bounded wait in seconds; {@code 0}/{@code null} is async.
     * @return a structured launch/outcome summary.
     * @throws AgiToolException if the wait is interrupted.
     */
    private String debugConfiguration(Project project, RunnerAndConfigurationSettings settings, Integer timeoutSeconds) throws AgiToolException {
        Executor debugExecutor = DefaultDebugExecutor.getDebugExecutorInstance();

        if (timeoutSeconds == null || timeoutSeconds <= 0) {
            ApplicationManager.getApplication().invokeAndWait(() ->
                    ProgramRunnerUtil.executeConfiguration(settings, debugExecutor));
            log("Launched debug configuration: " + settings.getName());
            return "Launched '" + settings.getName() + "' under the debugger (asynchronous).";
        }

        CountDownLatch settledLatch = new CountDownLatch(1);
        AtomicReference<XDebugSession> sessionRef = new AtomicReference<>();
        AtomicReference<String> outcome = new AtomicReference<>();

        MessageBusConnection connection = project.getMessageBus().connect();
        try {
            connection.subscribe(XDebuggerManager.TOPIC, new XDebuggerManagerListener() {
                @Override
                public void processStarted(XDebugProcess debugProcess) {
                    XDebugSession session = debugProcess.getSession();
                    sessionRef.set(session);
                    session.addSessionListener(new XDebugSessionListener() {
                        @Override
                        public void sessionPaused() {
                            outcome.compareAndSet(null, "PAUSED");
                            settledLatch.countDown();
                        }

                        @Override
                        public void sessionStopped() {
                            outcome.compareAndSet(null, "STOPPED");
                            settledLatch.countDown();
                        }
                    });
                }
            });

            ApplicationManager.getApplication().invokeAndWait(() ->
                    ProgramRunnerUtil.executeConfiguration(settings, debugExecutor));
            log("Awaiting debug session for '" + settings.getName() + "' (timeout: " + timeoutSeconds + "s)...");

            boolean settled;
            try {
                settled = settledLatch.await(timeoutSeconds, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new AgiToolException("Interrupted while waiting for debug session: " + settings.getName());
            }

            StringBuilder result = new StringBuilder("## Debug Launch: ").append(settings.getName()).append("\n");
            result.append("- **Project**: ").append(project.getName()).append("\n");
            if (!settled) {
                result.append("- **State**: still running after ").append(timeoutSeconds)
                        .append("s (no breakpoint hit yet).\n");
                return result.toString();
            }
            XDebugSession session = sessionRef.get();
            result.append("- **State**: ").append("PAUSED".equals(outcome.get()) ? "paused at a breakpoint" : "terminated").append("\n");
            if (session != null && "PAUSED".equals(outcome.get())) {
                result.append("- **Location**: ").append(positionOf(session.getCurrentPosition())).append("\n");
            }
            String summary = result.toString();
            log(summary);
            return summary;
        } finally {
            connection.disconnect();
        }
    }

    /**
     * Resumes the current (paused) debug session.
     *
     * @return a confirmation message.
     * @throws AgiToolException if there is no current debug session.
     */
    @AgiTool("Resumes the current paused debug session.")
    public String resumeDebugSession() throws AgiToolException {
        XDebugSession session = requireCurrentSession();
        ApplicationManager.getApplication().invokeAndWait(session::resume);
        return "Resumed debug session '" + session.getSessionName() + "'.";
    }

    /**
     * Steps over the current line in the current (paused) debug session.
     *
     * @return a confirmation message.
     * @throws AgiToolException if there is no current debug session.
     */
    @AgiTool("Steps over the current line in the current debug session.")
    public String stepOver() throws AgiToolException {
        XDebugSession session = requireCurrentSession();
        ApplicationManager.getApplication().invokeAndWait(() -> session.stepOver(false));
        return "Stepped over in '" + session.getSessionName() + "'.";
    }

    /**
     * Steps into the call at the current line in the current (paused) debug session.
     *
     * @return a confirmation message.
     * @throws AgiToolException if there is no current debug session.
     */
    @AgiTool("Steps into the call at the current line in the current debug session.")
    public String stepInto() throws AgiToolException {
        XDebugSession session = requireCurrentSession();
        ApplicationManager.getApplication().invokeAndWait(session::stepInto);
        return "Stepped into in '" + session.getSessionName() + "'.";
    }

    /**
     * Steps out of the current method in the current (paused) debug session.
     *
     * @return a confirmation message.
     * @throws AgiToolException if there is no current debug session.
     */
    @AgiTool("Steps out of the current method in the current debug session.")
    public String stepOut() throws AgiToolException {
        XDebugSession session = requireCurrentSession();
        ApplicationManager.getApplication().invokeAndWait(session::stepOut);
        return "Stepped out in '" + session.getSessionName() + "'.";
    }

    /**
     * Stops (terminates) the current debug session.
     *
     * @return a confirmation message.
     * @throws AgiToolException if there is no current debug session.
     */
    @AgiTool("Stops the current debug session.")
    public String stopDebugSession() throws AgiToolException {
        XDebugSession session = requireCurrentSession();
        ApplicationManager.getApplication().invokeAndWait(session::stop);
        return "Stopped debug session '" + session.getSessionName() + "'.";
    }

    // ------------------------------------------------------------------------------------------------
    // Evaluation & variables (asynchronous → bounded synchronous)
    // ------------------------------------------------------------------------------------------------

    /**
     * Evaluates an expression in the top frame of the current (paused) debug session.
     *
     * @param expression     the expression to evaluate, in the debuggee's language.
     * @param timeoutSeconds optional bounded wait in seconds (default 30).
     * @return the evaluated value's presentation, or an error message from the evaluator.
     * @throws AgiToolException if there is no suspended session, no evaluator, or the wait times out.
     */
    @AgiTool("Evaluates an expression in the top frame of the current paused debug session.")
    public String evaluateExpression(
            @AgiToolParam("The expression to evaluate, in the debuggee's language.") String expression,
            @AgiToolParam(value = "Optional bounded wait in seconds (default 30).", required = false) Integer timeoutSeconds) throws AgiToolException {

        XDebugSession session = requireSuspendedSession();
        XStackFrame frame = session.getCurrentStackFrame();
        if (frame == null) {
            throw new AgiToolException("The current debug session has no active stack frame to evaluate in.");
        }
        XDebuggerEvaluator evaluator = frame.getEvaluator();
        if (evaluator == null) {
            throw new AgiToolException("The current stack frame does not support expression evaluation.");
        }

        CountDownLatch latch = new CountDownLatch(1);
        AtomicReference<String> value = new AtomicReference<>();
        AtomicReference<String> error = new AtomicReference<>();

        evaluator.evaluate(expression, new XDebuggerEvaluator.XEvaluationCallback() {
            @Override
            public void evaluated(XValue result) {
                result.computePresentation(new CapturingValueNode(value, latch), XValuePlace.TREE);
            }

            @Override
            public void errorOccurred(String errorMessage) {
                error.set(errorMessage);
                latch.countDown();
            }
        }, frame.getSourcePosition());

        awaitLatch(latch, timeoutSeconds, "evaluate '" + expression + "'");
        if (error.get() != null) {
            return "Evaluation error: " + error.get();
        }
        return "`" + expression + "` = " + value.get();
    }

    /**
     * Lists the variables visible in the top frame of the current (paused) debug session.
     *
     * @param timeoutSeconds optional bounded wait in seconds (default 30).
     * @return a Markdown listing of {@code name = value} pairs for the top-frame variables.
     * @throws AgiToolException if there is no suspended session or active stack frame, or the wait
     *                          times out.
     */
    @AgiTool("Lists the variables (name and value) visible in the top frame of the current paused debug session.")
    public String listFrameVariables(
            @AgiToolParam(value = "Optional bounded wait in seconds (default 30).", required = false) Integer timeoutSeconds) throws AgiToolException {

        XDebugSession session = requireSuspendedSession();
        XStackFrame frame = session.getCurrentStackFrame();
        if (frame == null) {
            throw new AgiToolException("The current debug session has no active stack frame.");
        }

        CountDownLatch latch = new CountDownLatch(1);
        StringBuilder sb = new StringBuilder("## Frame Variables\n");
        AtomicReference<String> error = new AtomicReference<>();

        frame.computeChildren(new CollectingCompositeNode(sb, error, latch, timeoutSeconds));

        awaitLatch(latch, timeoutSeconds, "list frame variables");
        if (error.get() != null) {
            return "Could not compute variables: " + error.get();
        }
        return sb.length() > "## Frame Variables\n".length() ? sb.toString() : "No variables are visible in the top frame.";
    }

    // ------------------------------------------------------------------------------------------------
    // Internal helpers
    // ------------------------------------------------------------------------------------------------

    /**
     * Adds a line breakpoint, capturing the breakpoint type's property generic so the
     * wildcard-typed {@link XLineBreakpointType} can be passed to
     * {@link XBreakpointManager#addLineBreakpoint}.
     *
     * @param <P>      the breakpoint properties type of {@code type}.
     * @param mgr      the breakpoint manager.
     * @param type     the (capture of the) line breakpoint type.
     * @param file     the source file.
     * @param zeroLine the 0-based line number.
     * @return the created breakpoint.
     */
    private static <P extends XBreakpointProperties> XLineBreakpoint<P> addLineBreakpoint(
            XBreakpointManager mgr, XLineBreakpointType<P> type, VirtualFile file, int zeroLine) {
        return mgr.addLineBreakpoint(type, file.getUrl(), zeroLine, type.createBreakpointProperties(file, zeroLine));
    }

    /**
     * Finds the first registered line breakpoint type that accepts the given location.
     *
     * @param file     the source file.
     * @param zeroLine the 0-based line number.
     * @param project  the host project.
     * @return an accepting breakpoint type, or {@code null} if none can be placed there.
     */
    private static XLineBreakpointType<?> findLineBreakpointType(VirtualFile file, int zeroLine, Project project) {
        for (XLineBreakpointType<?> type : com.intellij.xdebugger.XDebuggerUtil.getInstance().getLineBreakpointTypes()) {
            if (type.canPutAt(file, zeroLine, project)) {
                return type;
            }
        }
        return null;
    }

    /**
     * Resolves a source file path to a {@link VirtualFile}, refreshing the VFS if necessary.
     *
     * @param filePath the absolute file path.
     * @return the resolved virtual file.
     * @throws AgiToolException if the file does not exist.
     */
    private VirtualFile resolveFile(String filePath) throws AgiToolException {
        String normalized = filePath.replace('\\', '/');
        VirtualFile file = LocalFileSystem.getInstance().findFileByPath(normalized);
        if (file == null) {
            file = LocalFileSystem.getInstance().refreshAndFindFileByPath(normalized);
        }
        if (file == null) {
            throw new AgiToolException("File not found: " + filePath);
        }
        return file;
    }

    /**
     * Resolves the open project best associated with a file (content-root prefix match, else the
     * first open project).
     *
     * @param file the source file.
     * @return the hosting project.
     * @throws AgiToolException if no project is open.
     */
    private Project resolveProjectForFile(VirtualFile file) throws AgiToolException {
        Project[] open = ProjectManager.getInstance().getOpenProjects();
        String target = file.getPath();
        for (Project project : open) {
            String basePath = project.getBasePath();
            if (basePath != null && target.startsWith(Path.of(basePath).toAbsolutePath().toString())) {
                return project;
            }
        }
        if (open.length > 0) {
            return open[0];
        }
        throw new AgiToolException("No open IntelliJ project to host the breakpoint for: " + file.getPath());
    }

    /**
     * Returns the current debug session of the first open project that has one.
     *
     * @return the current debug session.
     * @throws AgiToolException if there is no current debug session in any open project.
     */
    private XDebugSession requireCurrentSession() throws AgiToolException {
        for (Project project : ProjectManager.getInstance().getOpenProjects()) {
            XDebugSession session = XDebuggerManager.getInstance(project).getCurrentSession();
            if (session != null) {
                return session;
            }
        }
        throw new AgiToolException("There is no active debug session.");
    }

    /**
     * Returns the current debug session, requiring it to be suspended (paused).
     *
     * @return the suspended current debug session.
     * @throws AgiToolException if there is no current session or it is not suspended.
     */
    private XDebugSession requireSuspendedSession() throws AgiToolException {
        XDebugSession session = requireCurrentSession();
        if (!session.isSuspended()) {
            throw new AgiToolException("The current debug session '" + session.getSessionName()
                    + "' is running; pause it (or hit a breakpoint) before evaluating.");
        }
        return session;
    }

    /**
     * Waits on a latch for a bounded time, mapping timeout/interruption to an {@link AgiToolException}.
     *
     * @param latch          the latch to await.
     * @param timeoutSeconds the optional timeout in seconds (default 30).
     * @param what           a short description of the awaited operation, for error messages.
     * @throws AgiToolException if the wait times out or is interrupted.
     */
    private static void awaitLatch(CountDownLatch latch, Integer timeoutSeconds, String what) throws AgiToolException {
        int timeout = timeoutSeconds != null && timeoutSeconds > 0 ? timeoutSeconds : DEFAULT_TIMEOUT_SECONDS;
        try {
            if (!latch.await(timeout, TimeUnit.SECONDS)) {
                throw new AgiToolException("Timed out after " + timeout + "s waiting to " + what + ".");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new AgiToolException("Interrupted while waiting to " + what + ".");
        }
    }

    /**
     * Describes a debug session's runtime state compactly.
     *
     * @param session the debug session.
     * @return a short human-readable state description.
     */
    private static String describeState(XDebugSession session) {
        if (session.isStopped()) {
            return "stopped";
        }
        if (session.isPaused() || session.isSuspended()) {
            return "paused at " + positionOf(session.getCurrentPosition());
        }
        return "running";
    }

    /**
     * Formats a source position as {@code file:line} (1-based), or {@code "unknown"} if null.
     *
     * @param position the source position, possibly {@code null}.
     * @return a compact {@code file:line} description.
     */
    private static String positionOf(XSourcePosition position) {
        if (position == null) {
            return "unknown";
        }
        return position.getFile().getName() + ":" + (position.getLine() + 1);
    }

    /**
     * An {@link XValueNode} that captures a value's textual presentation into an
     * {@link AtomicReference} and releases a latch, bridging the asynchronous presentation callback
     * to the synchronous evaluate tool.
     */
    private static final class CapturingValueNode implements XValueNode {

        /**
         * Receives the rendered value text.
         */
        private final AtomicReference<String> sink;

        /**
         * Released once a presentation has been captured.
         */
        private final CountDownLatch latch;

        /**
         * Creates a capturing value node.
         *
         * @param sink  the reference to populate with the rendered value.
         * @param latch the latch to release once the value is captured.
         */
        CapturingValueNode(AtomicReference<String> sink, CountDownLatch latch) {
            this.sink = sink;
            this.latch = latch;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setPresentation(Icon icon, String type, String value, boolean hasChildren) {
            sink.set((type != null && !type.isEmpty() ? "(" + type + ") " : "") + value);
            latch.countDown();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setPresentation(Icon icon, XValuePresentation presentation, boolean hasChildren) {
            TextAccumulatingRenderer renderer = new TextAccumulatingRenderer();
            presentation.renderValue(renderer);
            String type = presentation.getType();
            sink.set((type != null && !type.isEmpty() ? "(" + type + ") " : "") + renderer.text());
            latch.countDown();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setFullValueEvaluator(XFullValueEvaluator fullValueEvaluator) {
            // No-op: the tool captures the inline presentation, not the expandable full value.
        }
    }

    /**
     * An {@link XCompositeNode} that collects computed children as {@code name = value} lines into a
     * {@link StringBuilder}, bridging the asynchronous children callback to the synchronous
     * variable-listing tool. Each child's value is rendered synchronously via its own presentation
     * callback (values are already resolved by the time {@code addChildren} is invoked).
     */
    private static final class CollectingCompositeNode implements XCompositeNode {

        /**
         * The buffer receiving {@code - name = value} lines.
         */
        private final StringBuilder sink;

        /**
         * Receives an error message if the children could not be computed.
         */
        private final AtomicReference<String> error;

        /**
         * Released once the final batch of children has been added (or an error reported).
         */
        private final CountDownLatch latch;

        /**
         * The per-child presentation timeout in seconds.
         */
        private final Integer timeoutSeconds;

        /**
         * Creates a collecting composite node.
         *
         * @param sink           the buffer to append variable lines to.
         * @param error          the reference to populate on error.
         * @param latch          the latch to release when children are complete.
         * @param timeoutSeconds the per-child presentation timeout in seconds.
         */
        CollectingCompositeNode(StringBuilder sink, AtomicReference<String> error, CountDownLatch latch, Integer timeoutSeconds) {
            this.sink = sink;
            this.error = error;
            this.latch = latch;
            this.timeoutSeconds = timeoutSeconds;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void addChildren(XValueChildrenList children, boolean last) {
            for (int i = 0; i < children.size(); i++) {
                String name = children.getName(i);
                XValue value = children.getValue(i);
                AtomicReference<String> rendered = new AtomicReference<>("...");
                CountDownLatch childLatch = new CountDownLatch(1);
                value.computePresentation(new CapturingValueNode(rendered, childLatch), XValuePlace.TREE);
                try {
                    childLatch.await(timeoutSeconds != null && timeoutSeconds > 0 ? timeoutSeconds : DEFAULT_TIMEOUT_SECONDS, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
                sink.append("- `").append(name).append("` = ").append(rendered.get()).append("\n");
            }
            if (last) {
                latch.countDown();
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void tooManyChildren(int remaining, Runnable addNextChildren) {
            sink.append("- _(").append(remaining).append(" more not shown)_\n");
            latch.countDown();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        @SuppressWarnings("deprecation")
        public void tooManyChildren(int remaining) {
            tooManyChildren(remaining, null);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setAlreadySorted(boolean alreadySorted) {
            // No-op: display order is not significant for the tool's textual listing.
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setErrorMessage(String errorMessage) {
            error.set(errorMessage);
            latch.countDown();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setErrorMessage(String errorMessage, com.intellij.xdebugger.frame.XDebuggerTreeNodeHyperlink link) {
            setErrorMessage(errorMessage);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void setMessage(String message, Icon icon, com.intellij.ui.SimpleTextAttributes attributes,
                               com.intellij.xdebugger.frame.XDebuggerTreeNodeHyperlink link) {
            // No-op: informational messages are not part of the variable listing.
        }
    }

    /**
     * An {@link XValuePresentation.XValueTextRenderer} that accumulates every rendered fragment into
     * a single string, so a value's rich presentation can be flattened to plain text.
     */
    private static final class TextAccumulatingRenderer implements XValuePresentation.XValueTextRenderer {

        /**
         * The accumulated text.
         */
        private final StringBuilder buffer = new StringBuilder();

        /**
         * Returns the accumulated text.
         *
         * @return the flattened presentation text.
         */
        String text() {
            return buffer.toString();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderValue(String value) {
            buffer.append(value);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderValue(String value, com.intellij.openapi.editor.colors.TextAttributesKey key) {
            buffer.append(value);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderStringValue(String value) {
            buffer.append('"').append(value).append('"');
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderStringValue(String value, String additionalSpecialCharsToHighlight, int maxLength) {
            buffer.append('"').append(value).append('"');
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderNumericValue(String value) {
            buffer.append(value);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderKeywordValue(String value) {
            buffer.append(value);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderComment(String comment) {
            buffer.append(comment);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderSpecialSymbol(String symbol) {
            buffer.append(symbol);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void renderError(String error) {
            buffer.append(error);
        }
    }
}
