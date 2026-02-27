/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool.java;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Parameter;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.model.tool.AbstractToolResponse;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.tool.AiToolException;

/**
 * A rich POJO that captures the complete and final outcome of a single tool
 * call. This class now follows a deferred execution model and manages the
 * thread-local context for Java tool execution.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public class JavaMethodToolResponse extends AbstractToolResponse<JavaMethodToolCall> {

    /** 
     * A thread-local storage for the currently executing Java tool response.
     * This allows tool logic to access its own context without explicit parameter passing.
     */
    private static final ThreadLocal<JavaMethodToolResponse> current = new ThreadLocal<>();

    /**
     * Gets the tool response associated with the current thread.
     * @return The active tool response, or {@code null} if none is active.
     */
    public static JavaMethodToolResponse getCurrent() {
        return current.get();
    }

    /**
     * Sets the tool response for the current thread.
     * @param response The response to set, or {@code null} to clear the context.
     */
    public static void setCurrent(JavaMethodToolResponse response) {
        if (response == null) {
            current.remove();
        } else {
            current.set(response);
        }
    }

    /**
     * The raw exception thrown during execution, for debugging and session serialization.
     * Ignored during schema generation as it's an internal detail.
     */
    @JsonIgnore
    private Throwable exception;
    
    /** Guard to ensure only one thread can execute this response at a time. */
    @JsonIgnore
    private transient AtomicBoolean executing = new AtomicBoolean(false);

    public JavaMethodToolResponse(@NonNull JavaMethodToolCall call) {
        super(call);
        setStatus(ToolExecutionStatus.PENDING);
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        this.executing = new AtomicBoolean(false);
    }

    @Override
    public void execute() {
        if (!executing.compareAndSet(false, true)) {
            throw new IllegalStateException("Tool execution is already in progress for this call.");
        }
        
        long startTime = System.currentTimeMillis();
        setCurrent(this); // Establish the thread-local context
        setStatus(ToolExecutionStatus.EXECUTING);
        setThread(Thread.currentThread());
        getAgi().getToolManager().registerExecutingCall(getCall());
        
        // Capture a snapshot of the modified arguments at the time of execution
        getModifiedArgs().clear();
        getModifiedArgs().putAll(getCall().getModifiedArgs());
        
        try {
            JavaMethodTool tool = getCall().getTool();
            Object toolkitInstance = tool.getToolkitInstance();

            var method = tool.getMethod();
            Parameter[] methodParameters = method.getParameters();
            Object[] argsToInvoke = new Object[methodParameters.length];
            Map<String, Object> effectiveArgs = getEffectiveArgs();

            for (int i = 0; i < methodParameters.length; i++) {
                Parameter p = methodParameters[i];
                String paramName = p.getName();
                argsToInvoke[i] = effectiveArgs.get(paramName);
            }

            Object result = method.invoke(toolkitInstance, argsToInvoke);

            setResult(result);
            setStatus(ToolExecutionStatus.EXECUTED);

        } catch (Throwable e) {
            log.error("Exception executing tool " + getCall(), e);
            Throwable cause = (e instanceof InvocationTargetException && e.getCause() != null) ? e.getCause() : e;
            this.exception = cause;

            if (cause instanceof InterruptedException) {
                log.info("Tool execution interrupted: {}", getCall().getToolName());
                addError("Execution interrupted by user.");
                setStatus(ToolExecutionStatus.INTERRUPTED);
                Thread.currentThread().interrupt();
            } else {
                log.error("Tool execution failed for: {}", getCall().getToolName(), cause);

                if (cause instanceof AiToolException) {
                    addError(cause.getMessage());
                } else {
                    addError(ExceptionUtils.getStackTrace(cause));
                }
                setStatus(ToolExecutionStatus.FAILED);
            }

        } finally {
            getAgi().getToolManager().unregisterExecutingCall(getCall());
            setCurrent(null); // Clear the context
            setThread(null);
            setExecutionTimeMillis(System.currentTimeMillis() - startTime);
            executing.set(false);
        }
    }

    @Override
    public void stop() {
        Thread t = getThread();
        if (t == null || !t.isAlive()) {
            throw new IllegalStateException("Tool is not currently executing.");
        }
        log.info("Stopping tool execution thread: {}", t.getName());
        t.interrupt();
    }
}
