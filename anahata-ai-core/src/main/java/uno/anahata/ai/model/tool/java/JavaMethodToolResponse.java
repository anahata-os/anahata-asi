/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool.java;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Parameter;
import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolExecutionStatus;
import uno.anahata.ai.tool.AiToolException;

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
     * The original invocation request that this result corresponds to.
     * Ignored during schema generation to prevent circular references.
     */
    @NonNull
    @JsonIgnore
    private final JavaMethodToolCall call;

    /**
     * The raw exception thrown during execution, for debugging and session serialization.
     * Ignored during schema generation as it's an internal detail.
     */
    @JsonIgnore
    private Throwable exception;

    public JavaMethodToolResponse(@NonNull JavaMethodToolCall call) {
        super(call);
        this.call = call;
        setStatus(ToolExecutionStatus.PENDING);
    }

    @Override
    public JavaMethodToolCall getCall() {
        return call;
    }

    @Override
    public void execute() {
        long startTime = System.currentTimeMillis();
        setCurrent(this); // Establish the thread-local context
        
        try {
            JavaMethodTool tool = getCall().getTool();
            Object toolInstance = tool.getToolInstance();

            var method = tool.getMethod();
            Parameter[] methodParameters = method.getParameters();
            Object[] argsToInvoke = new Object[methodParameters.length];
            Map<String, Object> argsFromModel = getCall().getArgs();

            for (int i = 0; i < methodParameters.length; i++) {
                Parameter p = methodParameters[i];
                String paramName = p.getName();
                argsToInvoke[i] = argsFromModel.get(paramName);
            }

            Object result = method.invoke(toolInstance, argsToInvoke);

            setResult(result);
            setStatus(ToolExecutionStatus.EXECUTED);

        } catch (Exception e) {
            Throwable cause = (e instanceof InvocationTargetException && e.getCause() != null) ? e.getCause() : e;
            this.exception = cause;

            log.error("Tool execution failed for: {}", getCall().getToolName(), cause);

            if (cause instanceof AiToolException) {
                addError(cause.getMessage());
            } else {
                addError(getStackTraceAsString(cause));
            }
            setStatus(ToolExecutionStatus.FAILED);

        } finally {
            setCurrent(null); // Clear the context
            setExecutionTimeMillis(System.currentTimeMillis() - startTime);
        }
    }

    private String getStackTraceAsString(Throwable throwable) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        throwable.printStackTrace(pw);
        return sw.toString();
    }

    @Override
    protected int getDefaultTurnsToKeep() {
        int toolRetention = getCall().getTool().getRetentionTurns();
        if (toolRetention != -1) {
            return toolRetention;
        }
        // Fall back to the system-wide default from ChatConfig
        return getChatConfig().getDefaultToolTurnsToKeep();
    }
}
