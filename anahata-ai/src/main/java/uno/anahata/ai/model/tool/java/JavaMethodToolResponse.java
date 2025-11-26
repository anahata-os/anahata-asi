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
import uno.anahata.ai.tool.AbstractJavaTool;

/**
 * A rich POJO that captures the complete and final outcome of a single tool
 * call. This class now follows a deferred execution model and manages the
 * thread-local context for {@link AbstractJavaTool} instances.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public class JavaMethodToolResponse extends AbstractToolResponse<JavaMethodToolCall> {

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
        JavaMethodTool tool = getCall().getTool();
        Object toolInstance = tool.getToolInstance();
        AbstractJavaTool contextAwareTool = (toolInstance instanceof AbstractJavaTool) ? (AbstractJavaTool) toolInstance : null;

        try {
            if (contextAwareTool != null) {
                contextAwareTool.setContext(this);
            }

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

            log.error("Tool execution failed for: {}", getCall().getName(), cause);

            if (cause instanceof AiToolException) {
                setError(cause.getMessage());
            } else {
                setError(getStackTraceAsString(cause));
            }
            setStatus(ToolExecutionStatus.FAILED);

        } finally {
            if (contextAwareTool != null) {
                contextAwareTool.clearContext();
            }
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
