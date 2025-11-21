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
 * call. This class now follows a deferred execution model.
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
        try {
            JavaMethodTool tool = getCall().getTool();
            var method = tool.getMethod();
            var toolInstance = tool.getToolInstance();

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
            setExecutionTimeMillis(System.currentTimeMillis() - startTime);
        }
    }
    
    private String getStackTraceAsString(Throwable throwable) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        throwable.printStackTrace(pw);
        return sw.toString();
    }
}
