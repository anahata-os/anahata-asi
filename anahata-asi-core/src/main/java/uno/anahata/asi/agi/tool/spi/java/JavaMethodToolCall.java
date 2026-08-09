/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.tool.spi.java;

import java.util.Map;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;

/**
 * A model-agnostic representation of a request to execute a specific Java
 * method tool.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class JavaMethodToolCall extends AbstractToolCall<JavaMethodTool, JavaMethodToolResponse> {

    /**
     * Constructs a new JavaMethodToolCall instance.
     *
     * @param modelMessage The model message initiating this call.
     * @param id The unique identifier of this call.
     * @param tool The target JavaMethodTool instance.
     * @param rawArgs The raw, untyped JSON arguments passed by the model.
     * @param args The converted, type-safe Java arguments.
     */
    public JavaMethodToolCall(AbstractModelMessage modelMessage, String id, @NonNull JavaMethodTool tool, @NonNull Map<String, Object> rawArgs, @NonNull Map<String, Object> args) {
        super(modelMessage, id, tool, rawArgs, args);
    }

    @Override
    protected JavaMethodToolResponse createResponse() {
        return new JavaMethodToolResponse(this);
    }

    /**
     * {@inheritDoc} Returns a Java-like method signature: toolName(arg1, arg2,
     * null, arg4).
     */
    @Override
    public String asText() {
        String inner = getTool().getParameters().stream()
                .map(p -> {
                    Object val = getArgs().get(p.getName());
                    return p.getName() + " = " + TextUtils.resolveContentString(val);
                })
                .collect(Collectors.joining(", "));
        String callStr = getToolName() + "(" + inner + ")";
        JavaMethodToolResponse resp = getResponse();

        return callStr + "\nResponse:\n" + resp.asText();

    }
}
