/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.tool.java;

import java.util.Map;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.asi.internal.TextUtils;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.tool.AbstractToolCall;

/**
 * A model-agnostic representation of a request to execute a specific Java method tool.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class JavaMethodToolCall extends AbstractToolCall<JavaMethodTool, JavaMethodToolResponse> {
    
    public JavaMethodToolCall(AbstractModelMessage modelMessage, @NonNull String id, @NonNull JavaMethodTool tool, @NonNull Map<String, Object> rawArgs, @NonNull Map<String, Object> args) {
        super(modelMessage, id, tool, rawArgs, args);
    }

    @Override
    protected JavaMethodToolResponse createResponse(AbstractToolMessage toolMessage) {
        return new JavaMethodToolResponse(this);
    }

    /**
     * {@inheritDoc}
     * Returns a Java-like method signature: toolName(arg1, arg2, null, arg4).
     */
    @Override
    public String asText() {
        String inner = getTool().getParameters().stream()
                .map(p -> {
                    Object val = getArgs().get(p.getName());
                    return val == null ? "null" : TextUtils.formatValue(val.toString());
                })
                .collect(Collectors.joining(", "));
        return TextUtils.formatValue(getToolName() + "(" + inner + ")");
    }
}
