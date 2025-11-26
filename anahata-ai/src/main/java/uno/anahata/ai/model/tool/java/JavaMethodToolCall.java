/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool.java;

import java.util.Map;
import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.core.AbstractToolMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.tool.AbstractToolCall;

/**
 * A model-agnostic representation of a request to execute a specific Java method tool.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class JavaMethodToolCall extends AbstractToolCall<JavaMethodTool, JavaMethodToolResponse> {
    
    public JavaMethodToolCall(AbstractModelMessage modelMessage, @NonNull String id, @NonNull JavaMethodTool tool, @NonNull Map<String, Object> args) {
        super(modelMessage, id, tool, args);
    }

    @Override
    protected JavaMethodToolResponse createResponse(AbstractToolMessage toolMessage) {
        return new JavaMethodToolResponse(this);
    }
}
