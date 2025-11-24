/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.tool.bad;

import java.lang.reflect.Type;
import java.util.Map;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.ToolPermission;

/**
 * A special tool implementation representing a tool that was requested by the
 * model but was not found in the registered toolkits.
 *
 * @author anahata-gemini-pro-2.5
 */
public class BadTool extends AbstractTool<BadToolParam, BadToolCall> {

    public BadTool(String name) {
        super(name);
        super.description = "Tool not found: " + name;
        super.permission = ToolPermission.DENY_NEVER;
        // No parameters are created for a bad tool.
    }

    @Override
    public BadToolCall createCall(String id, Map<String, Object> args) {
        return new BadToolCall(id, this, args);
    }

    @Override
    public Type getResponseType() {
        return BadToolResponse.class;
    }
}
