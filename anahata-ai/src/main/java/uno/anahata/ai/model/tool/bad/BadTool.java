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
package uno.anahata.ai.model.tool.bad;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Map;
import uno.anahata.ai.model.tool.AbstractTool;
import uno.anahata.ai.model.tool.ToolParameter;
import uno.anahata.ai.model.tool.ToolPermission;

/**
 * A special tool implementation representing a tool that was requested by the
 * model but was not found in the registered toolkits.
 *
 * @author anahata-gemini-pro-2.5
 */
public class BadTool extends AbstractTool<ToolParameter, BadToolCall> {

    public BadTool(String name) {
        super(
            name,
            "Tool not found: " + name,
            null, // No parent toolkit
            ToolPermission.DENY_NEVER,
            Collections.emptyList(),
            null // No return type schema for a bad tool
        );
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
