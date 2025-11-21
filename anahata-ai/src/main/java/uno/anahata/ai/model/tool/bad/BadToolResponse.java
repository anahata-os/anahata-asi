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

import lombok.Getter;
import lombok.NonNull;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolExecutionStatus;

/**
 * The response for a {@link BadToolCall}. Its status is immediately set to
 * {@link ToolExecutionStatus#NOT_EXECUTED} and its execute method is a no-op.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
public class BadToolResponse extends AbstractToolResponse<BadToolCall> {

    @NonNull
    private final BadToolCall call;

    public BadToolResponse(@NonNull BadToolCall call) {
        this.call = call;
        setStatus(ToolExecutionStatus.NOT_EXECUTED);
        setError("Tool call rejected: The tool '" + call.getName() + "' was not found.");
    }

    @Override
    public BadToolCall getCall() {
        return call;
    }

    @Override
    public void execute() {
        // No-op, as the tool was never found.
    }
}
