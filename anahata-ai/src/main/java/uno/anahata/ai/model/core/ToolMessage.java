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
package uno.anahata.ai.model.core;

import java.util.List;
import java.util.stream.Collectors;
import uno.anahata.ai.model.tool.AbstractToolResponse;

/**
 * Represents a message containing the results of tool executions.
 * This message is sent from the client back to the model.
 *
 * @author anahata-gemini-pro-2.5
 */
public class ToolMessage extends AbstractMessage {
    @Override
    public Role getRole() {
        return Role.TOOL;
    }

    /**
     * Filters and returns only the tool response parts from this message.
     * @return A list of {@link AbstractToolResponse} parts, or an empty list if none exist.
     */
    @SuppressWarnings("unchecked")
    public List<AbstractToolResponse> getToolResponses() {
        return getParts().stream()
                .filter(AbstractToolResponse.class::isInstance)
                .map(p -> (AbstractToolResponse) p)
                .collect(Collectors.toList());
    }
}
