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
import lombok.Getter;
import lombok.Setter;
import uno.anahata.ai.model.tool.AbstractToolCall;

/**
 * Represents a message originating from the AI model.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public class ModelMessage extends AbstractMessage {
    
    /** The ID of the model that generated this message. */
    private String modelId;
    
    /** The number of tokens used by this message. */
    private int tokenCount;
    
    public ModelMessage() {
        //no arg constructor
    }
    
    @Override
    public Role getRole() {
        return Role.MODEL;
    }

    /**
     * Filters and returns only the tool call parts from this message.
     * @return A list of {@link AbstractToolCall} parts, or an empty list if none exist.
     */
    public List<AbstractToolCall> getToolCalls() {
        return getParts().stream()
                .filter(AbstractToolCall.class::isInstance)
                .map(AbstractToolCall.class::cast)
                .collect(Collectors.toList());
    }
}
