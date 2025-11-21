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

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;

/**
 * The abstract base class for all components of a {@link AbstractMessage}.
 * <p>
 * This rich, hierarchical model allows for type-safe handling of different
 * content types (text, tool calls, etc.) and provides common functionality
 * like a back-reference to the parent message and a pruning flag.
 */
@Getter
@Setter
public abstract class AbstractPart {
    /**
     * A backward reference to the Message that contains this part.
     * This is for runtime convenience and is ignored during schema generation
     * to keep the public contract clean.
     */
    @JsonIgnore
    private AbstractMessage message;

    /**
     * A flag indicating whether this part has been pruned from the context.
     */
    private boolean pruned = false;
    
    /**
     * Returns the content of the part as a simple string.
     * This is implemented by subclasses.
     * @return The text representation of the part.
     */
    public abstract String asText();
}
