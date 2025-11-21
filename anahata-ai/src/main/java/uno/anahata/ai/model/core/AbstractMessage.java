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

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;

/**
 * The abstract base class for all messages in a conversation, providing common
 * metadata and functionality.
 * <p>
 * This rich, hierarchical model supports type-safe roles through its subclasses
 * (e.g., {@code UserMessage}, {@code ModelMessage}) and ensures each message
 * has a unique identity and timestamp.
 */
@Getter
@Setter
public abstract class AbstractMessage {
    /**
     * A unique, immutable identifier for this message.
     */
    private final String id = UUID.randomUUID().toString();

    /**
     * The timestamp when this message was created, in milliseconds since the epoch.
     */
    private final long timestamp = System.currentTimeMillis();
    
    /**
     * A monotonically increasing number assigned to the message when it is added to a chat,
     * representing its order in the conversation.
     */
    private long sequenceNumber;
    
    /**
     * The number of tokens in this specific message, as reported by the provider.
     * This is crucial for granular cost analysis and context management.
     */
    private int tokenCount;

    /**
     * The list of parts that make up the message content.
     */
    private List<AbstractPart> parts = new ArrayList<>();

    /**
     * A flag indicating whether this message has been pruned from the context.
     */
    private boolean pruned = false;

    /**
     * Gets the role of the entity that created this message.
     * This is implemented by subclasses to provide compile-time type safety.
     *
     * @return The role of the message creator.
     */
    public abstract Role getRole();
    
    /**
     * Convenience method to get the message content as a single string,
     * concatenating the text representation of all its parts.
     *
     * @return The concatenated text content.
     */
    public String asText() {
        return getParts().stream()
            .map(AbstractPart::asText)
            .collect(Collectors.joining());
    }
}
