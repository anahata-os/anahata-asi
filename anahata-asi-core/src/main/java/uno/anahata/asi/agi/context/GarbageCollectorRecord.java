/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.context;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * A record of a single garbage collection event, capturing the recycling of a 
 * conversation turn.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GarbageCollectorRecord {
    /** The timestamp when the collection occurred. */
    private long timestamp;
    /** The sequential ID of the message that was recycled. */
    private long messageId;
    /** The simple class name of the message type. */
    private String type;
    /** The total number of content tokens recycled (not including metadata). */
    private int tokenCount;
}
