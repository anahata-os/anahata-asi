/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.message;

import lombok.Getter;
import uno.anahata.asi.agi.Agi;

/**
 * A specialized UserMessage representing intent originated by another AGI session
 * within the same ASI container.
 * 
 * @author anahata
 */
public class AgiUserMessage extends UserMessage {

    /** The UUID of the AGI session that originated this message. */
    @Getter
    private final String spawningAgiUuid;

    /**
     * Constructs a new AgiUserMessage.
     * 
     * @param agi The target session that will receive the message.
     * @param spawningAgiUuid The UUID of the session sending the message.
     */
    public AgiUserMessage(Agi agi, String spawningAgiUuid) {
        super(agi);
        this.spawningAgiUuid = spawningAgiUuid;
    }

    /**
     * {@inheritDoc}
     * <p>Returns the origin formatted as 'spawningUuid@containerId'.</p>
     */
    @Override
    public String getFrom() {
        return spawningAgiUuid + "@" + getAgi().getConfig().getHostApplicationId();
    }
}
