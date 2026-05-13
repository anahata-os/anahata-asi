/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;

/**
 * Message implementation for Anthropic's Claude.
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class AnthropicMessage extends AbstractModelMessage<AnthropicResponse> {

    public AnthropicMessage(Agi agi, String modelId) {
        super(agi, modelId);
    }
}
