/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * A concrete {@link AbstractPart} implementation for simple text content.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public class TextPart extends AbstractPart {
    private String text;

    public TextPart(AbstractMessage message, String text) {
        super(message);
        this.text = text;
    }
    

    @Override
    public String asText() {
        return text;
    }

    @Override
    protected int getDefaultTurnsToKeep() {
        return getChatConfig().getDefaultTextPartTurnsToKeep();
    }
}
