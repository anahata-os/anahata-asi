/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.model.core.UserMessage;
import uno.anahata.asi.swing.agi.AgiPanel;

/**
 * A concrete implementation of {@link AbstractMessagePanel} specifically for rendering
 * {@link UserMessage} instances in the conversation history.
 *
 * @author anahata
 */
public class UserMessagePanel extends AbstractMessagePanel<UserMessage> {

    /**
     * Constructs a new UserMessagePanel.
     *
     * @param agiPanel The parent agi panel.
     * @param message The user message to render.
     */
    public UserMessagePanel(@NonNull AgiPanel agiPanel, @NonNull UserMessage message) {
        super(agiPanel, message);
    }

    @Override
    protected Color getHeaderStartColor() {
        return agiConfig.getTheme().getUserHeaderBg();
    }

    @Override
    protected Color getHeaderEndColor() {
        return agiConfig.getTheme().getUserContentBg();
    }

    @Override
    protected Color getHeaderForegroundColor() {
        return agiConfig.getTheme().getUserHeaderFg();
    }

    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(agiConfig.getTheme().getUserBorder(), 2, true);
    }
}
