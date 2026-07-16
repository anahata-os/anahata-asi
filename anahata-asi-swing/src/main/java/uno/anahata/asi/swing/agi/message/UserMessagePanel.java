/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message;

import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.agi.message.UserMessage;
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

    /** 
     * {@inheritDoc} 
     * <p>Returns the user header background color.</p>
     */
    @Override
    protected Color getHeaderStartColor() {
        return agiConfig.getTheme().getUserHeaderBg();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the user content background color.</p>
     */
    @Override
    protected Color getHeaderEndColor() {
        return agiConfig.getTheme().getUserContentBg();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the user header foreground color.</p>
     */
    @Override
    protected Color getHeaderForegroundColor() {
        return agiConfig.getTheme().getUserHeaderFg();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the user message border.</p>
     */
    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(agiConfig.getTheme().getUserBorder(), 1, true);
    }
}
