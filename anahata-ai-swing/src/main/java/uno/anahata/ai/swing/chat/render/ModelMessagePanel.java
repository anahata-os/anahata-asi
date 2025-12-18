/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.ModelBlobPart;
import uno.anahata.ai.model.core.ModelTextPart;
import uno.anahata.ai.swing.chat.ChatPanel;

/**
 * A concrete implementation of {@link AbstractMessagePanel} specifically for rendering
 * {@link AbstractModelMessage} instances.
 *
 * @author pablo
 */
public class ModelMessagePanel extends AbstractMessagePanel<AbstractModelMessage> {

    /**
     * Constructs a new ModelMessagePanel.
     *
     * @param chatPanel The parent chat panel.
     * @param message The model message to render.
     */
    public ModelMessagePanel(@NonNull ChatPanel chatPanel, @NonNull AbstractModelMessage message) {
        super(chatPanel, message);
    }

    @Override
    protected AbstractPartPanel createPartPanel(AbstractPart part) {
        if (part instanceof ModelTextPart modelTextPart) {
            return new TextPartPanel(chatPanel, modelTextPart);
        } else if (part instanceof ModelBlobPart modelBlobPart) {
            return new BlobPartPanel(chatPanel, modelBlobPart);
        }
        return super.createPartPanel(part);
    }

    @Override
    protected Color getHeaderStartColor() {
        return chatConfig.getTheme().getModelHeaderBg();
    }

    @Override
    protected Color getHeaderEndColor() {
        return chatConfig.getTheme().getModelContentBg();
    }

    @Override
    protected Color getHeaderForegroundColor() {
        return chatConfig.getTheme().getModelHeaderFg();
    }

    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(chatConfig.getTheme().getModelBorder(), 2, true);
    }
}
