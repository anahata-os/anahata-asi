/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat.render;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.BlobPart;
import uno.anahata.asi.model.core.ModelBlobPart;
import uno.anahata.asi.model.core.ModelTextPart;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.chat.ChatPanel;

/**
 * A factory for creating {@link AbstractPartPanel} instances based on the 
 * concrete type of the {@link AbstractPart}.
 * <p>
 * This factory centralizes the mapping between part domain objects and their 
 * corresponding UI components, ensuring consistency across the application.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class PartPanelFactory {

    /**
     * Creates the appropriate part panel for the given part.
     * 
     * @param chatPanel The parent chat panel.
     * @param part The part to render.
     * @return The created part panel, or null if the part type is unsupported.
     */
    public static AbstractPartPanel<?> createPartPanel(ChatPanel chatPanel, AbstractPart part) {
        if (part instanceof ModelTextPart modelTextPart) {
            return new TextPartPanel(chatPanel, modelTextPart);
        } else if (part instanceof ModelBlobPart modelBlobPart) {
            return new BlobPartPanel(chatPanel, modelBlobPart);
        } else if (part instanceof AbstractToolCall toolCall) {
            return new ToolCallPanel(chatPanel, toolCall);
        } else if (part instanceof TextPart textPart) {
            return new TextPartPanel(chatPanel, textPart);
        } else if (part instanceof BlobPart blobPart) {
            return new BlobPartPanel(chatPanel, blobPart);
        }
        log.warn("No panel found for part type: {}", part.getClass().getName());
        return null;
    }
}
