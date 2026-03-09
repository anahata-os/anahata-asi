/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message.part;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.BlobPart;
import uno.anahata.asi.agi.message.ModelBlobPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.TextPart;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.agi.message.part.tool.ToolCallPanel;

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
     * @param agiPanel The parent agi panel.
     * @param part The part to render.
     * @return The created part panel, or null if the part type is unsupported.
     */
    public static AbstractPartPanel<?> createPartPanel(AgiPanel agiPanel, AbstractPart part) {
        if (part instanceof ModelTextPart modelTextPart) {
            return new TextPartPanel(agiPanel, modelTextPart);
        } else if (part instanceof ModelBlobPart modelBlobPart) {
            return new BlobPartPanel(agiPanel, modelBlobPart);
        } else if (part instanceof AbstractToolCall toolCall) {
            return new ToolCallPanel(agiPanel, toolCall);
        } else if (part instanceof TextPart textPart) {
            return new TextPartPanel(agiPanel, textPart);
        } else if (part instanceof BlobPart blobPart) {
            return new BlobPartPanel(agiPanel, blobPart);
        }
        log.warn("No panel found for part type: {}", part.getClass().getName());
        return null;
    }
}
