/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat.render;

import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.ModelBlobPart;
import uno.anahata.ai.model.core.ModelTextPart;
import uno.anahata.ai.swing.chat.ChatPanel;
import uno.anahata.ai.swing.components.CodeHyperlink;
import uno.anahata.ai.swing.internal.EdtPropertyChangeListener;

/**
 * A concrete implementation of {@link AbstractMessagePanel} specifically for rendering
 * {@link AbstractModelMessage} instances.
 *
 * @author pablo
 */
public class ModelMessagePanel extends AbstractMessagePanel<AbstractModelMessage> {

    private GroundingMetadataPanel groundingPanel;
    private JPanel actionPanel;

    /**
     * Constructs a new ModelMessagePanel.
     *
     * @param chatPanel The parent chat panel.
     * @param message The model message to render.
     */
    public ModelMessagePanel(@NonNull ChatPanel chatPanel, @NonNull AbstractModelMessage message) {
        super(chatPanel, message);
        
        // Listen for rawJson changes to show the "Json" link when streaming completes.
        new EdtPropertyChangeListener(this, message, "rawJson", evt -> render());
        // Listen for tokenCount changes to update the header in real-time during streaming.
        new EdtPropertyChangeListener(this, message, "tokenCount", evt -> updateHeaderInfoText());
    }

    @Override
    protected String getHeaderSuffix() {
        return String.format(" <font color='#888888' size='3'><i>(Tokens: %d, Depth: %d)</i></font>", message.getTokenCount(), message.getDepth());
    }

    @Override
    protected void renderFooter() {
        if (message.getGroundingMetadata() != null) {
            if (groundingPanel == null) {
                groundingPanel = new GroundingMetadataPanel(chatPanel, message.getGroundingMetadata());
            }
            
            if (!footerContainer.isAncestorOf(groundingPanel)) {
                footerContainer.add(groundingPanel, 0);
            }
        }
        
        if (message.getRawJson() != null) {
            if (actionPanel == null) {
                actionPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
                actionPanel.setOpaque(false);
                String popupTitle = "Model Message #" + message.getSequentialId();
                String prettyJson = JacksonUtils.prettyPrintJsonString(message.getRawJson());
                actionPanel.add(new CodeHyperlink("Json", popupTitle, prettyJson, "json"));
            }
            
            if (!footerContainer.isAncestorOf(actionPanel)) {
                footerContainer.add(actionPanel);
            }
        } else if (actionPanel != null) {
            footerContainer.remove(actionPanel);
            actionPanel = null;
        }
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
