/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.render;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.util.Objects;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.FinishReason;
import uno.anahata.asi.model.core.ModelBlobPart;
import uno.anahata.asi.model.core.ModelTextPart;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.components.CodeHyperlink;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;

/**
 * A concrete implementation of {@link AbstractMessagePanel} specifically for rendering
 * {@link AbstractModelMessage} instances.
 *
 * @author anahata
 */
public class ModelMessagePanel extends AbstractMessagePanel<AbstractModelMessage> {

    private GroundingMetadataPanel groundingPanel;
    
    /** Container for the finish reason and JSON link. */
    private final JPanel footerActionsPanel;
    /** Label for the finish reason. */
    private final JLabel finishLabel;
    /** Hyperlink for the raw JSON. */
    private final CodeHyperlink jsonLink;

    /**
     * Constructs a new ModelMessagePanel.
     *
     * @param agiPanel The parent agi panel.
     * @param message The model message to render.
     */
    public ModelMessagePanel(@NonNull AgiPanel agiPanel, @NonNull AbstractModelMessage message) {
        super(agiPanel, message);
        
        // 1. Initialize footer components once
        this.footerActionsPanel = new JPanel(new BorderLayout());
        this.footerActionsPanel.setOpaque(false);
        this.footerActionsPanel.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 5));
        
        this.finishLabel = new JLabel();
        this.finishLabel.setFont(this.finishLabel.getFont().deriveFont(11f));
        this.finishLabel.setForeground(new Color(120, 120, 120));
        this.footerActionsPanel.add(this.finishLabel, BorderLayout.WEST);
        
        // Use a lazy supplier for the JSON content and title.
        // CodeHyperlink handles pretty-printing internally for "json" language.
        this.jsonLink = new CodeHyperlink("Json", 
                () -> "Model Message #" + message.getSequentialId(), 
                () -> message.getRawJson(), 
                "json");
        this.footerActionsPanel.add(this.jsonLink, BorderLayout.EAST);
        
        // Add the actions panel to the footer container immediately.
        footerContainer.add(footerActionsPanel);
        
        // 2. Setup reactive listeners for specific property updates
        new EdtPropertyChangeListener(this, message, "rawJson", evt -> updateRawJsonVisibility());
        new EdtPropertyChangeListener(this, message, "billedTokenCount", evt -> updateHeaderInfoText());
        new EdtPropertyChangeListener(this, message, "finishReason", evt -> updateFinishReason());
        new EdtPropertyChangeListener(this, message, "groundingMetadata", evt -> render());
        
        // Initial sync
        updateRawJsonVisibility();
        updateFinishReason();
    }

    /**
     * Updates the JSON link visibility.
     */
    private void updateRawJsonVisibility() {
        String rawJson = message.getRawJson();
        boolean shouldBeVisible = rawJson != null && !rawJson.isEmpty();
        if (jsonLink.isVisible() != shouldBeVisible) {
            jsonLink.setVisible(shouldBeVisible);
        }
    }

    /**
     * Updates the finish reason label and visibility.
     */
    private void updateFinishReason() {
        FinishReason reason = message.getFinishReason();
        boolean shouldBeVisible = reason != null;
        
        if (shouldBeVisible) {
            finishLabel.setText("Finish Reason: " + reason.name());
        }
        finishLabel.setVisible(shouldBeVisible);
    }

    @Override
    protected String getHeaderSuffix() {
        return String.format(" <font color='#888888' size='3'><i>(Billed Tokens: %d, Depth: %d)</i></font>", message.getBilledTokenCount(), message.getDepth());
    }

    @Override
    protected void renderFooter() {
        // Grounding metadata is usually available at the end or as a separate update.
        if (message.getGroundingMetadata() != null) {
            if (groundingPanel == null) {
                groundingPanel = new GroundingMetadataPanel(agiPanel, message.getGroundingMetadata());
                footerContainer.add(groundingPanel, 0); // Add at the top of footer
            } else {
                // If it already exists, we should ideally update it. 
                // For now, let's just replace it if the metadata object is different.
                if (groundingPanel.getMetadata() != message.getGroundingMetadata()) {
                    footerContainer.remove(groundingPanel);
                    groundingPanel = new GroundingMetadataPanel(agiPanel, message.getGroundingMetadata());
                    footerContainer.add(groundingPanel, 0);
                }
            }
        } else if (groundingPanel != null) {
            footerContainer.remove(groundingPanel);
            groundingPanel = null;
        }
    }

    @Override
    protected Color getHeaderStartColor() {
        return agiConfig.getTheme().getModelHeaderBg();
    }

    @Override
    protected Color getHeaderEndColor() {
        return agiConfig.getTheme().getModelContentBg();
    }

    @Override
    protected Color getHeaderForegroundColor() {
        return agiConfig.getTheme().getModelHeaderFg();
    }

    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(agiConfig.getTheme().getModelBorder(), 2, true);
    }
}
