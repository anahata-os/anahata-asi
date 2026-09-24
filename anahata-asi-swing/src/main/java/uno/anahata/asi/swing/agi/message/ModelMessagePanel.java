/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi.message;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;
import lombok.NonNull;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.swing.agi.AgiPanel;
import uno.anahata.asi.swing.components.CodeHyperlink;
import uno.anahata.asi.swing.icons.ActionIconKey;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingTask;

/**
 * A concrete implementation of {@link AbstractMessagePanel} specifically for rendering
 * {@link AbstractModelMessage} instances.
 *
 * @author anahata
 */
public class ModelMessagePanel extends AbstractMessagePanel<AbstractModelMessage> {

    /** The panel displaying grounding metadata, if available. */
    private GroundingMetadataPanel groundingPanel;
    
    /** Container for the finish reason and JSON link. */
    private final JPanel footerActionsPanel;
    /**
     * Label for the finish reason.
     */
    private final JLabel finishLabel;
    /**
     * Hyperlink for the raw JSON.
     */
    private final CodeHyperlink jsonLink;

    /**
     * Container for the batch tool execution controls.
     */
    private final JPanel batchToolsPanel;
    /**
     * Button to execute all pending tool calls without sending a prompt to the
     * model.
     */
    private final JButton runAllPendingButton;
    /**
     * Button to decline all pending tool calls in this message.
     */
    private final JButton declineAllPendingButton;
    /**
     * Button to stop the sequential batch tool execution.
     */
    private final JButton stopBatchButton;

    /**
     * Constructs a new ModelMessagePanel with integrated batch execution
     * toolbar and live countdown controls.
     *
     * @param message The model message to render.
     * @param agiPanel The parent agi panel.
     */
    public ModelMessagePanel(@NonNull AgiPanel agiPanel, @NonNull AbstractModelMessage message) {
        super(agiPanel, message);

        // 1. Initialize footer components once
        this.batchToolsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 2));
        this.batchToolsPanel.setOpaque(false);
        this.batchToolsPanel.setBorder(BorderFactory.createEmptyBorder(2, 4, 2, 4));

        this.runAllPendingButton = new JButton("Run All Pending", agiConfig.getActionIcon(ActionIconKey.SEND, 14));
        this.runAllPendingButton.setToolTipText("Executes all pending tool calls in this message without sending a prompt to the model");
        this.runAllPendingButton.addActionListener(e -> executeBatchTools());

        this.declineAllPendingButton = new JButton("Decline All Pending", agiConfig.getActionIcon(ActionIconKey.CANCEL, 14));
        this.declineAllPendingButton.setToolTipText("Declines all pending tool calls in this message");
        this.declineAllPendingButton.addActionListener(e -> message.declineAllPending());

        this.stopBatchButton = new JButton("Stop Remaining", agiConfig.getActionIcon(ActionIconKey.STOP, 14));
        this.stopBatchButton.setToolTipText("Stops sequential tool execution after the active tool completes; unstarted tools stay in PENDING");
        this.stopBatchButton.addActionListener(e -> message.stopRunningAllPending());

        this.batchToolsPanel.add(declineAllPendingButton);
        this.batchToolsPanel.add(runAllPendingButton);
        this.batchToolsPanel.add(stopBatchButton);

        this.footerActionsPanel = new JPanel(new BorderLayout());
        this.footerActionsPanel.setOpaque(false);
        this.footerActionsPanel.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 5));

        this.finishLabel = new JLabel();
        this.finishLabel.setFont(this.finishLabel.getFont().deriveFont(11f));
        this.finishLabel.setForeground(agiConfig.getTheme().getMutedFg());
        this.footerActionsPanel.add(this.finishLabel, BorderLayout.WEST);

        // Use a lazy supplier for the JSON content and title.
        // CodeHyperlink handles pretty-printing internally for "json" language.
        this.jsonLink = new CodeHyperlink("Json",
                () -> "Model Message #" + message.getSequentialId(),
                () -> message.getRawJson(),
                "json");
        this.footerActionsPanel.add(this.jsonLink, BorderLayout.EAST);

        // Add the batch tools panel and actions panel to the footer container immediately.
        footerContainer.add(batchToolsPanel);
        footerContainer.add(footerActionsPanel);

        // 2. Setup reactive listeners for specific property updates
        new EdtPropertyChangeListener(this, message, "rawJson", evt -> updateRawJsonVisibility());
        new EdtPropertyChangeListener(this, message, "billedPromptTokens", evt -> updateHeaderInfoText());
        new EdtPropertyChangeListener(this, message, "billedCompletionTokens", evt -> updateHeaderInfoText());
        new EdtPropertyChangeListener(this, message, "finishReason", evt -> updateFinishReason());
        new EdtPropertyChangeListener(this, message, "groundingMetadata", evt -> render());
        new EdtPropertyChangeListener(this, message, "runningAllPending", evt -> updateBatchToolsUI());
        new EdtPropertyChangeListener(this, message, "remainingTools", evt -> updateBatchToolsUI());
        new EdtPropertyChangeListener(this, message, "parts", evt -> updateBatchToolsUI());

        // Initial sync
        updateRawJsonVisibility();
        updateFinishReason();
        updateBatchToolsUI();
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

    /**
     * Updates the batch tools execution panel visibility and countdown button
     * labels based on remaining pending and executing tool calls.
     */
    private void updateBatchToolsUI() {
        boolean hasPending = message.hasPendingTools();
        boolean runningAll = message.isRunningAllPending();

        if (!hasPending && !runningAll) {
            batchToolsPanel.setVisible(false);
            return;
        }

        batchToolsPanel.setVisible(true);
        int remaining = message.getRemainingToolCallsCount();

        if (runningAll) {
            runAllPendingButton.setVisible(false);
            declineAllPendingButton.setVisible(false);
            stopBatchButton.setVisible(true);
            stopBatchButton.setText(remaining > 1 ? "Stop Remaining (" + remaining + ")" : "Stop Remaining");
        } else {
            int pendingCount = message.getPendingToolCalls().size();
            runAllPendingButton.setText(pendingCount > 1 ? "Run All Pending (" + pendingCount + ")" : "Run Pending");
            declineAllPendingButton.setText(pendingCount > 1 ? "Decline All Pending (" + pendingCount + ")" : "Decline Pending");
            runAllPendingButton.setVisible(true);
            declineAllPendingButton.setVisible(true);
            stopBatchButton.setVisible(false);
        }
    }

    /**
     * Executes all pending tool calls in this message asynchronously via
     * SwingTask without sending a prompt to the model.
     */
    private void executeBatchTools() {
        new SwingTask<Boolean>(agiPanel, "Executing Batch Tools", () -> {
            return message.executeAllPending();
        }).start();
    }
    /** 
     * {@inheritDoc} 
     * <p>Includes billed token count and depth in the header suffix.</p>
     */
    @Override
    protected String getHeaderSuffix() {
        return String.format(" <font color='#888888' size='3'><i>(Billed: In:%d, Out:%d, Depth: %d)</i></font>", message.getBilledPromptTokens(), message.getBilledCompletionTokens(), message.getDepth());
    }

    /** 
     * {@inheritDoc} 
     * <p>Renders the {@link GroundingMetadataPanel} if metadata is available.</p>
     */
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

    /** 
     * {@inheritDoc} 
     * <p>Returns the model header background color.</p>
     */
    @Override
    protected Color getHeaderStartColor() {
        return agiConfig.getTheme().getModelHeaderBg();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the model content background color.</p>
     */
    @Override
    protected Color getHeaderEndColor() {
        return agiConfig.getTheme().getModelContentBg();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the model header foreground color.</p>
     */
    @Override
    protected Color getHeaderForegroundColor() {
        return agiConfig.getTheme().getModelHeaderFg();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the model message border.</p>
     */
    @Override
    protected Border getMessageBorder() {
        return BorderFactory.createLineBorder(agiConfig.getTheme().getModelBorder(), 1, true);
    }
}
