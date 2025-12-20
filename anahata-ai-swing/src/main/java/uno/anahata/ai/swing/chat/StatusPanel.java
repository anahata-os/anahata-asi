/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.RenderingHints;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.Timer;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.internal.TimeUtils;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.ResponseUsageMetadata;
import uno.anahata.ai.status.ApiErrorRecord;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.status.StatusManager;
import uno.anahata.ai.swing.components.CodeHyperlink;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.media.util.AudioPlaybackPanel;

/**
 * A panel that displays the real-time status of the chat session, including
 * API call status, context usage, and error/retry information.
 *
 * @author anahata
 */
@Getter
public class StatusPanel extends JPanel {
    /** Formatter for timestamps in error logs. */
    private static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat("HH:mm:ss");
    /** Formatter for token counts. */
    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    /** The parent chat panel. */
    private final ChatPanel chatPanel;
    /** The active chat session. */
    private Chat chat;
    /** The chat configuration. */
    private SwingChatConfig chatConfig;
    /** Timer for periodic UI refreshes. */
    private final Timer refreshTimer;
    /** The last known status, used to detect changes for audio feedback. */
    private ChatStatus lastStatus = null;

    /** Visual indicator for the current status. */
    private StatusIndicator statusIndicator;
    /** Label displaying the status text. */
    private JLabel statusLabel;
    /** Progress bar showing context window usage. */
    private ContextUsageBar contextUsageBar;
    /** Panel for displaying API error and retry details. */
    private JPanel apiErrorsPanel; 
    /** Label for detailed token usage information. */
    private JLabel tokenDetailsLabel; 
    /** Hyperlink to view the raw JSON request configuration. */
    private CodeHyperlink rawJsonRequestConfigLink; 
    /** Hyperlink to view the raw JSON response. */
    private CodeHyperlink rawJsonResponseLink; 
    /** Toggle button for sound notifications. */
    private JToggleButton soundToggle;
    /** Panel for managing audio playback feedback. */
    private final AudioPlaybackPanel audioPlaybackPanel; 
    /** Label for displaying prompt blocking reasons. */
    private JLabel blockReasonLabel; 

    /**
     * Constructs a new StatusPanel.
     * 
     * @param chatPanel The parent chat panel.
     */
    public StatusPanel(ChatPanel chatPanel) {
        super(new BorderLayout(10, 2));
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        this.chatConfig = chatPanel.getChatConfig();
        this.audioPlaybackPanel = new AudioPlaybackPanel(chatPanel);
        initComponents();
        
        this.refreshTimer = new Timer(1000, e -> refresh());
    }

    /**
     * {@inheritDoc}
     * Starts the refresh timer when the panel is added to the UI.
     */
    @Override
    public void addNotify() {
        super.addNotify();
        refreshTimer.start();
    }

    /**
     * {@inheritDoc}
     * Stops the refresh timer when the panel is removed from the UI.
     */
    @Override
    public void removeNotify() {
        refreshTimer.stop();
        super.removeNotify();
    }

    /**
     * Initializes the UI components and layout.
     */
    private void initComponents() {
        setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 5));

        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        // --- Row 1 (Top) --- 
        JPanel row1Panel = new JPanel(new BorderLayout(10, 0));
        row1Panel.setAlignmentX(LEFT_ALIGNMENT);
        
        JPanel chatStatusPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        statusIndicator = new StatusIndicator();
        statusLabel = new JLabel("Initializing...");
        soundToggle = new JToggleButton(IconUtils.getIcon("bell.png"));
        soundToggle.setSelectedIcon(IconUtils.getIcon("bell_mute.png"));
        soundToggle.setToolTipText("Toggle Sound Notifications");
        soundToggle.setSelected(!chatConfig.isAudioFeedbackEnabled());
        soundToggle.addActionListener(e -> chatConfig.setAudioFeedbackEnabled(!soundToggle.isSelected()));
        chatStatusPanel.add(soundToggle);
        chatStatusPanel.add(statusIndicator);
        chatStatusPanel.add(statusLabel);
        row1Panel.add(chatStatusPanel, BorderLayout.WEST);
        
        contextUsageBar = new ContextUsageBar(chatPanel); 
        row1Panel.add(contextUsageBar, BorderLayout.EAST);
        
        add(row1Panel);

        // --- Row 2 (Middle) --- 
        JPanel row2Panel = new JPanel(new BorderLayout(10, 0));
        row2Panel.setAlignmentX(LEFT_ALIGNMENT);
        row2Panel.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));

        JPanel tokenAndJsonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        
        rawJsonRequestConfigLink = new CodeHyperlink("Request Config", "Raw JSON Request Config", "", "json");
        tokenAndJsonPanel.add(rawJsonRequestConfigLink);

        rawJsonResponseLink = new CodeHyperlink("Response", "Raw JSON Response", "", "json");
        tokenAndJsonPanel.add(rawJsonResponseLink);

        tokenDetailsLabel = new JLabel();
        tokenAndJsonPanel.add(tokenDetailsLabel);

        row2Panel.add(tokenAndJsonPanel, BorderLayout.WEST);

        row2Panel.add(audioPlaybackPanel, BorderLayout.EAST);
        
        add(row2Panel);

        // --- Row 3 (Bottom-Middle) --- 
        JPanel responseDetailsPanel = new JPanel();
        responseDetailsPanel.setLayout(new BoxLayout(responseDetailsPanel, BoxLayout.X_AXIS));
        responseDetailsPanel.setAlignmentX(LEFT_ALIGNMENT);
        responseDetailsPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));

        blockReasonLabel = new JLabel();
        blockReasonLabel.setForeground(Color.RED.darker());
        responseDetailsPanel.add(blockReasonLabel);
        responseDetailsPanel.add(Box.createHorizontalGlue());

        add(responseDetailsPanel);

        apiErrorsPanel = new JPanel();
        apiErrorsPanel.setAlignmentX(LEFT_ALIGNMENT);
        apiErrorsPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 0, 0));
        apiErrorsPanel.setVisible(false);
        add(apiErrorsPanel);
    }

    /**
     * Reloads the panel with the state of the current chat session.
     */
    public void reload() {
        this.chat = chatPanel.getChat();
        this.chatConfig = chatPanel.getChatConfig();
        this.lastStatus = null;
        contextUsageBar.reload();
        refresh();
    }

    /**
     * Refreshes the UI components with the latest data from the chat session.
     */
    public void refresh() {
        if (chat.isShutdown()) {
            if (refreshTimer.isRunning()) refreshTimer.stop();
            return;
        }
        
        StatusManager statusManager = chat.getStatusManager();
        ChatStatus currentStatus = statusManager.getCurrentStatus();
        long now = System.currentTimeMillis();
        Color statusColor = chatConfig.getColor(currentStatus);

        if (lastStatus != currentStatus && chatConfig.isAudioFeedbackEnabled()) {
            handleStatusSound(currentStatus);
        }
        this.lastStatus = currentStatus;

        statusIndicator.setColor(statusColor);
        statusLabel.setForeground(statusColor);
        statusLabel.setToolTipText(currentStatus.getDescription());
        
        String statusText = currentStatus.getDisplayName();
        if (currentStatus == ChatStatus.TOOL_EXECUTION_IN_PROGRESS && StringUtils.isNotBlank(statusManager.getExecutingToolName())) {
            statusText = String.format("%s (%s)", currentStatus.getDisplayName(), statusManager.getExecutingToolName());
        }
        
        if (currentStatus == ChatStatus.WAITING_WITH_BACKOFF) {
            long elapsedSinceBackoffStart = now - statusManager.getStatusChangeTime();
            long totalBackoffDuration = statusManager.getCurrentBackoffAmount();
            long remainingBackoff = totalBackoffDuration - elapsedSinceBackoffStart;
            if (remainingBackoff < 0) remainingBackoff = 0;

            statusLabel.setText(String.format("%s... (%s remaining)", statusText, TimeUtils.formatMillisConcise(remainingBackoff)));
        } else if (currentStatus.isActive()) {
            long duration = now - statusManager.getStatusChangeTime();
            statusLabel.setText(String.format("%s... (%s)", statusText, TimeUtils.formatMillisConcise(duration)));
        } else {
            long lastDuration = statusManager.getLastOperationDuration();
            if (lastDuration > 0) {
                statusLabel.setText(String.format("%s (took %s)", currentStatus.getDisplayName(), TimeUtils.formatMillisConcise(lastDuration)));
            } else {
                statusLabel.setText(currentStatus.getDisplayName());
            }
        }

        contextUsageBar.refresh();

        List<ApiErrorRecord> errors = statusManager.getApiErrors();
        Response<?> lastResponse = chat.getLastResponse().orElse(null);
        boolean isRetrying = !errors.isEmpty() && (currentStatus == ChatStatus.WAITING_WITH_BACKOFF || currentStatus == ChatStatus.API_CALL_IN_PROGRESS);

        rawJsonResponseLink.setVisible(false);
        rawJsonRequestConfigLink.setVisible(false);
        blockReasonLabel.setVisible(false);
        tokenDetailsLabel.setVisible(false);

        if (isRetrying) {
            apiErrorsPanel.setVisible(true);
            apiErrorsPanel.removeAll();
            apiErrorsPanel.setLayout(new GridLayout(0, 1));

            ApiErrorRecord lastError = errors.get(errors.size() - 1);
            long totalErrorTime = now - lastError.getTimestamp().toEpochMilli();
            String headerText = String.format("Retrying... Total Time: %s | Attempt: %d | Backoff: %s",
                                              TimeUtils.formatMillisConcise(totalErrorTime),
                                              lastError.getRetryAttempt() + 1,
                                              TimeUtils.formatMillisConcise(lastError.getBackoffAmount()));
            apiErrorsPanel.add(new JLabel(headerText));

            for (ApiErrorRecord error : errors) {
                String displayString = StringUtils.abbreviateMiddle(error.getException().toString(), " ... ", 108) ;
                String apiKeySuffix = StringUtils.right(error.getApiKey(), 4);
                String errorText = String.format("  • [%s] [..%s] %s",
                                                 TIME_FORMAT.format(error.getTimestamp().toEpochMilli()),
                                                 apiKeySuffix,
                                                 displayString);
                JLabel errorLabel = new JLabel(errorText);
                errorLabel.setForeground(Color.RED.darker());
                apiErrorsPanel.add(errorLabel);
            }
            
        } else if (lastResponse != null) {
            apiErrorsPanel.setVisible(false);
            apiErrorsPanel.removeAll();
            apiErrorsPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));

            rawJsonResponseLink.setVisible(true);
            rawJsonResponseLink.setContent(JacksonUtils.prettyPrintJsonString(lastResponse.getRawJson()));
            
            rawJsonRequestConfigLink.setVisible(true);
            rawJsonRequestConfigLink.setContent(JacksonUtils.prettyPrintJsonString(lastResponse.getRawRequestConfigJson()));
            
            tokenDetailsLabel.setVisible(true);

            lastResponse.getPromptFeedback().ifPresent(blockReason -> {
                blockReasonLabel.setText("Prompt Blocked: " + blockReason);
                blockReasonLabel.setVisible(true);
            });
            
            ResponseUsageMetadata usage = lastResponse.getUsageMetadata();
            if (usage != null) {
                String prompt = "Prompt: " + NUMBER_FORMAT.format(usage.getPromptTokenCount());
                String candidates = "Candidates: " + NUMBER_FORMAT.format(usage.getCandidatesTokenCount());
                String cached = "Cached: " + NUMBER_FORMAT.format(usage.getCachedContentTokenCount());
                String thoughts = "Thoughts: " + NUMBER_FORMAT.format(usage.getThoughtsTokenCount());
                String toolPrompt = "Tool Prompt: " + NUMBER_FORMAT.format(usage.getToolUsePromptTokenCount());
                String total = "Total: " + NUMBER_FORMAT.format(usage.getTotalTokenCount());

                tokenDetailsLabel.setText(String.join(" | ", prompt, candidates, cached, thoughts, toolPrompt, total));
            } else {
                tokenDetailsLabel.setText("");
            }
            
        } else {
            apiErrorsPanel.setVisible(false);
        }
        
        revalidate();
        repaint();
    }
    
    /**
     * Plays a sound notification based on the new status.
     * 
     * @param newStatus The new status.
     */
    private void handleStatusSound(ChatStatus newStatus) {
        String soundFileName = newStatus.name().toLowerCase() + ".wav";
        audioPlaybackPanel.playSound(soundFileName);
    }
    
    /**
     * A simple component that paints a colored circle to indicate status.
     */
    private static class StatusIndicator extends JComponent {
        /** The color of the indicator. */
        private Color color = Color.GRAY;

        /**
         * Constructs a new StatusIndicator.
         */
        public StatusIndicator() {
            setPreferredSize(new Dimension(16, 16));
        }

        /**
         * Sets the color of the indicator and triggers a repaint.
         * @param color The new color.
         */
        public void setColor(Color color) {
            this.color = color;
            repaint();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            Graphics2D g2d = (Graphics2D) g.create();
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2d.setColor(color);
            g2d.fillOval(2, 2, getWidth() - 4, getHeight() - 4);
            g2d.dispose();
        }
    }
}
