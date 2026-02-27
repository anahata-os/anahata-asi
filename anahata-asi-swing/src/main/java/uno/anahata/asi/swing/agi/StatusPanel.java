/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

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
import java.util.stream.Collectors;
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
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jdesktop.swingx.JXHyperlink;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.internal.TimeUtils;
import uno.anahata.asi.model.core.Response;
import uno.anahata.asi.model.core.ResponseUsageMetadata;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.status.ApiErrorRecord;
import uno.anahata.asi.status.AgiStatus;
import uno.anahata.asi.status.StatusManager;
import uno.anahata.asi.swing.components.CodeHyperlink;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.internal.EdtPropertyChangeListener;
import uno.anahata.asi.swing.internal.SwingUtils;
import uno.anahata.asi.swing.media.util.AudioPlaybackPanel;
import uno.anahata.asi.tool.ToolManager;

/**
 * A panel that displays the real-time status of the agi session, including
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

    /** The parent agi panel. */
    private final AgiPanel agiPanel;
    /** The active agi session. */
    private Agi agi;
    /** The agi configuration. */
    private SwingAgiConfig agiConfig;
    /** Timer for periodic UI refreshes. */
    private final Timer refreshTimer;
    /** The last known status, used to detect changes for audio feedback. */
    private AgiStatus lastStatus = null;

    /** Visual indicator for the current status. */
    private StatusIndicator statusIndicator;
    /** Label displaying the status text. */
    private JLabel statusLabel;
    /** Label displaying currently executing tools. */
    private JLabel executingToolsLabel;
    /** Progress bar showing context window usage. */
    private ContextUsageBar contextUsageBar;
    /** Panel for displaying API error and retry details. */
    private JPanel apiErrorsPanel; 
    /** Label for detailed token usage information. */
    private JLabel tokenDetailsLabel; 
    /** Hyperlink to view the raw JSON request configuration. */
    private CodeHyperlink rawJsonRequestConfigLink; 
    /** Hyperlink to view the conversation history as JSON. */
    private CodeHyperlink historyJsonLink;
    /** Hyperlink to view the raw JSON response. */
    private CodeHyperlink rawJsonResponseLink; 
    /** Toggle button for sound notifications. */
    private JToggleButton soundToggle;
    /** Panel for managing audio playback feedback. */
    private final AudioPlaybackPanel audioPlaybackPanel; 
    /** Label for displaying prompt blocking reasons. */
    private JLabel blockReasonLabel; 
    
    private EdtPropertyChangeListener executingCallsListener;

    /**
     * Constructs a new StatusPanel.
     * 
     * @param agiPanel The parent agi panel.
     */
    public StatusPanel(AgiPanel agiPanel) {
        super(new BorderLayout(10, 2));
        this.agiPanel = agiPanel;
        this.agi = agiPanel.getAgi();
        this.agiConfig = agiPanel.getAgiConfig();
        this.audioPlaybackPanel = new AudioPlaybackPanel(agiPanel);
        initComponents();
        
        this.refreshTimer = new Timer(1000, e -> refresh());
        this.executingCallsListener = new EdtPropertyChangeListener(this, agi.getToolManager(), "executingCalls", evt -> refresh());
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
        
        JPanel agiStatusPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        statusIndicator = new StatusIndicator();
        statusLabel = new JLabel("Initializing...");
        soundToggle = new JToggleButton(IconUtils.getIcon("bell.png"));
        soundToggle.setSelectedIcon(IconUtils.getIcon("bell_mute.png"));
        soundToggle.setToolTipText("Toggle Sound Notifications");
        soundToggle.setSelected(!agiConfig.isAudioFeedbackEnabled());
        soundToggle.addActionListener(e -> agiConfig.setAudioFeedbackEnabled(!soundToggle.isSelected()));
        agiStatusPanel.add(soundToggle);
        agiStatusPanel.add(statusIndicator);
        agiStatusPanel.add(statusLabel);
        
        executingToolsLabel = new JLabel();
        executingToolsLabel.setForeground(new Color(128, 0, 128)); // Purple
        executingToolsLabel.setFont(executingToolsLabel.getFont().deriveFont(java.awt.Font.BOLD));
        agiStatusPanel.add(executingToolsLabel);
        
        row1Panel.add(agiStatusPanel, BorderLayout.WEST);
        
        contextUsageBar = new ContextUsageBar(agiPanel); 
        row1Panel.add(contextUsageBar, BorderLayout.EAST);
        
        add(row1Panel);

        // --- Row 2 (Links) --- 
        JPanel row2Panel = new JPanel(new BorderLayout(10, 0));
        row2Panel.setAlignmentX(LEFT_ALIGNMENT);
        row2Panel.setBorder(BorderFactory.createEmptyBorder(2, 0, 2, 0));

        JPanel linksPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        
        rawJsonRequestConfigLink = new CodeHyperlink("Request", 
                () -> "Raw JSON Request", 
                () -> agi.getLastResponse().map(Response::getRawRequestConfigJson).orElse(""), 
                "json");
        linksPanel.add(rawJsonRequestConfigLink);

        historyJsonLink = new CodeHyperlink("History",
                () -> "Conversation History JSON",
                () -> agi.getLastResponse().map(Response::getRawHistoryJson).orElse(""),
                "json");
        linksPanel.add(historyJsonLink);

        rawJsonResponseLink = new CodeHyperlink("Response", 
                () -> "Raw JSON Response", 
                () -> agi.getLastResponse().map(r -> r.getRawJson()).orElse(""), 
                "json");
        linksPanel.add(rawJsonResponseLink);

        row2Panel.add(linksPanel, BorderLayout.WEST);
        row2Panel.add(audioPlaybackPanel, BorderLayout.EAST);
        
        add(row2Panel);

        // --- Row 3 (Token Details) ---
        JPanel row3Panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        row3Panel.setAlignmentX(LEFT_ALIGNMENT);
        tokenDetailsLabel = new JLabel();
        row3Panel.add(tokenDetailsLabel);
        add(row3Panel);

        // --- Row 4 (Block Reason) --- 
        JPanel row4Panel = new JPanel();
        row4Panel.setLayout(new BoxLayout(row4Panel, BoxLayout.X_AXIS));
        row4Panel.setAlignmentX(LEFT_ALIGNMENT);
        row4Panel.setBorder(BorderFactory.createEmptyBorder(2, 0, 2, 0));

        blockReasonLabel = new JLabel();
        blockReasonLabel.setForeground(Color.RED.darker());
        row4Panel.add(blockReasonLabel);
        row4Panel.add(Box.createHorizontalGlue());

        add(row4Panel);

        apiErrorsPanel = new JPanel();
        apiErrorsPanel.setAlignmentX(LEFT_ALIGNMENT);
        apiErrorsPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 0, 0));
        apiErrorsPanel.setVisible(false);
        add(apiErrorsPanel);
    }

    /**
     * Reloads the panel with the state of the current agi session.
     */
    public void reload() {
        this.agi = agiPanel.getAgi();
        this.agiConfig = agiPanel.getAgiConfig();
        this.lastStatus = null;
        
        if (executingCallsListener != null) {
            executingCallsListener.unbind();
        }
        this.executingCallsListener = new EdtPropertyChangeListener(this, agi.getToolManager(), "executingCalls", evt -> refresh());
        
        contextUsageBar.reload();
        refresh();
    }

    /**
     * Refreshes the UI components with the latest data from the agi session.
     */
    public void refresh() {
        if (agi.isShutdown()) {
            if (refreshTimer.isRunning()) refreshTimer.stop();
            return;
        }
        
        StatusManager statusManager = agi.getStatusManager();
        AgiStatus currentStatus = statusManager.getCurrentStatus();
        long now = System.currentTimeMillis();
        Color statusColor = agiConfig.getColor(currentStatus);

        if (lastStatus != currentStatus && agiConfig.isAudioFeedbackEnabled()) {
            handleStatusSound(currentStatus);
        }
        this.lastStatus = currentStatus;

        statusIndicator.setColor(statusColor);
        statusLabel.setForeground(statusColor);
        statusLabel.setToolTipText(currentStatus.getDescription());
        
        String statusText = currentStatus.getDisplayName();
        
        if (currentStatus == AgiStatus.WAITING_WITH_BACKOFF) {
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
        
        // Update Executing Tools Label
        ToolManager toolManager = agi.getToolManager();
        List<AbstractToolCall<?, ?>> executingCalls = toolManager.getExecutingCalls();
        if (!executingCalls.isEmpty()) {
            String tools = executingCalls.stream()
                    .map(AbstractToolCall::getToolName)
                    .collect(Collectors.joining(", "));
            executingToolsLabel.setText(" | Executing: " + tools);
            executingToolsLabel.setVisible(true);
        } else {
            executingToolsLabel.setVisible(false);
        }

        contextUsageBar.refresh();

        List<ApiErrorRecord> errors = statusManager.getApiErrors();
        Response<?> lastResponse = agi.getLastResponse().orElse(null);
        boolean isRetrying = !errors.isEmpty() && (currentStatus == AgiStatus.WAITING_WITH_BACKOFF || currentStatus == AgiStatus.API_CALL_IN_PROGRESS);

        rawJsonResponseLink.setVisible(false);
        rawJsonRequestConfigLink.setVisible(false);
        historyJsonLink.setVisible(false);
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
                String errorText = String.format("[%s] [..%s] %s",
                                                 TIME_FORMAT.format(error.getTimestamp().toEpochMilli()),
                                                 apiKeySuffix,
                                                 displayString);
                
                JXHyperlink errorLink = new JXHyperlink();
                errorLink.setText(errorText);
                errorLink.setToolTipText("Click to view full stack trace");
                errorLink.setForeground(Color.RED.darker());
                errorLink.addActionListener(e -> SwingUtils.showException(this, "API Error", error.getException().getMessage(), error.getException()));
                apiErrorsPanel.add(errorLink);
            }
            
        } else if (lastResponse != null) {
            apiErrorsPanel.setVisible(false);
            apiErrorsPanel.removeAll();
            apiErrorsPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));

            rawJsonResponseLink.setVisible(true);
            rawJsonRequestConfigLink.setVisible(true);
            historyJsonLink.setVisible(true);
            
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
                String toolPrompt = "Tools: " + NUMBER_FORMAT.format(usage.getToolUsePromptTokenCount());
                String total = "Billed Tokens: " + NUMBER_FORMAT.format(usage.getTotalTokenCount());

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
    private void handleStatusSound(AgiStatus newStatus) {
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
