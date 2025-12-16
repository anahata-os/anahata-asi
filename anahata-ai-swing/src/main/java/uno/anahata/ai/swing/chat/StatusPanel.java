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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
import lombok.Getter; // Added Getter for audioPlaybackPanel
import org.apache.commons.lang3.StringUtils;
import org.jdesktop.swingx.JXHyperlink;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.internal.JacksonUtils; // Added import
import uno.anahata.ai.internal.TimeUtils;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.ResponseUsageMetadata;
import uno.anahata.ai.status.ApiErrorRecord;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.status.StatusManager;
import uno.anahata.ai.swing.chat.render.CodeBlockSegmentRenderer;
import uno.anahata.ai.swing.icons.IconUtils;
import uno.anahata.ai.swing.media.util.AudioPlaybackPanel; // Added import

/**
 * A panel that displays the real-time status of the chat session, including
 * API call status, context usage, and error/retry information.
 *
 * @author anahata
 */
@Getter // Added Getter for audioPlaybackPanel
public class StatusPanel extends JPanel {
    private static final SimpleDateFormat TIME_FORMAT = new SimpleDateFormat("HH:mm:ss");
    private static final NumberFormat NUMBER_FORMAT = NumberFormat.getInstance();

    private final ChatPanel chatPanel;
    private final Chat chat;
    private final SwingChatConfig chatConfig;
    private final Timer refreshTimer;
    private ChatStatus lastStatus = null;

    // UI Components
    private StatusIndicator statusIndicator;
    private JLabel statusLabel;
    private ContextUsageBar contextUsageBar;
    private JPanel apiErrorsPanel; // Renamed from detailsPanel
    private JLabel tokenDetailsLabel; // For Section 3
    private JXHyperlink rawJsonRequestConfigLink; // New: For Section 3
    private JXHyperlink rawJsonResponseLink; // For Section 5 (now Section 3)
    private JToggleButton soundToggle;
    private final AudioPlaybackPanel audioPlaybackPanel; // For Section 4
    private JLabel blockReasonLabel; // For Section 5 (now Section 3/Row 3)

    public StatusPanel(ChatPanel chatPanel) { // Modified constructor
        super(new BorderLayout(10, 2));
        this.chatPanel = chatPanel;
        this.chat = chatPanel.getChat();
        this.chatConfig = chatPanel.getChatConfig();
        this.audioPlaybackPanel = new AudioPlaybackPanel(chatPanel); // Initialize here
        initComponents();
        
        this.refreshTimer = new Timer(1000, e -> refresh());
    }

    @Override
    public void addNotify() {
        super.addNotify();
        refreshTimer.start();
    }

    @Override
    public void removeNotify() {
        refreshTimer.stop();
        super.removeNotify();
    }

    private void initComponents() {
        setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 5));

        // Use BoxLayout (Y_AXIS) for the main StatusPanel to stack the rows vertically
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        // --- Row 1 (Top) --- 
        JPanel row1Panel = new JPanel(new BorderLayout(10, 0));
        row1Panel.setAlignmentX(LEFT_ALIGNMENT);
        
        // Section 1: Top Left (soundToggle, statusIndicator, statusLabel)
        JPanel chatStatusPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0)); // Renamed
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
        
        // Section 2: Top Right (contextUsageBar)
        contextUsageBar = new ContextUsageBar(chatPanel); 
        row1Panel.add(contextUsageBar, BorderLayout.EAST); // Directly add contextUsageBar
        
        add(row1Panel);

        // --- Row 2 (Middle) --- 
        JPanel row2Panel = new JPanel(new BorderLayout(10, 0));
        row2Panel.setAlignmentX(LEFT_ALIGNMENT);
        row2Panel.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0)); // Padding between rows

        // Section 3: Second line aligned to the left (rawJsonRequestConfigLink, rawJsonResponseLink, tokenUsage)
        JPanel tokenAndJsonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0)); // New panel
        
        rawJsonRequestConfigLink = new JXHyperlink(); // Initialize new hyperlink
        rawJsonRequestConfigLink.setText("Request Config");
        rawJsonRequestConfigLink.setToolTipText("View raw JSON request configuration");
        rawJsonRequestConfigLink.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (chat.getLastResponse().isPresent()) {
                    String rawJson = chat.getLastResponse().get().getRawRequestConfigJson();
                    String prettyPrintedJson = JacksonUtils.prettyPrintJsonString(rawJson);
                    new CodeBlockSegmentRenderer(chatPanel, prettyPrintedJson, "json").showInPopup("Raw JSON Request Config");
                }
            }
        });
        tokenAndJsonPanel.add(rawJsonRequestConfigLink); // Add new hyperlink first

        rawJsonResponseLink = new JXHyperlink();
        rawJsonResponseLink.setText("Response");
        rawJsonResponseLink.setToolTipText("View raw JSON response");
        rawJsonResponseLink.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (chat.getLastResponse().isPresent()) {
                    String rawJson = chat.getLastResponse().get().getRawJson();
                    String prettyPrintedJson = JacksonUtils.prettyPrintJsonString(rawJson); // Use JacksonUtils.prettyPrintJsonString
                    new CodeBlockSegmentRenderer(chatPanel, prettyPrintedJson, "json").showInPopup("Raw JSON Response");
                }
            }
        });
        tokenAndJsonPanel.add(rawJsonResponseLink); // Add response link second

        tokenDetailsLabel = new JLabel();
        tokenAndJsonPanel.add(tokenDetailsLabel); // Add token details last

        row2Panel.add(tokenAndJsonPanel, BorderLayout.WEST); // Add the new panel

        // Section 4: Second line aligned to the right (AudioPlaybackPanel)
        row2Panel.add(audioPlaybackPanel, BorderLayout.EAST); // Directly add audioPlaybackPanel
        
        add(row2Panel);

        // --- Row 3 (Bottom-Middle) --- 
        // Section 5 (now Row 3): block reason ALL in one long horizontal box layout
        JPanel responseDetailsPanel = new JPanel(); // Renamed
        responseDetailsPanel.setLayout(new BoxLayout(responseDetailsPanel, BoxLayout.X_AXIS));
        responseDetailsPanel.setAlignmentX(LEFT_ALIGNMENT);
        responseDetailsPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0)); // Padding

        blockReasonLabel = new JLabel(); // New label for block reason
        blockReasonLabel.setForeground(Color.RED.darker());
        responseDetailsPanel.add(blockReasonLabel);
        responseDetailsPanel.add(Box.createHorizontalGlue()); // Push everything to the left

        add(responseDetailsPanel);

        // Section 6 (Fourth row, uses all available horizontal space)
        // API error/retry information (multiple lines, GridLayout). displays only if there are api errors
        apiErrorsPanel = new JPanel(); // Renamed
        apiErrorsPanel.setAlignmentX(LEFT_ALIGNMENT);
        apiErrorsPanel.setBorder(BorderFactory.createEmptyBorder(5, 0, 0, 0)); // Top padding
        apiErrorsPanel.setVisible(false);
        add(apiErrorsPanel);
    }

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

        // 1. Update Status Indicator and Label
        statusIndicator.setColor(statusColor);
        statusLabel.setForeground(statusColor);
        statusLabel.setToolTipText(currentStatus.getDescription());
        
        String statusText = currentStatus.getDisplayName();
        if (currentStatus == ChatStatus.TOOL_EXECUTION_IN_PROGRESS && StringUtils.isNotBlank(statusManager.getExecutingToolName())) {
            statusText = String.format("%s (%s)", currentStatus.getDisplayName(), statusManager.getExecutingToolName());
        }
        
        if (currentStatus != ChatStatus.IDLE) {
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

        // 2. Refresh Context Usage Bar
        contextUsageBar.refresh();

        // 3. Update Details Panel (Section 6) and Section 5 labels
        List<ApiErrorRecord> errors = statusManager.getApiErrors();
        Response lastResponse = chat.getLastResponse().orElse(null);
        boolean isRetrying = !errors.isEmpty() && (currentStatus == ChatStatus.WAITING_WITH_BACKOFF || currentStatus == ChatStatus.API_CALL_IN_PROGRESS);

        // Reset visibility for Section 3/Row 3 labels
        rawJsonResponseLink.setVisible(false);
        rawJsonRequestConfigLink.setVisible(false); // New: Reset visibility for request config link
        blockReasonLabel.setVisible(false);
        tokenDetailsLabel.setVisible(false);

        if (isRetrying) {
            apiErrorsPanel.setVisible(true);
            apiErrorsPanel.removeAll(); // Clear previous content
            apiErrorsPanel.setLayout(new GridLayout(0, 1)); // Layout for errors

            ApiErrorRecord lastError = errors.get(errors.size() - 1);
            long totalErrorTime = now - lastError.getTimestamp().toEpochMilli();
            String headerText = String.format("Retrying... Total Time: %s | Attempt: %d | Next Backoff: %dms",
                                              TimeUtils.formatMillisConcise(totalErrorTime),
                                              lastError.getRetryAttempt() + 1,
                                              lastError.getBackoffAmount());
            apiErrorsPanel.add(new JLabel(headerText));

            for (ApiErrorRecord error : errors) {
                
                String displayString = StringUtils.abbreviateMiddle(error.getException().toString(), " ... ", 108) ;
                
                String errorText = String.format("  • [%s] [..%s] %s",
                                                 TIME_FORMAT.format(error.getTimestamp().toEpochMilli()),
                                                 error.getApiKey(),
                                                 displayString);
                JLabel errorLabel = new JLabel(errorText);
                errorLabel.setForeground(Color.RED.darker());
                apiErrorsPanel.add(errorLabel);
            }
            
        } else if (lastResponse != null) {
            apiErrorsPanel.setVisible(false); // Hide error panel if no errors
            apiErrorsPanel.removeAll(); // Clear any previous error messages
            apiErrorsPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0)); // Reset layout for potential future use

            // Section 3/Row 3: Display relevant info
            rawJsonResponseLink.setVisible(true);
            rawJsonRequestConfigLink.setVisible(true); // New: Show request config link
            tokenDetailsLabel.setVisible(true);

            lastResponse.getPromptFeedback().ifPresent(blockReason -> {
                blockReasonLabel.setText("Prompt Blocked: " + blockReason);
                blockReasonLabel.setVisible(true);
            });
            
            // Display Token Usage (Section 3)
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
    
    private void handleStatusSound(ChatStatus newStatus) {
        String soundFileName = newStatus.name().toLowerCase() + ".wav";
        audioPlaybackPanel.playSound(soundFileName);
    }
    
    /**
     * A simple component that paints a colored circle.
     */
    private static class StatusIndicator extends JComponent {
        private Color color = Color.GRAY;

        public StatusIndicator() {
            setPreferredSize(new Dimension(16, 16));
        }

        public void setColor(Color color) {
            this.color = color;
            repaint();
        }

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