/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.chat;

import java.awt.Color;
import java.awt.Font;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.model.core.Role;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.model.tool.ToolPermission;
import uno.anahata.asi.status.ChatStatus;
import uno.anahata.asi.swing.chat.render.editorkit.EditorKitProvider;

/**
 * A concrete ChatConfig for standalone Swing applications, providing UI-specific settings like themes and colors.
 *
 * @author anahata
 */
@Getter @Setter
public class SwingChatConfig extends ChatConfig {
    
    private EditorKitProvider editorKitProvider;
    
    /**
     * If true, parts that are effectively pruned will still be rendered in the UI
     * (e.g., in a collapsed state) to allow the user to inspect and un-prune them.
     */
    private boolean showPrunedParts = true;
    
    /**
     * If true, sound notifications will be played on status changes.
     */
    private boolean audioFeedbackEnabled = true; 

    /**
     * Constructs a new SwingChatConfig with a randomly generated session ID.
     * 
     * @param aiConfig The global AI configuration.
     */
    public SwingChatConfig(AsiContainer aiConfig) {
        super(aiConfig);
    }

    /**
     * Constructs a new SwingChatConfig with a specific session ID.
     * 
     * @param aiConfig The global AI configuration.
     * @param sessionId The unique session ID.
     */
    public SwingChatConfig(AsiContainer aiConfig, String sessionId) {
        super(aiConfig, sessionId);
    }

    public static Color getColor(ChatStatus status) {
        return switch (status) {
            case API_CALL_IN_PROGRESS -> new Color(0, 123, 255); // BLUE
            case TOOL_PROMPT -> new Color(255, 193, 7); // AMBER
            case CANDIDATE_CHOICE_PROMPT -> new Color(23, 162, 184); // CYAN
            case AUTO_EXECUTING_TOOLS -> new Color(128, 0, 128); // PURPLE
            case TOOL_EXECUTION_ERROR -> Color.ORANGE; // ORANGE
            case WAITING_WITH_BACKOFF -> new Color(255, 0, 0); // RED
            case MAX_RETRIES_REACHED -> new Color(150, 0, 0); // DARK RED
            case ERROR -> new Color(100, 0, 0); // Even darker red for general error
            case SHUTDOWN -> Color.GRAY; // GRAY
            case IDLE -> new Color(0, 128, 0); // GREEN
        };
    }
    
    public static Color getColor(ToolExecutionStatus status) {
        if (status == null) return Color.GRAY;
        return switch (status) {
            case EXECUTED -> new Color(40, 167, 69); // Green
            case EXECUTING -> new Color(20, 157, 49); // Green
            case FAILED -> new Color(220, 53, 69);   // Red
            case INTERRUPTED -> new Color(255, 193, 7); // Amber
            case PENDING -> new Color(128, 0, 128);  // Purple
            case NOT_FOUND -> new Color(253, 126, 20); // Orange
            case NOT_EXECUTED -> new Color(108, 117, 125); // Gray
        };
    }

    public static Color getColor(ToolPermission permission) {
        if (permission == null) return Color.GRAY;
        return switch (permission) {
            case PROMPT -> new Color(0, 123, 255);        // Blue
            case APPROVE_ALWAYS -> new Color(40, 167, 69); // Green
            case DENY_NEVER -> new Color(220, 53, 69);     // Red
        };
    }

    public static Color getColorForContextUsage(double percentage) {
        if (percentage > 1.0) {
            return new Color(150, 0, 0); // Dark Red
        } else if (percentage > 0.9) {
            return new Color(255, 50, 50); // Red
        } else if (percentage > 0.7) {
            return new Color(255, 193, 7); // Yellow/Amber
        } else {
            return new Color(40, 167, 69); // Green
        }
    }

    public UITheme getTheme() {
        return new UITheme();
    }

    @Getter
    public static class UITheme {
        // General
        private final Color fontColor = Color.BLACK;
        private final Font monoFont = new Font("SF Mono", Font.PLAIN, 14);

        // Role-specific colors (for Message Headers)
        private final Color userHeaderBg = new Color(212, 237, 218);
        private final Color userContentBg = new Color(235, 250, 235); // More green, still faint
        private final Color userHeaderFg = new Color(21, 87, 36);
        private final Color userBorder = new Color(144, 198, 149);

        private final Color modelHeaderBg = new Color(221, 234, 248);
        private final Color modelContentBg = new Color(250, 252, 255); // Nearly white with minimal blue tint
        private final Color modelHeaderFg = new Color(0, 123, 255);
        private final Color modelBorder = new Color(160, 195, 232);

        private final Color toolHeaderBg = new Color(223, 213, 235);
        private final Color toolContentBg = new Color(250, 248, 252);
        private final Color toolHeaderFg = new Color(80, 60, 100);
        private final Color toolBorder = new Color(200, 180, 220);
        
        private final Color toolOutputFg = Color.GREEN;
        private final Color toolOutputBg = Color.BLACK;
        private final Color toolErrorFg = new Color(255, 80, 80); // Brighter Red (V1 style)
        private final Color toolErrorBg = new Color(51, 28, 28);  // Deep Burgundy (V1 style)
        private final Color toolLogsFg = Color.WHITE;
        private final Color toolLogsBg = Color.BLACK;

        // Part-specific colors (Faint and Role-Neutral)
        private final Color partHeaderBg = new Color(240, 240, 240, 100); // Faint gray
        private final Color partHeaderFg = new Color(100, 100, 100);
        private final Color partBorder = new Color(220, 220, 220, 150);
        
        private final Color thoughtFg = new Color(150, 150, 150); // Fainted gray for thoughts

        private final Color defaultHeaderBg = Color.WHITE;
        private final Color defaultContentBg = new Color(248, 249, 250);
        private final Color defaultBorder = Color.LIGHT_GRAY;

        // Function Call/Response
        private final Color functionCallBg = new Color(28, 37, 51);
        private final Color functionCallFg = new Color(0, 229, 255);
        private final Color functionResponseBg = Color.BLACK;
        private final Color functionResponseFg = new Color(0, 255, 0);
        private final Color functionErrorBg = new Color(51, 28, 28);
        private final Color functionErrorFg = new Color(255, 80, 80);
        
        // Grounding Metadata
        private final Color groundingHeaderBg = new Color(240, 248, 255);
        private final Color groundingContentBg = new Color(250, 252, 255);
        private final Color groundingDetailsHeaderBg = new Color(230, 240, 250);
        private final Color groundingDetailsHeaderColor = new Color(0, 50, 100);
        private final Color groundingDetailsContentBg = Color.WHITE;
        private final Color chipBackground = new Color(235, 245, 255);
        private final Color chipText = new Color(0, 100, 200);
        private final Color chipBorder = new Color(180, 210, 240);

        // Chat Card Colors
        private final Color cardNormalBg = new Color(255, 253, 208);
        private final Color cardHoverBg = new Color(255, 255, 225);
        private final Color cardSelectedBg = new Color(255, 245, 180);
        private final Color cardBorder = new Color(220, 220, 180);
        private final Color cardSelectedBorder = new Color(180, 160, 50);

        public Color getHeaderStartColor(Role role) {
            return switch (role) {
                case USER -> userHeaderBg;
                case MODEL -> modelHeaderBg;
                case TOOL -> toolHeaderBg;
                default -> defaultHeaderBg;
            };
        }

        public Color getHeaderEndColor(Role role) {
            return switch (role) {
                case USER -> userContentBg;
                case MODEL -> modelContentBg;
                case TOOL -> toolContentBg;
                default -> defaultContentBg;
            };
        }

        public Color getHeaderForegroundColor(Role role) {
            return switch (role) {
                case USER -> userHeaderFg;
                case MODEL -> modelHeaderFg;
                case TOOL -> toolHeaderFg;
                default -> Color.BLACK;
            };
        }

        public Color getBorderColor(Role role) {
            return switch (role) {
                case USER -> userBorder;
                case MODEL -> modelBorder;
                case TOOL -> toolBorder;
                default -> defaultBorder;
            };
        }
    }
}
