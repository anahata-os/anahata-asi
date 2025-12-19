/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.Color;
import java.awt.Font;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.chat.ChatConfig;
import uno.anahata.ai.model.core.Role;
import uno.anahata.ai.status.ChatStatus;
import uno.anahata.ai.swing.chat.render.editorkit.DefaultEditorKitProvider;
import uno.anahata.ai.swing.chat.render.editorkit.EditorKitProvider;

/**
 * A concrete ChatConfig for standalone Swing applications, providing UI-specific settings like themes and colors.
 *
 * @author pablo
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
    private boolean audioFeedbackEnabled = true; // Added field

    public SwingChatConfig(AiConfig aiConfig) {
        super(aiConfig, "standalone");
        //this.editorKitProvider = new DefaultEditorKitProvider();
    }

    public static Color getColor(ChatStatus status) {
        switch (status) {
            case API_CALL_IN_PROGRESS:
                return new Color(0, 123, 255); // BLUE
            case TOOL_PROMPT:
                return new Color(255, 193, 7); // AMBER
            case CANDIDATE_CHOICE_PROMPT:
                return new Color(23, 162, 184); // CYAN
            case TOOL_EXECUTION_IN_PROGRESS:
                return new Color(128, 0, 128); // PURPLE
            case TOOL_EXECUTION_ERROR:
                return Color.ORANGE; // ORANGE
            case WAITING_WITH_BACKOFF:
                return new Color(255, 0, 0); // RED
            case MAX_RETRIES_REACHED:
                return new Color(150, 0, 0); // DARK RED
            case ERROR:
                return new Color(100, 0, 0); // Even darker red for general error
            case SHUTDOWN:
                return Color.GRAY; // GRAY
            case IDLE:
                return new Color(0, 128, 0); // GREEN
            default:
                return Color.BLACK;
        }
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
        private final Color userContentBg = new Color(233, 247, 239);
        private final Color userHeaderFg = new Color(21, 87, 36);
        private final Color userBorder = new Color(144, 198, 149);

        private final Color modelHeaderBg = new Color(221, 234, 248);
        private final Color modelContentBg = Color.WHITE;
        private final Color modelHeaderFg = new Color(0, 123, 255);
        private final Color modelBorder = new Color(160, 195, 232);

        private final Color toolHeaderBg = new Color(223, 213, 235);
        private final Color toolContentBg = new Color(250, 248, 252);
        private final Color toolHeaderFg = new Color(80, 60, 100);
        private final Color toolBorder = new Color(200, 180, 220);

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
