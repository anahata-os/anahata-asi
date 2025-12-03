/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.swing.chat;

import java.awt.Color;
import java.awt.Font;
import lombok.Getter;
import uno.anahata.ai.AiConfig;
import uno.anahata.ai.chat.ChatConfig;
import uno.anahata.ai.status.ChatStatus;

/**
 * A concrete ChatConfig for standalone Swing applications, providing UI-specific settings like themes and colors.
 *
 * @author pablo
 */
public class SwingChatConfig extends ChatConfig {

    public SwingChatConfig(AiConfig aiConfig) {
        super(aiConfig, "standalone");
    }

    public static Color getColor(ChatStatus status) {
        switch (status) {
            case API_CALL_IN_PROGRESS:
                return new Color(0, 123, 255); // BLUE
            case TOOL_PROMPT:
                return new Color(255, 193, 7); // AMBER
            case TOOL_EXECUTION_IN_PROGRESS:
                return new Color(128, 0, 128); // PURPLE
            case WAITING_WITH_BACKOFF:
                return new Color(255, 0, 0); // RED
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

        // Role-specific colors
        private final Color userHeaderBg = new Color(212, 237, 218);
        private final Color userContentBg = new Color(233, 247, 239);
        private final Color userHeaderFg = new Color(21, 87, 36);
        private final Color userBorder = new Color(144, 198, 149);

        private final Color modelHeaderBg = new Color(221, 234, 248);
        private final Color modelContentBg = new Color(240, 248, 255);
        private final Color modelHeaderFg = new Color(0, 123, 255);
        private final Color modelBorder = new Color(160, 195, 232);

        private final Color toolHeaderBg = new Color(223, 213, 235);
        private final Color toolContentBg = new Color(250, 248, 252);
        private final Color toolHeaderFg = new Color(80, 60, 100);
        private final Color toolBorder = new Color(200, 180, 220);

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
    }
}
