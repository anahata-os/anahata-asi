/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.Color;
import java.awt.Font;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.Role;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.swing.agi.message.part.tool.param.FullTextFileCreateRenderer;
import uno.anahata.asi.swing.agi.message.part.tool.param.ParameterRendererFactory;
import uno.anahata.asi.swing.icons.IconProvider;
import uno.anahata.asi.swing.toolkit.Screens;
import uno.anahata.asi.swing.toolkit.SwingJava;
import uno.anahata.asi.toolkit.Java;
import uno.anahata.asi.toolkit.files.FullTextFileCreate;
import uno.anahata.asi.yam.tools.Chrome;
import uno.anahata.asi.yam.tools.Radio;
import uno.anahata.asi.yam.tools.Speech;

/**
 * A concrete AgiConfig for standalone Swing applications, providing UI-specific settings like themes and colors.
 *
 * @author anahata
 */
@Getter @Setter
public class SwingAgiConfig extends AgiConfig {
    
    static {
        ParameterRendererFactory.register(FullTextFileCreate.class, FullTextFileCreateRenderer.class);
    }
    
    /** 
     * The provider for context-related icons, used to visually distinguish between 
     * different types of resources and tools. 
     */
    private IconProvider iconProvider;
    
    /**
     * If true, parts and messages that are effectively pruned will still be rendered in the UI
     * (e.g., in a collapsed state) to allow the user to inspect and un-prune them.
     */
    private boolean showPruned = false;
    
    /**
     * If true, sound notifications will be played on status changes.
     */
    private boolean audioFeedbackEnabled = true; 

    {
        // Register the Yam tools for all Swing-based configurations
        getToolClasses().remove(Java.class);
        getToolClasses().add(SwingJava.class);
        getToolClasses().add(Radio.class);
        getToolClasses().add(Speech.class);
        getToolClasses().add(Chrome.class);
        getToolClasses().add(Screens.class);
    }

    /**
     * Constructs a new SwingAgiConfig with a randomly generated session ID.
     * 
     * @param aiConfig The global AI configuration.
     */
    public SwingAgiConfig(AsiContainer aiConfig) {
        super(aiConfig);
    }

    /**
     * Constructs a new SwingAgiConfig with a specific session ID.
     * 
     * @param aiConfig The global AI configuration.
     * @param sessionId The unique session ID.
     */
    public SwingAgiConfig(AsiContainer aiConfig, String sessionId) {
        super(aiConfig, sessionId);
    }

    /**
     * Sets whether pruned parts and messages should be shown in the UI.
     * Fires a property change event for reactive UI updates.
     * 
     * @param showPruned true to show pruned content.
     */
    public void setShowPruned(boolean showPruned) {
        boolean old = this.showPruned;
        this.showPruned = showPruned;
        propertyChangeSupport.firePropertyChange("showPruned", old, showPruned);
    }

    /** 
     * Retrieves the thematic color associated with a specific agi status.
     * 
     * @param status The status to look up.
     * @return The color representing that status in the UI.
     */
    public static Color getColor(AgiStatus status) {
        return switch (status) {
            case AWAKENING_KUNDALINI -> new Color(80, 200, 120); // Emerald Green
            case API_CALL_IN_PROGRESS -> new Color(0, 123, 255); // BLUE
            case TOOL_PROMPT -> new Color(170, 75, 45); // GERU (Ochre)
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
    
    /** 
     * Retrieves the color associated with a specific tool execution status.
     * 
     * @param status The status to look up.
     * @return The color representing that execution state.
     */
    public static Color getColor(ToolExecutionStatus status) {
        if (status == null) return Color.GRAY;
        return switch (status) {
            case EXECUTED -> new Color(40, 167, 69); // Green
            case EXECUTING -> new Color(20, 157, 49); // Green
            case FAILED -> new Color(220, 53, 69);   // Red
            case INTERRUPTED -> new Color(255, 193, 7); // Amber
            case PENDING -> new Color(128, 0, 128);  // Purple
            case NOT_FOUND -> new Color(253, 126, 20); // Orange
            case DECLINED -> new Color(108, 117, 125); // Gray
        };
    }

    /** 
     * Retrieves the color associated with a specific tool permission level.
     * 
     * @param permission The permission level.
     * @return The color representing that permission.
     */
    public static Color getColor(ToolPermission permission) {
        if (permission == null) return Color.GRAY;
        return switch (permission) {
            case PROMPT -> new Color(0, 123, 255);        // Blue
            case APPROVE_ALWAYS -> new Color(40, 167, 69); // Green
            case DENY_NEVER -> new Color(220, 53, 69);     // Red
        };
    }

    /** 
     * Calculates a color representing context window usage, shifting from green 
     * to red as the usage approaches the threshold.
     * 
     * @param percentage The usage percentage (0.0 to 1.0+).
     * @return The appropriate color for the usage bar.
     */
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

    /** 
     * Returns a new theme object containing the color and font definitions for the UI.
     * @return The UI theme.
     */
    public UITheme getTheme() {
        return new UITheme();
    }

    /** 
     * A collection of color and font definitions that define the visual 
     * identity of the Anahata Swing UI.
     */
    @Getter
    public static class UITheme {
        /** The primary foreground color for general text. */
        private final Color fontColor = Color.BLACK;
        /** The fixed-width font used for code blocks and monospaced text segments. */
        private final Font monoFont = new Font("SF Mono", Font.PLAIN, 14);

        /** Background color for the user message header. */
        private final Color userHeaderBg = new Color(212, 237, 218);
        /** Background color for the user message content area. */
        private final Color userContentBg = new Color(235, 250, 235);
        /** Foreground color for the user message header text. */
        private final Color userHeaderFg = new Color(21, 87, 36);
        /** Border color for user message panels. */
        private final Color userBorder = new Color(144, 198, 149);

        /** Background color for the model message header. */
        private final Color modelHeaderBg = new Color(221, 234, 248);
        /** Background color for the model message content area. */
        private final Color modelContentBg = new Color(250, 252, 255);
        /** Foreground color for the model message header text. */
        private final Color modelHeaderFg = new Color(0, 123, 255);
        /** Border color for model message panels. */
        private final Color modelBorder = new Color(160, 195, 232);

        /** Background color for tool/system message headers. */
        private final Color toolHeaderBg = new Color(223, 213, 235);
        /** Background color for tool/system message content areas. */
        private final Color toolContentBg = new Color(250, 248, 252);
        /** Foreground color for tool/system message header text. */
        private final Color toolHeaderFg = new Color(80, 60, 100);
        /** Border color for tool/system message panels. */
        private final Color toolBorder = new Color(200, 180, 220);
        
        /** Foreground color for standard tool output text. */
        private final Color toolOutputFg = Color.GREEN;
        /** Background color for standard tool output text area. */
        private final Color toolOutputBg = Color.BLACK;
        /** Foreground color for tool error output. */
        private final Color toolErrorFg = new Color(255, 80, 80);
        /** Background color for tool error output area. */
        private final Color toolErrorBg = new Color(51, 28, 28);
        /** Foreground color for tool log messages. */
        private final Color toolLogsFg = Color.WHITE;
        /** Background color for tool logs area. */
        private final Color toolLogsBg = Color.BLACK;

        /** Background color for individual message part headers. */
        private final Color partHeaderBg = new Color(240, 240, 240, 100);
        /** Foreground color for message part header text. */
        private final Color partHeaderFg = new Color(100, 100, 100);
        /** Border color for message part panels. */
        private final Color partBorder = new Color(220, 220, 220, 150);
        
        /** Foreground color for model thought/reasoning text. */
        private final Color thoughtFg = new Color(150, 150, 150);

        /** Default background color for message headers if role is undefined. */
        private final Color defaultHeaderBg = Color.WHITE;
        /** Default background color for message content areas. */
        private final Color defaultContentBg = new Color(248, 249, 250);
        /** Default border color for message panels. */
        private final Color defaultBorder = Color.LIGHT_GRAY;

        /** Background color for function call visualization. */
        private final Color functionCallBg = new Color(28, 37, 51);
        /** Foreground color for function call text. */
        private final Color functionCallFg = new Color(0, 229, 255);
        /** Background color for function response visualization. */
        private final Color functionResponseBg = Color.BLACK;
        /** Foreground color for function response text. */
        private final Color functionResponseFg = new Color(0, 255, 0);
        /** Background color for function error visualization. */
        private final Color functionErrorBg = new Color(51, 28, 28);
        /** Foreground color for function error text. */
        private final Color functionErrorFg = new Color(255, 80, 80);
        
        /** Background color for grounding metadata headers. */
        private final Color groundingHeaderBg = new Color(240, 248, 255);
        /** Background color for grounding metadata content. */
        private final Color groundingContentBg = new Color(250, 252, 255);
        /** Background color for grounding source details header. */
        private final Color groundingDetailsHeaderBg = new Color(230, 240, 250);
        /** Foreground color for grounding source details header text. */
        private final Color groundingDetailsHeaderColor = new Color(0, 50, 100);
        /** Background color for grounding source details content. */
        private final Color groundingDetailsContentBg = Color.WHITE;
        /** Background color for interactive grounding chips. */
        private final Color chipBackground = new Color(235, 245, 255);
        /** Text color for interactive grounding chips. */
        private final Color chipText = new Color(0, 100, 200);
        /** Border color for interactive grounding chips. */
        private final Color chipBorder = new Color(180, 210, 240);

        /** Default background color for agi cards in the selection grid. */
        private final Color cardNormalBg = new Color(255, 253, 208);
        /** Background color for agi cards on mouse hover. */
        private final Color cardHoverBg = new Color(255, 255, 225);
        /** Background color for the currently selected agi card. */
        private final Color cardSelectedBg = new Color(255, 245, 180);
        /** Border color for standard agi cards. */
        private final Color cardBorder = new Color(220, 220, 180);
        /** Border color for the selected agi card. */
        private final Color cardSelectedBorder = new Color(180, 160, 50);
        
        /** 
         * Gets the background color for the start of a message header based on the role.
         * @param role The actor's role.
         * @return The header start color.
         */
        public Color getHeaderStartColor(Role role) {
            return switch (role) {
                case USER -> userHeaderBg;
                case MODEL -> modelHeaderBg;
                case TOOL -> toolHeaderBg;
                default -> defaultHeaderBg;
            };
        }

        /** 
         * Gets the background color for the end of a message header (and content area) based on the role.
         * @param role The actor's role.
         * @return The header end color.
         */
        public Color getHeaderEndColor(Role role) {
            return switch (role) {
                case USER -> userContentBg;
                case MODEL -> modelContentBg;
                case TOOL -> toolContentBg;
                default -> defaultContentBg;
            };
        }

        /** 
         * Gets the foreground (text) color for a message header based on the role.
         * @param role The actor's role.
         * @return The header text color.
         */
        public Color getHeaderForegroundColor(Role role) {
            return switch (role) {
                case USER -> userHeaderFg;
                case MODEL -> modelHeaderFg;
                case TOOL -> toolHeaderFg;
                default -> Color.BLACK;
            };
        }

        /** 
         * Gets the border color for a message panel based on the role.
         * @param role The actor's role.
         * @return The border color.
         */
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
