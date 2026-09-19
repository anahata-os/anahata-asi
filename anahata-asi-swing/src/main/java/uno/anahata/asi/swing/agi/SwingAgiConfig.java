/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.swing.agi;

import java.awt.Color;
import java.awt.Font;
import java.awt.Insets;
import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.UIManager;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.Role;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.status.AgiStatus;
import uno.anahata.asi.swing.icons.ActionIconKey;
import uno.anahata.asi.swing.icons.AddIcon;
import uno.anahata.asi.swing.icons.AttachIcon;
import uno.anahata.asi.swing.icons.AutoReplyIcon;
import uno.anahata.asi.swing.icons.BellIcon;
import uno.anahata.asi.swing.icons.CancelIcon;
import uno.anahata.asi.swing.icons.CardsIcon;
import uno.anahata.asi.swing.icons.CloneIcon;
import uno.anahata.asi.swing.icons.CompressIcon;
import uno.anahata.asi.swing.icons.CopyIcon;
import uno.anahata.asi.swing.icons.DeleteIcon;
import uno.anahata.asi.swing.icons.EditIcon;
import uno.anahata.asi.swing.icons.ExternalIcon;
import uno.anahata.asi.swing.icons.FramesIcon;
import uno.anahata.asi.swing.icons.IconProvider;
import uno.anahata.asi.swing.icons.IconUtils;
import uno.anahata.asi.swing.icons.LinkIcon;
import uno.anahata.asi.swing.icons.LoadSessionIcon;
import uno.anahata.asi.swing.icons.NewIcon;
import uno.anahata.asi.swing.icons.NextIcon;
import uno.anahata.asi.swing.icons.OkIcon;
import uno.anahata.asi.swing.icons.PinnedIcon;
import uno.anahata.asi.swing.icons.PrevIcon;
import uno.anahata.asi.swing.icons.PulseIcon;
import uno.anahata.asi.swing.icons.RestartIcon;
import uno.anahata.asi.swing.icons.RunAndSendIcon;
import uno.anahata.asi.swing.icons.SaveIcon;
import uno.anahata.asi.swing.icons.ScreenShareIcon;
import uno.anahata.asi.swing.icons.ScreenshotIcon;
import uno.anahata.asi.swing.icons.SearchIcon;
import uno.anahata.asi.swing.icons.SendIcon;
import uno.anahata.asi.swing.icons.ServerToolsIcon;
import uno.anahata.asi.swing.icons.SettingsIcon;
import uno.anahata.asi.swing.icons.StopIcon;
import uno.anahata.asi.swing.icons.TableIcon;
import uno.anahata.asi.swing.icons.TemplatesIcon;
import uno.anahata.asi.swing.toolkit.Screens;
import uno.anahata.asi.swing.toolkit.DesktopJava;
import uno.anahata.asi.toolkit.java.Java;
import uno.anahata.asi.yam.tools.chrome.Chrome;
import uno.anahata.asi.yam.tools.firefox.Firefox;
import uno.anahata.asi.yam.tools.Radio;
import uno.anahata.asi.yam.tools.Speech;
import uno.anahata.asi.swing.toolkit.benchmarks.Benchmarks;
import uno.anahata.asi.yam.tools.youtube.YouTube;

/**
 * A concrete {@link AgiConfig} implementation for standalone Swing applications, 
 * providing UI-specific configurations, look-and-feel adaptive themes, and color schemas.
 *
 * @author anahata
 */
@Getter @Setter
public class SwingAgiConfig extends AgiConfig {
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
        // Replace java for swing java
        getToolClasses().remove(Java.class);
        getToolClasses().add(DesktopJava.class);
        getToolClasses().add(Screens.class);
        // Add yam tools
        getToolClasses().add(Radio.class);        
        getToolClasses().add(Chrome.class);
        getToolClasses().add(Firefox.class);
        getToolClasses().add(YouTube.class);
        getToolClasses().add(Benchmarks.class);
        getToolClasses().add(Speech.class);
        //getToolClasses().add(OldChrome.class);
        
    }

    /**
     * Constructs a new SwingAgiConfig with a randomly generated session ID.
     *
     * @param aiConfig The global AI container instance.
     */
    public SwingAgiConfig(AbstractAsiContainer aiConfig) {
        super(aiConfig);
    }

    /**
     * Constructs a new SwingAgiConfig with a specific session ID.
     *
     * @param aiConfig The global AI container instance.
     * @param sessionId The unique session ID.
     */
    public SwingAgiConfig(AbstractAsiContainer aiConfig, String sessionId) {
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
     * Optional host-supplied detector for dark mode. When a host IDE (e.g. IntelliJ IDEA) knows its
     * own theme authoritatively, it registers a detector here so the shared Swing UI follows the IDE
     * theme exactly instead of relying on the built-in luminance heuristic. {@code null} (the
     * default, e.g. in the NetBeans and standalone Desktop hosts) falls back to the heuristic.
     */
    private static java.util.function.BooleanSupplier darkModeDetector;

    /**
     * Registers a host-specific dark-mode detector, overriding the built-in luminance heuristic.
     * <p>
     * Intended for IDE hosts whose Look and Feel does not expose a reliable {@code Panel.background}
     * to the heuristic (for instance IntelliJ IDEA's New UI): the host passes a supplier backed by
     * its own theme API so {@link #isDarkLaf()} tracks the IDE theme precisely. Passing {@code null}
     * restores the heuristic.
     *
     * @param detector the dark-mode detector, or {@code null} to use the built-in heuristic.
     */
    public static void setDarkModeDetector(java.util.function.BooleanSupplier detector) {
        darkModeDetector = detector;
    }

    /**
     * Determines whether the active theme is a dark-mode variant.
     * <p>
     * Uses the host-supplied {@linkplain #setDarkModeDetector(java.util.function.BooleanSupplier)
     * dark-mode detector} when one is registered; otherwise falls back to a Look-and-Feel-agnostic
     * relative-luminance check on {@code Panel.background}.
     *
     * @return true if the active theme is dark, false otherwise.
     */
    public static boolean isDarkLaf() {
        java.util.function.BooleanSupplier detector = darkModeDetector;
        if (detector != null) {
            return detector.getAsBoolean();
        }
        Color bg = UIManager.getColor("Panel.background");
        if (bg == null) {
            return false;
        }
        double luminance = (0.2126 * bg.getRed() + 0.7152 * bg.getGreen() + 0.0722 * bg.getBlue()) / 255.0;
        return luminance < 0.5;
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
            case DENY -> new Color(220, 53, 69);     // Red
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
     * Gets the unselected text color for truncated resources.
     * Can be overridden by host-specific configurations (e.g. IntellijAgiConfig).
     *
     * @return The theme-adaptive warning color.
     */
    public Color getTruncatedTokenColor() {
        return isDark() ? new Color(255, 175, 45) : new Color(215, 85, 0);
    }

    /**
     * Gets the selected text color for truncated resources.
     * High-contrast luminous gold across dark and light selection backgrounds.
     * Can be overridden by host-specific configurations (e.g. IntellijAgiConfig).
     *
     * @return The theme-adaptive selected warning color.
     */
    public Color getTruncatedSelectedColor() {
        return isDark() ? new Color(255, 215, 75) : new Color(255, 235, 120);
    }

    /**
     * Gets the background tint color for truncated resource rows.
     * Can be overridden by host-specific configurations (e.g. IntellijAgiConfig).
     *
     * @return The theme-adaptive background tint.
     */
    public Color getTruncatedTokenColorBackground() {
        return isDark() ? new Color(48, 36, 18) : new Color(255, 243, 225);
    }

    /**
     * Resolves the icon for a semantic action key at the specified nominal size.
     * <p>
     * By default, returns the built-in Anahata vector icons. Host-specific configurations
     * (such as {@code IntellijAgiConfig}) override this to return native platform icons
     * (such as IntelliJ's theme-adaptive {@code AllIcons}).
     * </p>
     *
     * @param key The semantic action key.
     * @param size The nominal icon dimension in pixels.
     * @return The resolved icon for the action.
     */
    public Icon getActionIcon(@NonNull ActionIconKey key, int size) {
        return switch (key) {
            case CANCEL -> new CancelIcon(size);
            case DELETE -> new DeleteIcon(size);
            case SAVE -> new SaveIcon(size);
            case EDIT -> new EditIcon(size);
            case EDIT_STAGED, REFRESH, CLEAR_HISTORY -> new RestartIcon(size);
            case COPY -> new CopyIcon(size);
            case SEND -> new SendIcon(size);
            case RUN_AND_SEND -> new RunAndSendIcon(size);
            case STOP -> new StopIcon(size);
            case ATTACH -> new AttachIcon(size);
            case LINK -> new LinkIcon(size);
            case SCREENSHOT -> new ScreenshotIcon(size);
            case SEARCH, OPEN_SESSION -> new SearchIcon(size);
            case NEW_SESSION -> new NewIcon(size);
            case IMPORT -> new LoadSessionIcon(size);
            case SETTINGS -> new SettingsIcon(size);
            case EXTERNAL -> new ExternalIcon(size);
            case OPEN_IN_IDE, NEXT -> new NextIcon(size);
            case PIN -> new PinnedIcon(size);
            case LOCAL_TOOLS -> IconUtils.getIcon("java.png", size);
            case SERVER_TOOLS -> new ServerToolsIcon(size);
            case AUTO_REPLY -> new AutoReplyIcon(size);
            case TEST_CONNECTION -> new PulseIcon(size);
            case CLONE -> new CloneIcon(size);
            case CAPTURE_WINDOW -> new FramesIcon(size);
            case SCREEN_SHARE -> new ScreenShareIcon(size);
            case ADD -> new AddIcon(size);
            case OK -> new OkIcon(size);
            case PREV -> new PrevIcon(size);
            case CARDS_VIEW -> new CardsIcon(size);
            case TABLE_VIEW -> new TableIcon(size);
            case COMPRESS -> new CompressIcon(size);
            case TEMPLATES -> new TemplatesIcon(size);
            case BELL -> new BellIcon(size, false);
            case BELL_MUTE -> new BellIcon(size, true);
        };
    }

    /**
     * Creates and styles an action button for the current host environment.
     *
     * @param key The semantic action icon key.
     * @param size The nominal icon size.
     * @param tooltip Optional tooltip text.
     * @return The configured JButton.
     */
    public JButton createSquareButton(@NonNull ActionIconKey key, int size, String tooltip) {
        JButton button = new JButton(getActionIcon(key, size));
        if (tooltip != null && !tooltip.isBlank()) {
            button.setToolTipText(tooltip);
        }
        return button;
    }

    /**
     * Creates and styles an action toggle button for the current host environment.
     *
     * @param key The semantic action icon key.
     * @param size The nominal icon size.
     * @param tooltip Optional tooltip text.
     * @param selected Initial selection state.
     * @return The configured JToggleButton.
     */
    public JToggleButton createSquareToggleButton(@NonNull ActionIconKey key, int size, String tooltip, boolean selected) {
        JToggleButton button = new JToggleButton(getActionIcon(key, size), selected);
        if (tooltip != null && !tooltip.isBlank()) {
            button.setToolTipText(tooltip);
        }
        return button;
    }
    
    /**
     * Does nothing because in normal swing buttons are already square
     * @param button
     * @param size 
     */
    public void forceSquare(AbstractButton button, int size) {
        
    }

    /**
     * Checks if the active Look and Feel is a dark variant.
     *
     * @return true if dark mode is active.
     */
    public boolean isDark() {
        return isDarkLaf();
    }


    /**
     * Convenience factory returning a {@link UITheme} that reflects the currently active Look and
     * Feel, for leaf UI components (renderers, dialogs, secondary panels) that do not hold a
     * {@code SwingAgiConfig} instance. A fresh theme is returned on each call so it always tracks the
     * live LaF (light/dark); callers should capture it once at construction time rather than in a
     * per-paint path.
     *
     * @return a UI theme for the active Look and Feel.
     */
    public static UITheme getTheme() {
        return new UITheme();
    }

    /**
     * A collection of color and font definitions that define the visual
     * identity of the Anahata Swing UI, dynamically adjusting between light
     * and dark aesthetics based on the active Look and Feel.
     */
    @Getter
    public static class UITheme {
        /** The primary foreground color for general text. */
        private final Color fontColor = UIManager.getColor("TextPane.foreground") != null 
                ? UIManager.getColor("TextPane.foreground") 
                : (isDarkLaf() ? new Color(220, 220, 220) : Color.BLACK);
        /** The fixed-width font used for code blocks and monospaced text segments. */
        private final Font monoFont = new Font("SF Mono", Font.PLAIN, 14);

        /** Background color for the user message header. */
        private final Color userHeaderBg = isDarkLaf() ? new Color(24, 34, 28) : new Color(212, 237, 218);
        /** Background color for the user message content area. */
        private final Color userContentBg = isDarkLaf() ? new Color(18, 25, 20) : new Color(235, 250, 235);
        /** Foreground color for the user message header text. */
        private final Color userHeaderFg = isDarkLaf() ? new Color(133, 180, 143) : new Color(21, 87, 36);
        /** Border color for user message panels. */
        private final Color userBorder = isDarkLaf() ? new Color(38, 55, 44) : new Color(144, 198, 149);

        /** Background color for the model message header. */
        private final Color modelHeaderBg = isDarkLaf() ? new Color(23, 29, 39) : new Color(221, 234, 248);
        /** Background color for the model message content area. */
        private final Color modelContentBg = isDarkLaf() ? new Color(16, 20, 27) : new Color(250, 252, 255);
        /** Foreground color for the model message header text. */
        private final Color modelHeaderFg = isDarkLaf() ? new Color(120, 162, 202) : new Color(0, 123, 255);
        /** Border color for model message panels. */
        private final Color modelBorder = isDarkLaf() ? new Color(36, 46, 61) : new Color(160, 195, 232);

        /** Background color for tool/system message headers. */
        private final Color toolHeaderBg = isDarkLaf() ? new Color(29, 23, 35) : new Color(223, 213, 235);
        /** Background color for tool/system message content areas. */
        private final Color toolContentBg = isDarkLaf() ? new Color(20, 16, 25) : new Color(250, 248, 252);
        /** Foreground color for tool/system message header text. */
        private final Color toolHeaderFg = isDarkLaf() ? new Color(175, 140, 205) : new Color(80, 60, 100);
        /** Border color for tool/system message panels. */
        private final Color toolBorder = isDarkLaf() ? new Color(48, 38, 58) : new Color(200, 180, 220);

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
        private final Color partHeaderBg = isDarkLaf() ? new Color(0, 0, 0, 40) : new Color(240, 240, 240, 100);
        /** Foreground color for message part header text. */
        private final Color partHeaderFg = isDarkLaf() ? new Color(180, 180, 180) : new Color(100, 100, 100);
        /** Border color for message part panels. */
        private final Color partBorder = isDarkLaf() ? new Color(255, 255, 255, 30) : new Color(220, 220, 220, 150);

        /** Foreground color for model thought/reasoning text. */
        private final Color thoughtFg = UIManager.getColor("Label.disabledForeground") != null
                ? UIManager.getColor("Label.disabledForeground")
                : (isDarkLaf() ? new Color(120, 120, 120) : new Color(150, 150, 150));

        /**
         * Generic chrome border color for separators, matte borders and line borders on secondary
         * panels (control strips, sidebars, code-block frames). Follows the active Look and Feel via
         * {@code Separator.foreground}, with dark/light fallbacks so it never renders a light-only
         * hairline on a dark IDE theme.
         */
        private final Color chromeBorder = UIManager.getColor("Separator.foreground") != null
                ? UIManager.getColor("Separator.foreground")
                : (isDarkLaf() ? new Color(70, 73, 78) : new Color(200, 200, 200));
        /**
         * Generic muted/secondary foreground for de-emphasised labels (language tags, class names,
         * status hints). Follows {@code Label.disabledForeground} with dark/light fallbacks.
         */
        private final Color mutedFg = UIManager.getColor("Label.disabledForeground") != null
                ? UIManager.getColor("Label.disabledForeground")
                : (isDarkLaf() ? new Color(150, 150, 150) : new Color(110, 110, 110));
        /**
         * Foreground for hyperlinks and active/selected tab labels. Follows
         * {@code Component.linkColor} with dark/light fallbacks, so links stay legible on a dark IDE
         * theme (plain {@link Color#BLUE} is far too dark on a dark background).
         */
        private final Color linkFg = UIManager.getColor("Component.linkColor") != null
                ? UIManager.getColor("Component.linkColor")
                : (isDarkLaf() ? new Color(88, 157, 246) : new Color(0, 102, 204));

        /** Default background color for message headers if role is undefined. */
        private final Color defaultHeaderBg = isDarkLaf() ? new Color(30, 30, 30) : Color.WHITE;
        /** Default background color for message content areas. */
        private final Color defaultContentBg = isDarkLaf() ? new Color(25, 25, 25) : new Color(248, 249, 250);
        /** Default border color for message panels. */
        private final Color defaultBorder = isDarkLaf() ? new Color(60, 60, 60) : Color.LIGHT_GRAY;

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
        private final Color groundingHeaderBg = isDarkLaf() ? new Color(30, 45, 60) : new Color(240, 248, 255);
        /** Background color for grounding metadata content. */
        private final Color groundingContentBg = isDarkLaf() ? new Color(25, 35, 50) : new Color(250, 252, 255);
        /** Background color for grounding source details header. */
        private final Color groundingDetailsHeaderBg = isDarkLaf() ? new Color(35, 50, 70) : new Color(230, 240, 250);
        /** Foreground color for grounding source details header text. */
        private final Color groundingDetailsHeaderColor = isDarkLaf() ? new Color(150, 200, 255) : new Color(0, 50, 100);
        /** Background color for grounding source details content. */
        private final Color groundingDetailsContentBg = isDarkLaf() ? new Color(20, 25, 35) : Color.WHITE;
        /** Background color for interactive grounding chips. */
        private final Color chipBackground = isDarkLaf() ? new Color(25, 45, 65) : new Color(235, 245, 255);
        /** Text color for interactive grounding chips. */
        private final Color chipText = isDarkLaf() ? new Color(100, 180, 255) : new Color(0, 100, 200);
        /** Border color for interactive grounding chips. */
        private final Color chipBorder = isDarkLaf() ? new Color(50, 80, 110) : new Color(180, 210, 240);

        /** Default background color for agi cards in the selection grid. */
        private final Color cardNormalBg = isDarkLaf() ? new Color(32, 34, 38) : new Color(255, 253, 208);
        /** Background color for agi cards on mouse hover. */
        private final Color cardHoverBg = isDarkLaf() ? new Color(40, 43, 49) : new Color(255, 255, 225);
        /** Background color for the currently selected agi card. */
        private final Color cardSelectedBg = isDarkLaf() ? new Color(38, 48, 64) : new Color(255, 245, 180);
        /** Background color for archived/closed agi cards. */
        private final Color cardArchivedBg = isDarkLaf() ? new Color(24, 25, 28) : new Color(240, 240, 240);
        /** Border color for standard agi cards. */
        private final Color cardBorder = isDarkLaf() ? new Color(55, 57, 62) : new Color(220, 220, 180);
        /** Border color for the selected agi card. */
        private final Color cardSelectedBorder = isDarkLaf() ? new Color(40, 120, 210) : new Color(180, 160, 50);

        /** Background color for the start of the pruned message header. */
        private final Color prunedHeaderStartBg = isDarkLaf() ? new Color(45, 45, 45, 120) : new Color(235, 235, 235);
        /** Background color for the end of the pruned message header. */
        private final Color prunedHeaderEndBg = isDarkLaf() ? new Color(35, 35, 35, 120) : new Color(242, 242, 242);
        /** Foreground color for pruned message titles. */
        private final Color prunedHeaderFg = isDarkLaf() ? new Color(110, 110, 110) : new Color(120, 120, 120);

        /** Background color for start of a pruned part header. */
        private final Color prunedPartHeaderStartBg = isDarkLaf() ? new Color(0, 0, 0, 60) : new Color(230, 230, 230, 150);
        /** Content background for a pruned part. */
        private final Color prunedPartContentBg = isDarkLaf() ? new Color(20, 20, 20) : new Color(240, 240, 240);
        /** Header start background overlay for normal part. */
        private final Color partHeaderStartBg = isDarkLaf() ? new Color(255, 255, 255, 10) : new Color(248, 248, 248, 80);
        /** Content background overlay for normal part. */
        private final Color partContentBg = new Color(0, 0, 0, 0);

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
