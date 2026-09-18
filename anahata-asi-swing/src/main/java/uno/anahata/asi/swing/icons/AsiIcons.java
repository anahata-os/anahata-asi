/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

import javax.swing.Icon;

/**
 * A host-overridable registry of semantic action-button icons for the shared Swing UI.
 * <p>
 * The shared chat/dashboard UI creates its action buttons against {@linkplain Key semantic keys}
 * rather than concrete icon classes, so an IDE host can substitute its own native, theme-adaptive
 * icon set. By default (in the standalone Desktop and NetBeans hosts) each key resolves to the
 * built-in Anahata vector icon it has always used, so those hosts are visually unchanged. An IDE
 * host that wants native chrome — notably IntelliJ IDEA, whose {@code AllIcons} automatically adapt
 * to the active light/dark theme — registers a {@link Provider} via {@link #setProvider(Provider)};
 * any key the provider does not map (returns {@code null} for) falls back to the built-in default.
 * </p>
 *
 * @author anahata
 */
public final class AsiIcons {

    /**
     * Semantic identity of an action-button icon, decoupled from any concrete icon artwork so hosts
     * can map each meaning to their own iconography.
     */
    public enum Key {
        /** Cancel / close / decline (an "X"). */
        CANCEL,
        /** Permanently delete / dispose / remove. */
        DELETE,
        /** Save. */
        SAVE,
        /** Edit. */
        EDIT,
        /** Edit / revert a staged message before sending. */
        EDIT_STAGED,
        /** Copy to clipboard. */
        COPY,
        /** Send a message. */
        SEND,
        /** Auto-run pending tools and send. */
        RUN_AND_SEND,
        /** Stop / suspend the current operation. */
        STOP,
        /** Attach a file. */
        ATTACH,
        /** Add a URL / hyperlink. */
        LINK,
        /** Capture a screenshot. */
        SCREENSHOT,
        /** Search. */
        SEARCH,
        /** Open an existing session. */
        OPEN_SESSION,
        /** Refresh / recompute. */
        REFRESH,
        /** Clear the conversation history. */
        CLEAR_HISTORY,
        /** Create a new session. */
        NEW_SESSION,
        /** Import a saved session. */
        IMPORT,
        /** Open settings / preferences. */
        SETTINGS,
        /** Open in an external application (browser / file manager). */
        EXTERNAL,
        /** Navigate to / open the item in the IDE. */
        OPEN_IN_IDE,
        /** Pin. */
        PIN,
        /** Toggle local (in-process/desktop) tools. */
        LOCAL_TOOLS,
        /** Toggle host/server-provided tools. */
        SERVER_TOOLS,
        /** Toggle automatic tool replies. */
        AUTO_REPLY,
        /** Test a provider connection. */
        TEST_CONNECTION
    }

    /**
     * A host-specific icon source. Implementations map semantic {@link Key}s to native icons.
     */
    public interface Provider {

        /**
         * Resolves a native icon for the given semantic key.
         *
         * @param key  the semantic icon identity.
         * @param size the requested nominal size in pixels (hosts may ignore it if their icons are
         *             fixed-size).
         * @return the host icon, or {@code null} to fall back to the built-in default.
         */
        Icon get(Key key, int size);
    }

    /**
     * The currently registered host provider, or {@code null} to use the built-in defaults.
     */
    private static Provider provider;

    /**
     * Private constructor to prevent instantiation of this static utility.
     */
    private AsiIcons() {
    }

    /**
     * Registers a host-specific icon provider, or clears it.
     *
     * @param p the provider to use, or {@code null} to restore the built-in default icons.
     */
    public static void setProvider(Provider p) {
        provider = p;
    }

    /**
     * Resolves the icon for a semantic key at a given size, preferring the registered host provider
     * and falling back to the built-in Anahata vector icon.
     *
     * @param key  the semantic icon identity.
     * @param size the nominal icon size in pixels.
     * @return a non-null icon.
     */
    public static Icon get(Key key, int size) {
        Provider p = provider;
        if (p != null) {
            Icon hostIcon = p.get(key, size);
            if (hostIcon != null) {
                return hostIcon;
            }
        }
        return defaultIcon(key, size);
    }

    /**
     * Returns the built-in Anahata vector icon for a key — the artwork these buttons have always
     * used, so hosts without a provider are visually unchanged.
     *
     * @param key  the semantic icon identity.
     * @param size the nominal icon size in pixels.
     * @return the built-in icon.
     */
    private static Icon defaultIcon(Key key, int size) {
        return switch (key) {
            case CANCEL -> new CancelIcon(size);
            case DELETE -> new DeleteIcon(size);
            case SAVE -> new SaveIcon(size);
            case EDIT -> new EditIcon(size);
            case EDIT_STAGED, REFRESH, CLEAR_HISTORY, NEW_SESSION -> new RestartIcon(size);
            case COPY -> new CopyIcon(size);
            case SEND -> new SendIcon(size);
            case RUN_AND_SEND -> new RunAndSendIcon(size);
            case STOP -> new StopIcon(size);
            case ATTACH -> new AttachIcon(size);
            case LINK -> new LinkIcon(size);
            case SCREENSHOT -> new ScreenshotIcon(size);
            case SEARCH, OPEN_SESSION -> new SearchIcon(size);
            case IMPORT -> new LoadSessionIcon(size);
            case SETTINGS -> new SettingsIcon(size);
            case EXTERNAL -> new ExternalIcon(size);
            case OPEN_IN_IDE -> new NextIcon(size);
            case PIN -> new PinnedIcon(size);
            case LOCAL_TOOLS -> IconUtils.getIcon("java.png", size);
            case SERVER_TOOLS -> new ServerToolsIcon(size);
            case AUTO_REPLY -> new AutoReplyIcon(size);
            case TEST_CONNECTION -> new PulseIcon(size);
        };
    }
}
