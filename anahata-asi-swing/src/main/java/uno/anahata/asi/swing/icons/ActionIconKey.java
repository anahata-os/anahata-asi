/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.icons;

/**
 * Semantic identity of an action-button icon across the Anahata Swing UI.
 * <p>
 * Decouples action button rendering from concrete icon implementations, allowing
 * host IDE environments (such as IntelliJ IDEA via {@code AllIcons}) to supply native,
 * theme-adaptive iconography while falling back to Anahata vector icons in standalone
 * and NetBeans environments.
 * </p>
 *
 * @author anahata
 */
public enum ActionIconKey {

    /**
     * Cancel, close, or decline operation.
     */
    CANCEL,

    /**
     * Permanently delete, dispose, or remove an item.
     */
    DELETE,

    /**
     * Save session, template, or provider configuration.
     */
    SAVE,

    /**
     * Edit content or toggle edit mode.
     */
    EDIT,

    /**
     * Edit or revert a staged message before dispatching.
     */
    EDIT_STAGED,

    /**
     * Copy content to the system clipboard.
     */
    COPY,

    /**
     * Send user message to the active AI model.
     */
    SEND,

    /**
     * Execute pending tool calls and immediately dispatch prompt to model.
     */
    RUN_AND_SEND,

    /**
     * Stop or interrupt active model generation or tool execution.
     */
    STOP,

    /**
     * Attach a local filesystem resource.
     */
    ATTACH,

    /**
     * Attach a remote URL resource.
     */
    LINK,

    /**
     * Capture desktop screenshot.
     */
    SCREENSHOT,

    /**
     * Search models, sessions, or text content.
     */
    SEARCH,

    /**
     * Open or focus an existing AGI session tab.
     */
    OPEN_SESSION,

    /**
     * Refresh metrics, tokens, or view state.
     */
    REFRESH,

    /**
     * Clear the conversation history for the active session.
     */
    CLEAR_HISTORY,

    /**
     * Create a brand-new AGI session.
     */
    NEW_SESSION,

    /**
     * Import an archived session from disk.
     */
    IMPORT,

    /**
     * Open global container settings and preferences dialog.
     */
    SETTINGS,

    /**
     * Open in an external application (system browser or desktop file manager).
     */
    EXTERNAL,

    /**
     * Open or navigate to an item inside the host IDE editor.
     */
    OPEN_IN_IDE,

    /**
     * Pin message or part to prevent garbage collection pruning.
     */
    PIN,

    /**
     * Toggle local in-process Java tools.
     */
    LOCAL_TOOLS,

    /**
     * Toggle remote hosted server tools.
     */
    SERVER_TOOLS,

    /**
     * Toggle automatic tool execution replies.
     */
    AUTO_REPLY,

    /**
     * Test connection to a remote AI provider endpoint.
     */
    TEST_CONNECTION,

    /**
     * Duplicate or clone session or template.
     */
    CLONE,

    /**
     * Capture application or window frames screenshot.
     */
    CAPTURE_WINDOW,

    /**
     * Toggle or configure live screen sharing.
     */
    SCREEN_SHARE,

    /**
     * Add item, property, or entry.
     */
    ADD,

    /**
     * Confirm dialog or affirmative action.
     */
    OK,

    /**
     * Navigate forward or advance to next step.
     */
    NEXT,

    /**
     * Navigate backward or return to previous step.
     */
    PREV,

    /**
     * Switch view to cards layout.
     */
    CARDS_VIEW,

    /**
     * Switch view to table layout.
     */
    TABLE_VIEW,

    /**
     * Compress or prune context items.
     */
    COMPRESS,

    /**
     * Session template or blueprint.
     */
    TEMPLATES,

    /**
     * Active sound feedback and notifications.
     */
    BELL,

    /**
     * Muted sound feedback and notifications.
     */
    BELL_MUTE
}
