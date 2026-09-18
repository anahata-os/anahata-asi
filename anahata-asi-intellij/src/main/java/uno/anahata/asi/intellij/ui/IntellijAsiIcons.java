/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij.ui;

import com.intellij.icons.AllIcons;
import javax.swing.Icon;
import uno.anahata.asi.swing.icons.AsiIcons;

/**
 * Maps the shared UI's semantic {@link AsiIcons.Key action-button icons} to native IntelliJ
 * {@link AllIcons}.
 * <p>
 * Registered with {@link AsiIcons#setProvider} during plugin bootstrap so the Anahata tool window's
 * buttons render with the IDE's own iconography. Because {@code AllIcons} entries carry built-in
 * light/dark variants, the buttons automatically match the active IntelliJ theme (unlike the
 * built-in Anahata vector icons, whose colors are fixed). The nominal size is ignored: platform
 * icons are the standard 16&nbsp;px and buttons size to their icon. Keys with no natural platform
 * equivalent return {@code null}, so the shared registry falls back to the Anahata icon (keeping
 * chat-specific glyphs such as the pruned-leaf and screen-share toggles).
 * </p>
 *
 * @author anahata
 */
public final class IntellijAsiIcons implements AsiIcons.Provider {

    /**
     * Constructs the IntelliJ icon provider.
     */
    public IntellijAsiIcons() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Icon get(AsiIcons.Key key, int size) {
        return switch (key) {
            case CANCEL -> AllIcons.Actions.Cancel;
            case DELETE -> AllIcons.General.Delete;
            case SAVE -> AllIcons.Actions.MenuSaveall;
            case EDIT, EDIT_STAGED -> AllIcons.Actions.Edit;
            case COPY -> AllIcons.Actions.Copy;
            case SEND, RUN_AND_SEND -> AllIcons.Actions.Execute;
            case STOP -> AllIcons.Actions.Suspend;
            case ATTACH -> AllIcons.Actions.Attach;
            case LINK -> AllIcons.Ide.Link;
            case SCREENSHOT -> AllIcons.Actions.Dump;
            case SEARCH -> AllIcons.Actions.Search;
            case OPEN_SESSION -> AllIcons.General.OpenInToolWindow;
            case REFRESH -> AllIcons.Actions.Refresh;
            case CLEAR_HISTORY -> AllIcons.Actions.GC;
            case NEW_SESSION -> AllIcons.General.Add;
            case IMPORT -> AllIcons.ToolbarDecorator.Import;
            case SETTINGS -> AllIcons.General.Settings;
            case EXTERNAL -> AllIcons.Ide.External_link_arrow;
            case OPEN_IN_IDE -> AllIcons.Actions.Forward;
            case PIN -> AllIcons.General.Pin_tab;
            case SERVER_TOOLS -> AllIcons.Nodes.Plugin;
            case AUTO_REPLY -> AllIcons.Actions.Rerun;
            case TEST_CONNECTION -> AllIcons.Actions.Lightning;
        };
    }
}
