/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Context menu actions for managing AGI session context within the IntelliJ Project view.
 * <p>
 * Houses dynamic action groups and toggle actions contributed to the Project-view popup menu:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.ide.actions.AgiContextActionGroup}: Dynamic action group rebuilding
 *       context menu items on each menu opening to list all active AGI sessions.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.ide.actions.ToggleAgiContextAction}: Per-session toggle action allowing
 *       the user to add or remove selected files and folders from a specific session's {@code ResourceManager}.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.ide.actions;
