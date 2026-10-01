/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * IntelliJ Status Bar widgets and factories for the Anahata ASI integration.
 * <p>
 * Manages status-bar presence and interaction:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.ui.status.AnahataStatusBarWidget}: Status-bar widget showing the active AGI session count
 *       with an action listener that activates and focuses the Anahata tool window on click.</li>
 *   <li>{@link uno.anahata.asi.intellij.ui.status.AnahataStatusBarWidgetFactory}: Factory registered with IntelliJ's
 *       {@code statusBarWidgetFactory} extension point to install the widget on project windows.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.ui.status;
