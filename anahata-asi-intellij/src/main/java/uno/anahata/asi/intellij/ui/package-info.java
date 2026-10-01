/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * UI presentation components, custom parameter renderers, icons, and notifications for IntelliJ IDEA.
 * <p>
 * Provides host-specific UI rendering and notifications:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.ui.AnahataFileIconProvider}: Supplies branded Anahata logo icons and badge icons for instruction files.</li>
 *   <li>{@link uno.anahata.asi.intellij.ui.AnahataNotifications}: Helper for posting balloon notifications to IntelliJ's Event Log.</li>
 *   <li>{@link uno.anahata.asi.intellij.ui.IntellijIconProvider}: Resolves authentic IntelliJ file type and project/module icons for the context tree.</li>
 *   <li>{@link uno.anahata.asi.intellij.ui.IntellijJavaCodeParameterRenderer}: Renders executable Java code snippets with syntax highlighting.</li>
 *   <li>{@link uno.anahata.asi.intellij.ui.IntellijTextResourceWriteRenderer}: Editable side-by-side diff renderer backed by IntelliJ's
 *       {@link com.intellij.diff.DiffRequestPanel} supporting two-way write-back and line-level gutter comment bubbles.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.ui;
