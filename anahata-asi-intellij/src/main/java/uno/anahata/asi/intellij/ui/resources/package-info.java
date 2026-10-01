/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * IntelliJ-native resource viewers, metadata panels, and UI strategies.
 * <p>
 * Provides high-fidelity resource inspection and editing within IntelliJ:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.ui.resources.IntellijHandlePanel}: Property panel for {@link uno.anahata.asi.intellij.resources.handle.IntellijHandle},
 *       displaying VFS validity, canonical paths, and VCS status colors.</li>
 *   <li>{@link uno.anahata.asi.intellij.ui.resources.IntellijResourceUI}: Resource UI strategy injecting IDE actions and binding text resources
 *       to IntelliJ native editor components.</li>
 *   <li>{@link uno.anahata.asi.intellij.ui.resources.IntellijTextResourceViewer}: Embedded text resource viewer creating real IntelliJ
 *       {@link com.intellij.openapi.editor.Editor} instances via {@link com.intellij.openapi.editor.EditorFactory} with full syntax highlighting,
 *       code folding, line numbers, and theme tracking.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.ui.resources;
