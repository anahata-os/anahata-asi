/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Reactive resource handles integrating IntelliJ Virtual Files into the Anahata resource pipeline.
 * <p>
 * Provides concrete implementations of {@link uno.anahata.asi.agi.resource.handle.ResourceHandle}
 * tightly coupled to IntelliJ's Virtual File System (VFS):
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.resources.handle.IntellijHandle}: A reactive resource handle that wraps
 *       physical and virtual files, subscribing to global VFS change events (renames, file movements, content modifications,
 *       and deletions) to automatically mark the parent {@link uno.anahata.asi.agi.resource.Resource} dirty and update
 *       AI context seamlessly without manual re-registration.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.resources.handle;
