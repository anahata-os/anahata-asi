/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Internal utilities and context bridges supporting the Anahata ASI IntelliJ integration.
 * <p>
 * Contains low-level helper classes for interacting with IntelliJ platform subsystems,
 * the Program Structure Interface (PSI), and the Virtual File System (VFS):
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.internal.AgiContext}: A bridge connecting IntelliJ {@link com.intellij.openapi.vfs.VirtualFile}
 *       selections with active AGI session resource managers, powering Project-view decorators and context actions.</li>
 *   <li>{@link uno.anahata.asi.intellij.internal.IntellijPluginUtils}: Introspects the IntelliJ plugin runtime, traversing
 *       plugin descriptors and platform dependencies to assemble the comprehensive runtime classpath.</li>
 *   <li>{@link uno.anahata.asi.intellij.internal.JavaPsi}: Shared PSI and VFS resolution utilities, canonical member FQN
 *       computation, and smart-mode synchronization guards to prevent {@code IndexNotReadyException}.</li>
 *   <li>{@link uno.anahata.asi.intellij.internal.ProjectUtils}: Language-agnostic project and virtual file resolution
 *       utilities decoupling callers from direct PSI dependencies.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.internal;
