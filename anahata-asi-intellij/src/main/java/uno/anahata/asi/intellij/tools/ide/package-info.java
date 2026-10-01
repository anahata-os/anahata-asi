/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Core IDE automation toolkits for the IntelliJ IDEA platform.
 * <p>
 * Provides native toolkits for inspecting and manipulating editor windows, platform logging,
 * and project-wide Java refactorings:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.ide.Editor}: For opening files, scrolling to line numbers,
 *       listing open editors, and capturing active selections and visible code snippets.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.ide.IDE}: For tailing {@code idea.log}, revealing files in
 *       the Project view, and reporting open tool-window states.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.ide.Refactor}: For performing non-interactive, project-wide Java
 *       refactorings (rename, safe-delete, where-used, move, copy, pull-up, push-down, inline, change-signature,
 *       extract superclass/interface) updating all references across open projects.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.ide;
