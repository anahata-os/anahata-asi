/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Visual node decorators and ambient context providers for the IntelliJ IDE workspace.
 * <p>
 * Contains components that contribute ambient contextual awareness and visual badges:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.ide.context.AgiContextDecorator}: A {@link com.intellij.ide.projectView.ProjectViewNodeDecorator}
 *       that badges in-context files and folders with session nicknames, counts, and branded icon overlays.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.ide.context.OpenToolWindowsContextProvider}: A context provider that injects
 *       a live Markdown snapshot of all registered tool windows and their active/visible states into the RAG message.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.ide.context;
