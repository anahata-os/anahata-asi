/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Context providers supplying structural and diagnostic awareness for IntelliJ projects and modules.
 * <p>
 * Houses unified context provider nodes that populate the turn-by-turn RAG message:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.project.context.IntellijProjectContextProvider}: A unified, polymorphic project and
 *       submodule context provider extending {@link uno.anahata.asi.ide.tools.project.context.AbstractProjectContextProvider}, consolidating
 *       structured project overviews, compiler alerts via {@link com.intellij.problems.WolfTheProblemSolver}, and PSI AST source tree
 *       hierarchies with scoping controls.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.project.context;
