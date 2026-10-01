/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Domain models and intent specifications for the AST-guided batch code refinement engine.
 * <p>
 * Houses the structural member-level modification contracts executed by {@link uno.anahata.asi.intellij.tools.java.BatchCodeRefiner}:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementBatch}: An atomic batch container extending
 *       {@link uno.anahata.asi.toolkit.resources.text.AbstractTextResourceWrite} to participate in optimistic locking and editable diff rendering.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.coderefiner.CodeRefinementIntent}: A specific structural modification descriptor
 *       specifying member insertion, update, deletion, or relocation.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.java.coderefiner.RelativePosition}: Placement positions (START, END, BEFORE, AFTER)
 *       relative to a class body or an anchor member.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.java.coderefiner;
