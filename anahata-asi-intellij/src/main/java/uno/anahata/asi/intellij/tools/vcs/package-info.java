/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Version Control System (VCS) and Git4Idea automation toolkit for IntelliJ IDEA.
 * <p>
 * Implements the universal {@link uno.anahata.asi.ide.tools.vcs.AbstractVCS} contract using IntelliJ's native VCS framework:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.vcs.IntellijVCS}: Full-featured VCS toolkit supporting provider-agnostic change
 *       tracking via {@link com.intellij.openapi.vcs.changes.ChangeListManager}, IntelliJ Local History inspection, file revert,
 *       and headless Git operations (init, status, add, commit, push, pull, fetch, branches, checkout, merge, log, diff, show, blame).</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.vcs;
