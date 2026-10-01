/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Gradle build and task execution toolkit for IntelliJ IDEA.
 * <p>
 * Encapsulates the {@link uno.anahata.asi.intellij.tools.gradle.Gradle} toolkit, which utilizes
 * IntelliJ's external-system framework to interact with linked Gradle projects:
 * </p>
 * <ul>
 *   <li>Discovering Gradle projects linked to open IntelliJ projects without compile-time coupling to Gradle plugins.</li>
 *   <li>Executing Gradle tasks via {@link com.intellij.openapi.externalSystem.util.ExternalSystemUtil#runTask} with
 *       bounded synchronous waiting and console streaming to the IDE's Run tool window.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.gradle;
