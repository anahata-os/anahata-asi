/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Maven project integration, dependency inspection, and repository indexing toolkits for IntelliJ IDEA.
 * <p>
 * Interfaces directly with IntelliJ's native Maven integration subsystem:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.maven.Maven}: Toolkit for querying imported Maven projects, resolving
 *       dependencies, parsing {@code pom.xml} declared dependencies, executing goals via {@link org.jetbrains.idea.maven.execution.MavenRunner},
 *       adding dependencies, and performing unified artifact searches.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.maven.MavenArtifactGroup}: Consolidated model representing a Maven artifact with
 *       all indexed versions, release stability classifications, and latest version resolution.</li>
 *   <li>{@link uno.anahata.asi.intellij.tools.maven.MavenSearchReport}: Paginated report container for Maven repository index search queries.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.maven;
