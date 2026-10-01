/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * IntelliJ Project model integration, SDK configuration, and project lifecycle management.
 * <p>
 * Bridges the Anahata core framework with IntelliJ's project and SDK subsystems:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.tools.project.IntellijProjects}: Toolkit extending {@link uno.anahata.asi.toolkit.project.AbstractProjects},
 *       providing tools to open and close projects, trigger incremental builds and rebuilds via {@link com.intellij.openapi.compiler.CompilerManager},
 *       save documents, inspect configured Project SDKs, auto-detect system JDKs, and manage project context provider hierarchies.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.tools.project;
