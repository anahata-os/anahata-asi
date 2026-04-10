/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides a robust integration layer for NetBeans Project APIs, enabling programmatic 
 * management of the project lifecycle and environment within the ASI framework.
 * <p>
 * This package is centered around the {@link uno.anahata.asi.nb.tools.project.Projects} toolkit, 
 * which provides high-level abstractions for IDE operations. It leverages the NetBeans 
 * {@code ActionProvider} to trigger standard actions (build, run, test) and specialized 
 * ones like {@code nbmreload}.
 * </p>
 * <p>
 * Key Functional Areas:
 * </p>
 * <ul>
 *   <li><b>Project Orchestration</b>: Tools for opening, closing, and selecting main projects across the workspace.</li>
 *   <li><b>Configuration Management</b>: Management of 'Compile on Save' status via {@code nb-configuration.xml} overrides or POM properties.</li>
 *   <li><b>Structural Intelligence</b>: Recursive discovery of source groups, resource folders, and project-specific metadata (encoding, Java levels).</li>
 *   <li><b>Context Injection</b>: Domain entities like {@link uno.anahata.asi.nb.tools.project.ProjectOverview} provide the AI with a logical map of the workspace.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.tools.project;
