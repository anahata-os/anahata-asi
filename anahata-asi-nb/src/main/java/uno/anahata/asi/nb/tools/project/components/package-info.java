/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Defines the structural components and domain model for NetBeans project mapping.
 * <p>
 * This package provides a hierarchical representation of a project's internal 
 * structure, bridging the gap between physical files and logical Java types. 
 * It employs a "Refined Project Model" that categorizes workspace elements into 
 * Java source groups, resource folders, and project nodes.
 * </p>
 * <p>
 * Core Architectural Features:
 * </p>
 * <ul>
 *   <li><b>Hybrid Scanning</b>: Uses the NetBeans {@code ClassIndex} for high-speed 
 *       logical type discovery and physical filesystem walks for non-indexed 
 *       resources (e.g., {@code package-info.java}, images).</li>
 *   <li><b>Recursive Modeling</b>: Implements a composite pattern via {@link uno.anahata.asi.nb.tools.project.components.ProjectNode} 
 *       for hierarchical size calculation and Markdown rendering.</li>
 *   <li><b>VCS Awareness</b>: {@link uno.anahata.asi.nb.tools.project.components.ProjectComponent} 
 *       integrates with the IDE's Node API to extract version control badges and status.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.tools.project.components;
