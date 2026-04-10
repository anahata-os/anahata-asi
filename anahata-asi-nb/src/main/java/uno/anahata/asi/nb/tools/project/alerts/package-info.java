/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides real-time diagnostic and alerting capabilities for NetBeans projects within 
 * the ASI framework.
 * <p>
 * This package implements a diagnostic layer that monitors the project's health by 
 * aggregating compiler-level alerts and high-level project problems (such as missing 
 * dependencies or broken classpaths).
 * </p>
 * <p>
 * Key Functional Components:
 * </p>
 * <ul>
 *   <li><b>Context Provisioning</b>: The {@link uno.anahata.asi.nb.tools.project.alerts.ProjectAlertsContextProvider} 
 *       injects live diagnostic data into the RAG message, allowing the ASI to respond 
 *       immediately to build failures or environment issues.</li>
 *   <li><b>Diagnostic Models</b>: Data structures like {@link uno.anahata.asi.nb.tools.project.alerts.JavacAlert} 
 *       and {@link uno.anahata.asi.nb.tools.project.alerts.ProjectAlert} provide a normalized 
 *       representation of IDE-specific problems.</li>
 *   <li><b>Aggregation</b>: {@link uno.anahata.asi.nb.tools.project.alerts.ProjectDiagnostics} 
 *       acts as a thread-safe container for all alerts found during a project scan.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.nb.tools.project.alerts;
