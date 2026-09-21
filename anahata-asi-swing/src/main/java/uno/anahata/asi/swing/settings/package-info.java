/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
/**
 * Container settings, administrative command center, and telemetry components for the Anahata ASI Swing UI.
 * <p>
 * This package provides the centralized, multi-tabbed administration suite for configuring,
 * monitoring, and bootstrapping ASI container runtimes across all desktop and IDE environments:
 * </p>
 * <ul>
 *   <li><b>Command Center Architecture:</b> {@link uno.anahata.asi.swing.settings.AsiContainerSettingsFrame} provides a maximized,
 *       single-instance window hosting the master {@link uno.anahata.asi.swing.settings.AsiContainerSettingsPanel}.</li>
 *   <li><b>AI Provider Management:</b> {@link uno.anahata.asi.swing.settings.AiProvidersPanel} delivers live endpoint configuration,
 *       encrypted API key rotation, credential validation, and real-time model discovery for all registered LLM providers.</li>
 *   <li><b>AGI Session Templates:</b> {@link uno.anahata.asi.swing.settings.TemplatesPanel} delivers a master-detail workspace
 *       for creating, customizing, cloning, and launching active sessions from templates, including designating the canonical
 *       {@code default.kryo} bootstrap template.</li>
 *   <li><b>Telemetry and Diagnostics:</b> {@link uno.anahata.asi.swing.settings.AsiContainerAboutPanel} presents real-time JVM heap
 *       memory telemetry (Generational ZGC utilization), verified storage paths on disk, container identity metadata, and boot notifications.</li>
 * </ul>
 * <p>
 * All UI components in this package are designed for thread-safe execution on the Swing Event Dispatch Thread (EDT)
 * and reactively bind to container property changes via {@link uno.anahata.asi.swing.internal.EdtPropertyChangeListener}.
 * </p>
 *
 * @author anahata
 */
package uno.anahata.asi.swing.settings;
