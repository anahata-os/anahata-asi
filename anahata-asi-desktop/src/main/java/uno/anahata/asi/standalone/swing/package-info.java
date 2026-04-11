/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the core Swing UI components and container orchestration for the 
 * standalone Anahata ASI desktop application.
 * <p>
 * This package extends the base Swing framework to provide a production-ready 
 * desktop experience. It manages the mapping between AGI sessions and their 
 * visual representations (panels and tabs) and handles the persistence 
 * lifecycle within the standalone environment.
 * </p>
 * <p>
 * Key Components:
 * </p>
 * <ul>
 *   <li><b>Container Orchestration</b>: {@link uno.anahata.asi.standalone.swing.AsiDesktopAsiContainer} 
 *       specializes the session lifecycle for a tabbed desktop environment.</li>
 *   <li><b>UI Assembly</b>: {@link uno.anahata.asi.standalone.swing.AsiDesktopMainPanel} 
 *       provides the primary layout, combining session cards with conversation tabs.</li>
 *   <li><b>Session Configuration</b>: {@link uno.anahata.asi.standalone.swing.AsiDesktopAgiConfig} 
 *       pre-configures the desktop environment with default providers (e.g., Gemini).</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.standalone.swing;
