/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Provides the main entry points and bootstrapping logic for the standalone 
 * Anahata ASI applications.
 * <p>
 * This package is responsible for the initial environment configuration, 
 * Look-and-Feel (Laf) setup, and the orchestration of the top-level Swing 
 * frames and containers outside of an IDE environment.
 * </p>
 * <p>
 * Key Responsibilities:
 * </p>
 * <ul>
 *   <li><b>Application Bootstrapping</b>: Initializing logging levels and system properties.</li>
 *   <li><b>UI Initialization</b>: Setting up FlatLaf and assembling the primary JFrame.</li>
 *   <li><b>Container Lifecycle</b>: Instantiating the standalone-specific {@code AsiContainer} 
 *       and binding it to the main UI panel.</li>
 * </ul>
 * 
 * @author anahata
 */
package uno.anahata.asi.standalone;
