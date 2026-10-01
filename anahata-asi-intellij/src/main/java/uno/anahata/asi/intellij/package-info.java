/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * Root package for the Anahata ASI IntelliJ IDEA integration module ({@code anahata-asi-intellij}).
 * <p>
 * This package hosts the foundational platform integration components that bind the Anahata ASI
 * core framework and Swing UI layers to the IntelliJ IDEA host environment:
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.AnahataToolWindowFactory}: The entry point for creating the
 *       {@code Anahata ASI} tool window in IntelliJ, embedding the master session cards dashboard,
 *       installing title-bar actions, and managing dynamic plugin reloading.</li>
 *   <li>{@link uno.anahata.asi.intellij.IntellijAsiContainer}: The application-level singleton service
 *       implementing {@link uno.anahata.asi.AbstractAsiContainer} and {@link com.intellij.openapi.Disposable},
 *       managing the lifecycle of active AGI sessions, AI providers, and JCEF media components.</li>
 *   <li>{@link uno.anahata.asi.intellij.IntellijAgiConfig}: The IntelliJ-specific session configuration
 *       extending {@link uno.anahata.asi.swing.agi.SwingAgiConfig}, registering all IntelliJ-native toolkits
 *       and mapping semantic action keys to theme-adaptive IntelliJ {@link com.intellij.icons.AllIcons}.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij;
