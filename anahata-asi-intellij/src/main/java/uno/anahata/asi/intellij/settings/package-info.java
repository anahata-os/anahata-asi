/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */

/**
 * IDE-level settings and configuration UI for the Anahata ASI IntelliJ plugin.
 * <p>
 * Integrates with IntelliJ's standard options dialog (Settings / Preferences):
 * </p>
 * <ul>
 *   <li>{@link uno.anahata.asi.intellij.settings.AnahataConfigurable}: The native Settings page under
 *       {@code Settings -> Tools -> Anahata ASI}, providing configuration fields for default AI provider
 *       and model selection.</li>
 *   <li>{@link uno.anahata.asi.intellij.settings.AnahataSettings}: The application-level persistent state component
 *       ({@link com.intellij.openapi.components.PersistentStateComponent}) persisted to {@code anahata-asi.xml}.</li>
 * </ul>
 *
 * @author anahata
 */
package uno.anahata.asi.intellij.settings;
