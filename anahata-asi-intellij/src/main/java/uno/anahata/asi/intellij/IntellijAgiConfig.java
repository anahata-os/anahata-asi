/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.intellij;

import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.swing.agi.SwingAgiConfig;

/**
 * IntelliJ-specific AGI configuration.
 * <p>
 * Customizes the default model, provider, and tool availability 
 * for the IntelliJ IDEA platform environment.
 * </p>
 * 
 * @author anahata
 */
public class IntellijAgiConfig extends SwingAgiConfig {

    /**
     * Default initialization block to set IntelliJ-specific settings.
     */
    {
        setSelectedProviderUuid("Gemini");
        setSelectedModelId("models/gemini-flash-latest");
    }

    /**
     * Constructs a new IntelliJ AGI configuration.
     * 
     * @param container The host ASI container.
     */
    public IntellijAgiConfig(AbstractAsiContainer container) {
        super(container);
    }

    /**
     * Constructs a new IntelliJ AGI configuration with a specific session ID.
     * 
     * @param container The host ASI container.
     * @param sessionId The unique session ID.
     */
    public IntellijAgiConfig(AbstractAsiContainer container, String sessionId) {
        super(container, sessionId);
    }
}
