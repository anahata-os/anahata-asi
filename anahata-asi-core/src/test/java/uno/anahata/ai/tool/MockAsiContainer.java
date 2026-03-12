/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.agi.AgiConfig;

/**
 * A minimal mock implementation of {@link AsiContainer} for unit testing.
 * 
 * @author anahata
 */
public class MockAsiContainer extends AsiContainer {

    public MockAsiContainer(String hostId) {
        super(hostId);
    }

    /** {@inheritDoc} */
    @Override
    public AgiConfig createNewAgiConfig() {
        return new AgiConfig(this);
    }
}
