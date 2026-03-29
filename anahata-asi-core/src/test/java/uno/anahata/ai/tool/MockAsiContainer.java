/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;

/**
 * A minimal mock implementation of {@link AbstractAsiContainer} for unit testing.
 * 
 * @author anahata
 */
public class MockAsiContainer extends AbstractAsiContainer {

    public MockAsiContainer(String hostId) {
        super(hostId);
    }

    /** {@inheritDoc} */
    @Override
    public AgiConfig createNewAgiConfig() {
        return new AgiConfig(this);
    }

    @Override
    protected void onAgiOpened(Agi agi) {
        // No-op for mock
    }

    @Override
    protected void onAgiClosed(Agi agi) {
        // No-op for mock
    }

    @Override
    public Object getUI(Agi agi) {
        return null;
    }
}
