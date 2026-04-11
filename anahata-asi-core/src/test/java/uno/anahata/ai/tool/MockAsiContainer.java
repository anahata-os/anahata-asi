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

    /**
     * Initializes a new mock container with the specified host identifier.
     * @param hostId The unique identifier for the virtual host.
     */
    public MockAsiContainer(String hostId) {
        super(hostId);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates a standard model-agnostic configuration for unit testing.
     * </p>
     */
    @Override
    public AgiConfig createNewAgiConfig() {
        return new AgiConfig(this);
    }

    /**
     * {@inheritDoc}
     * <p>
     * No-op implementation for mock isolation.
     * </p>
     */
    @Override
    protected void onAgiOpened(Agi agi) {
        // No-op for mock
    }

    /**
     * {@inheritDoc}
     * <p>
     * No-op implementation for mock isolation.
     * </p>
     */
    @Override
    protected void onAgiClosed(Agi agi) {
        // No-op for mock
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns {@code null} as this mock does not provide a visual representation.
     * </p>
     */
    @Override
    public Object getUI(Agi agi) {
        return null;
    }
}
