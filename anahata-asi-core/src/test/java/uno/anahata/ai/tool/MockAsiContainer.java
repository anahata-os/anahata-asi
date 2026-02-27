/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import uno.anahata.asi.AsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.model.resource.AbstractResource;

/**
 * A minimal mock implementation of {@link AsiContainer} for unit testing.
 * 
 * @author anahata
 */
public class MockAsiContainer extends AsiContainer {

    public MockAsiContainer(String hostId) {
        super(hostId);
    }

    @Override
    public Agi createNewAgi() {
        return new Agi(new AgiConfig(this));
    }

    @Override
    public void openResource(AbstractResource<?, ?> resource) {
        // No-op for tests
    }
}
