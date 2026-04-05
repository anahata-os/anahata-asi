/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.nb;

import java.util.logging.Logger;

/**
 * A dedicated test class for the CodeRefiner toolkit.
 */
public class RefinerTest {
    private static final Logger LOG = Logger.getLogger(RefinerTest.class.getName());
    
    @Deprecated
    public String testMethod() {
        LOG.info("Structural AST refinement successful. For\u00e7a Bar\u00e7a!");
        return "refined";
    }
}
