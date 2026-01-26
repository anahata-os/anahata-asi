/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.model.core;

/**
 * An interface for objects that require re-initialization after being 
 * deserialized (e.g., to restore transient fields like listeners or locks).
 * 
 * @author anahata
 */
public interface Rebindable {
    
    /**
     * Re-initializes the object's state after deserialization.
     */
    void rebind();
}
