/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.model.context;

/**
 * A marker interface for objects that represent a resource with a persistent,
 * trackable state (e.g., a file on disk). This is the lynchpin of the V2
 * context management system.
 *
 * @author anahata-gemini-pro-2.5
 */
public interface StatefulResource {

    /**
     * Gets the unique, stable identifier for this resource (e.g., the absolute file path).
     * This is used as the key for tracking the resource.
     * @return The unique resource ID.
     */
    String getResourceId();

    /**
     * Gets the timestamp of the last modification of the resource's state
     * that is currently loaded in memory.
     * @return The last modified timestamp.
     */
    long getLastModified();

    /**
     * Gets the size of the resource's state currently loaded in memory.
     * @return The size in bytes.
     */
    long getSize();

    /**
     * Gets the refresh policy for this resource, determining how its content
     * is updated in the Augmented Workspace.
     * @return The refresh policy.
     */
    RefreshPolicy getRefreshPolicy();
}
