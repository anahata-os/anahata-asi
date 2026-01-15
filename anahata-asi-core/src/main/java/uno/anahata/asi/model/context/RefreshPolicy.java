/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.model.context;

import uno.anahata.asi.model.resource.AbstractResource;

/**
 * Defines the refresh policy for a {@link AbstractResource} when its content
 * is provided to the model via the Augmented Workspace.
 *
 * @author anahata-gemini-pro-2.5
 */
public enum RefreshPolicy {
    /**
     * The resource's content is captured once when it is first loaded.
     * This snapshot is always provided to the model, even if the underlying
     * resource on disk changes. Ideal for historical data like a git diff.
     */
    SNAPSHOT,

    /**
     * The resource is checked for modifications on every turn. If the on-disk
     * version is newer than the version in context, it is automatically
     * reloaded before being provided to the model. Ideal for source files
     * under active development.
     */
    LIVE;
}
