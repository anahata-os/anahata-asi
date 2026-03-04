/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import uno.anahata.asi.model.core.Rebindable;

/**
 * The Source of Truth for a resource. 
 * <p>
 * Encapsulates the logic for accessing raw data and metadata from a URI. 
 * Handles are responsible for the "Physical/Virtual" connectivity.
 * </p>
 */
public interface ResourceHandle extends Rebindable {
    /** 
     * Gets the unique URI for this resource. 
     * @return The identifier URI.
     */
    URI getUri();

    /**
     * Returns a user-friendly name for the source.
     * @return The source name.
     */
    String getName();

    /**
     * Returns an optional HTML-formatted display name.
     * Used by IDE environments to show status (e.g. Git colors).
     * @return The HTML display name, or null.
     */
    default String getHtmlDisplayName() { return null; }

    /** 
     * Returns the detected MIME type of the resource. 
     * @return The MIME type string (e.g., "text/plain", "image/png").
     */
    String getMimeType();

    /** 
     * Returns the last modified timestamp in milliseconds. 
     * @return The timestamp, or 0 if unknown.
     */
    long getLastModified();

    /** 
     * Checks if the resource physically or virtually exists. 
     * @return true if the source is available.
     */
    boolean exists();

    /** 
     * Opens a fresh input stream to the resource content. 
     * @return A new InputStream instance.
     * @throws IOException if the stream cannot be opened.
     */
    InputStream openStream() throws IOException;
    
    /** 
     * Indicates if the resource is local to the host filesystem. 
     * @return true if the resource is a local file.
     */
    boolean isLocal();

    /** 
     * Returns the detected or configured charset. Defaults to UTF-8. 
     * @return The Charset to use for text interpretation.
     */
    default Charset getCharset() { 
        return StandardCharsets.UTF_8; 
    }

    /** 
     * Checks if the source has changed since the last load timestamp. 
     * @param lastLoadTimestamp The timestamp of the last successful load.
     * @return true if the source is newer than the timestamp.
     */
    default boolean isStale(long lastLoadTimestamp) {
        return getLastModified() > lastLoadTimestamp;
    }

    /** 
     * Associates this handle with its parent Resource. 
     * @param owner The owning Resource orchestrator.
     */
    void setOwner(Resource owner);

    /**
     * Gets the parent resource orchestrator for this handle.
     * @return The owning Resource instance.
     */
    Resource getOwner();
    
    /** 
     * Performs any necessary cleanup (e.g., removing listeners). 
     */
    default void dispose() {}
}
