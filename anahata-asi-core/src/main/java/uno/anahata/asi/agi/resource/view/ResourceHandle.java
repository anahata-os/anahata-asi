/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import org.apache.commons.io.IOUtils;
import uno.anahata.asi.persistence.Rebindable;
import uno.anahata.asi.agi.resource.Resource;

/**
 * A strategy interface for physical or virtual connectivity to resource data.
 * <p>
 * Handles encapsulate the details of how to read, write, and identify the 
 * source content (e.g., local files, IDE objects, or memory strings).
 * </p>
 * <p>
 * <b>Virtual vs. Physical:</b> Use {@link #isVirtual()} to distinguish between 
 * memory-backed content (snippets, tool args) and persistent storage (files).
 * </p>
 * 
 * @author anahata
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
    default String getHtmlDisplayName() { 
        return null; 
    }

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
     * Returns the full content of the resource as a String.
     * <p>
     * <b>Handy API:</b> This uses the handle's detected charset and ensures 
     * proper stream closure.
     * </p>
     * @return The text content.
     * @throws IOException if reading fails.
     */
    default String asText() throws IOException {
        try (InputStream is = openStream()) {
            return IOUtils.toString(is, getCharset());
        }
    }

    /**
     * Returns the full content of the resource as a byte array.
     * @return The binary content.
     * @throws IOException if reading fails.
     */
    default byte[] asBytes() throws IOException {
        try (InputStream is = openStream()) {
            return IOUtils.toByteArray(is);
        }
    }

    /**
     * Determines if the resource is writable in the current environment.
     * @return true if the handle supports the {@link #write(String)} operation.
     */
    default boolean isWritable() {
        return false;
    }

    /**
     * Agnostically writes text content back to the resource.
     * <p>
     * <b>Purity Note:</b> Read-only handles should throw {@link UnsupportedOperationException}.
     * </p>
     * 
     * @param content The text to write.
     * @throws IOException if the write fails.
     */
    default void write(String content) throws IOException {
        throw new UnsupportedOperationException("Resource handle is read-only: " + getUri());
    }

    /** 
     * Determines if this is a virtual, memory-backed handle.
     * <p>
     * <b>High-Fidelity Strategy:</b> Virtual handles (snippets) typically 
     * trigger the 'Preview-as-Editor' mode in viewers to ensure constant 
     * IDE fidelity without card-swapping.
     * </p>
     * @return true if virtual (in-memory content).
     */
    boolean isVirtual();

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
     * Determines if the resource is textual and suitable for a TextView.
     * <p>
     * This method provides a centralized capability check. It whitelists common 
     * structured data formats that may be classified under the 'application/' 
     * hierarchy but are inherently textual.
     * </p>
     * @return true if the resource should be handled by a TextView.
     */
    default boolean isTextual() {
        String mime = getMimeType();
        if (mime == null) {
            return false;
        }

        mime = mime.toLowerCase().split(";")[0].trim();

        if (mime.startsWith("text/")) {
            return true;
        }

        // Whitelist for common application-based text formats
        return mime.endsWith("/xml")
                || mime.endsWith("/json")
                || mime.endsWith("/javascript")
                || mime.contains("yaml")
                || mime.contains("markdown")
                || mime.equals("application/x-sh")
                || mime.equals("application/x-java-source")
                || mime.equals("application/octet-stream"); // Catch-all for lost/unknown files
    }

    /**
     * Returns a machine-readable header summarizing the connectivity state.
     * <p>
     * <b>Technical Purity:</b> The first line always contains the implementation 
     * class FQN. The second line provides a comma-separated list of common attributes. 
     * Subclasses should override this to append implementation-specific details.
     * </p>
     * @return The header string.
     */
    default String getHeader() {
        return "Handler fqn: " + getClass().getName() + "\n" +
               "Handler details: uri=" + getUri() + 
               ", mime=" + getMimeType() + 
               ", exists=" + exists() + 
               ", virtual=" + isVirtual() + 
               ", writable=" + isWritable() + 
               ", lastModified=" + getLastModified();
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
