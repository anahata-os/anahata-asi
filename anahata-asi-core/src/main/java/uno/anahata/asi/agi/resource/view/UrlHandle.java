/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * A resource handle that points to a remote URL or a protocol-based entry.
 * <p>
 * This implementation includes Metadata Caching to prevent 'Connection Storms'. 
 * It performs a single efficient check to capture MIME type, existence, and 
 * last-modified status in one network round-trip.
 * </p>
 * <p>
 * <b>Virtual Strategy:</b> This handle represents physical remote content, 
 * so it returns {@code false} for {@link #isVirtual()}.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@RequiredArgsConstructor
public class UrlHandle extends AbstractResourceHandle {

    /** The full URL string for the resource. */
    @NonNull
    private final String urlString;

    /** Cached metadata to prevent redundant connections. */
    private transient Metadata cache;

    /** 
     * Record for holding captured metadata. 
     * @param mimeType The detected MIME type.
     * @param lastModified The remote last modified timestamp.
     * @param exists Whether the remote source is reachable.
     */
    private record Metadata(String mimeType, long lastModified, boolean exists) {}

    /**
     * Refreshes the metadata cache if it is null by performing a HEAD request.
     * This is a private implementation detail for network efficiency.
     */
    private synchronized void refreshMetadata() {
        if (cache != null) {
            return;
        }
        log.debug("Refreshing remote metadata for: {}", urlString);
        try {
            URL url = new URL(urlString);
            URLConnection conn = url.openConnection();
            conn.setConnectTimeout(5000);
            conn.setReadTimeout(5000);
            
            if (conn instanceof HttpURLConnection http) {
                http.setRequestMethod("HEAD");
            }
            
            conn.connect();
            String type = conn.getContentType();
            long lm = conn.getLastModified();
            
            this.cache = new Metadata(
                (type != null) ? type : "application/octet-stream",
                lm,
                true
            );
            log.info("Captured metadata for {}: MIME={}, LM={}", urlString, cache.mimeType, cache.lastModified);
        } catch (IOException e) {
            log.warn("Failed to reach remote resource {}: {}", urlString, e.getMessage());
            this.cache = new Metadata("application/octet-stream", 0, false);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Extracts the display name from the URL path or host.</p>
     */
    @Override
    public String getName() {
        URI uri = getUri();
        String path = uri.getPath();
        if (path != null && !path.isBlank()) {
            return new java.io.File(path).getName();
        }
        return uri.getHost() != null ? uri.getHost() : uri.getScheme();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the URI representation of the URL string.</p>
     */
    @Override
    public URI getUri() {
        return URI.create(urlString);
    }

    /** 
     * {@inheritDoc} 
     * <p>Triggers a metadata refresh and returns the cached MIME type.</p>
     */
    @Override
    public String getMimeType() {
        refreshMetadata();
        return cache.mimeType();
    }

    /** 
     * {@inheritDoc} 
     * <p>Triggers a metadata refresh and returns the cached modification timestamp.</p>
     */
    @Override
    public long getLastModified() {
        refreshMetadata();
        return cache.lastModified();
    }

    /** 
     * {@inheritDoc} 
     * <p>Triggers a metadata refresh and returns true if the remote source is reachable.</p>
     */
    @Override
    public boolean exists() {
        refreshMetadata();
        return cache.exists();
    }

    /** 
     * {@inheritDoc} 
     * <p>Opens a fresh input stream to the remote URL.</p>
     */
    @Override
    public InputStream openStream() throws IOException {
        log.info("Opening remote byte stream for: {}", urlString);
        URL url = new URL(urlString);
        URLConnection conn = url.openConnection();
        conn.setConnectTimeout(10000);
        return conn.getInputStream();
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: returns {@code false} as URLs represent 
     * persistent remote resources, not memory-backed snippets.</p>
     */
    @Override
    public boolean isVirtual() {
        return false;
    }

    /** 
     * {@inheritDoc} 
     * <p>Clears the metadata cache to force a fresh HEAD request on next access.</p>
     */
    @Override
    public void rebind() {
        super.rebind();
        this.cache = null; 
    }
}
