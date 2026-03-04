/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

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
 * This implementation includes Metadata Caching to prevent "Connection Storms". 
 * It performs a single efficient check to capture MIME type, existence, and 
 * last-modified status in one network round-trip.
 * </p>
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

    /** Record for holding captured metadata. */
    private record Metadata(String mimeType, long lastModified, boolean exists) {}

    /**
     * Refreshes the metadata cache if it is null.
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

    /** {@inheritDoc} */
    @Override
    public String getName() {
        URI uri = getUri();
        String path = uri.getPath();
        if (path != null && !path.isBlank()) {
            return new java.io.File(path).getName();
        }
        return uri.getHost() != null ? uri.getHost() : uri.getScheme();
    }

    /** {@inheritDoc} */
    @Override
    public URI getUri() {
        return URI.create(urlString);
    }

    /** {@inheritDoc} */
    @Override
    public String getMimeType() {
        refreshMetadata();
        return cache.mimeType();
    }

    /** {@inheritDoc} */
    @Override
    public long getLastModified() {
        refreshMetadata();
        return cache.lastModified();
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists() {
        refreshMetadata();
        return cache.exists();
    }

    /** {@inheritDoc} */
    @Override
    public InputStream openStream() throws IOException {
        log.info("Opening remote byte stream for: {}", urlString);
        URL url = new URL(urlString);
        URLConnection conn = url.openConnection();
        conn.setConnectTimeout(10000);
        return conn.getInputStream();
    }

    /** {@inheritDoc} */
    @Override
    public boolean isLocal() {
        return urlString.startsWith("file:");
    }

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        this.cache = null; // Force refresh after deserialization
    }
}
