/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.handle;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
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
 * <b>Virtual Strategy:</b> This handle represents physical remote content, so
 * it returns {@code false} for {@link #isVirtual()}.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
@RequiredArgsConstructor
public class UrlHandle extends AbstractResourceHandle {

    /**
     * The full URL string for the resource.
     */
    @NonNull
    private final String urlString;

    /**
     * Cached metadata to prevent redundant connections.
     */
    private transient Metadata cache;

    /**
     * Record for holding captured metadata.
     *
     * @param mimeType The detected MIME type.
     * @param lastModified The remote last modified timestamp.
     * @param exists Whether the remote source is reachable.
     * @param contentLength The size of the remote resource in bytes.
     */
    private record Metadata(String mimeType, long lastModified, boolean exists, long contentLength) {

    }

    /**
     * Recursively follows HTTP redirects for the given URL and method.
     * <p>
     * This method handles common redirect status codes (301, 302, 303, 307, 308)
     * and updates the target URL from the 'Location' header.
     * </p>
     *
     * @param targetUrl The starting URL string.
     * @param method The HTTP method to use (e.g., HEAD, GET).
     * @return A connected HttpURLConnection pointing to the final destination.
     * @throws IOException If a network error occurs or the redirect limit is reached.
     */
    private HttpURLConnection followRedirects(String targetUrl, String method) throws IOException {
        URL url = new URL(targetUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setConnectTimeout(10000);
        conn.setReadTimeout(10000);
        conn.setRequestMethod(method);
        conn.setRequestProperty("User-Agent", "Mozilla/5.0 (Compatible; AnahataASI/1.0)");

        int status = conn.getResponseCode();
        if (status == HttpURLConnection.HTTP_MOVED_TEMP
                || status == HttpURLConnection.HTTP_MOVED_PERM
                || status == HttpURLConnection.HTTP_SEE_OTHER
                || status == 307 || status == 308) {
            String newUrl = conn.getHeaderField("Location");
            log.info("Following redirect from {} to {}", targetUrl, newUrl);
            return followRedirects(newUrl, method);
        }
        return conn;
    }

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
            HttpURLConnection conn = followRedirects(urlString, "HEAD");

            String type = conn.getContentType();
            long lm = conn.getLastModified();
            long cl = conn.getContentLengthLong();

            this.cache = new Metadata(
                    (type != null) ? type : "application/octet-stream",
                    lm,
                    true,
                    cl
            );
            log.info("Captured metadata for {}: MIME={}, LM={}, CL={}", urlString, cache.mimeType, cache.lastModified, cache.contentLength);
            conn.disconnect();
        } catch (IOException e) {
            log.warn("Failed to reach remote resource {}: {}", urlString, e.getMessage());
            this.cache = new Metadata("application/octet-stream", 0, false, -1L);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Extracts the display name from the URL path or host.</p>
     */
    @Override
    public String getName() {
        URI uri = getUri();
        String path = uri.getPath();
        if (path != null && !path.isBlank()) {
            return new File(path).getName();
        }
        return uri.getHost() != null ? uri.getHost() : uri.getScheme();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the URI representation of the URL string.</p>
     */
    @Override
    public URI getUri() {
        return URI.create(urlString);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Triggers a metadata refresh and returns the cached MIME type.</p>
     */
    @Override
    public String getMimeType() {
        refreshMetadata();
        return cache.mimeType();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Triggers a metadata refresh and returns the cached modification
     * timestamp.</p>
     */
    @Override
    public long getLastModified() {
        refreshMetadata();
        return cache.lastModified();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Triggers a metadata refresh and returns true if the remote source is
     * reachable.</p>
     */
    @Override
    public boolean exists() {
        refreshMetadata();
        return cache.exists();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Opens a fresh input stream to the remote URL.</p>
     */
    @Override
    public InputStream openStream() throws IOException {
        log.info("Opening remote byte stream for: {}", urlString);
        HttpURLConnection conn = followRedirects(urlString, "GET");
        return conn.getInputStream();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: returns {@code false} as URLs represent
     * persistent remote resources, not memory-backed snippets.</p>
     */
    @Override
    public boolean isVirtual() {
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: For LIVE refresh policy, bypasses the local metadata
     * cache to query the remote server via a fresh HEAD request, detecting remote
     * modifications accurately.
     * </p>
     */
    @Override
    public boolean isStale(long lastLoadTimestamp) {
        this.cache = null; // Invalidate cache to force a fresh remote HEAD check
        refreshMetadata();
        long remoteLm = cache.lastModified();
        if (remoteLm > 0) {
            return remoteLm > lastLoadTimestamp;
        }
        return lastLoadTimestamp == -1;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Clears the metadata cache to force a fresh HEAD request on next
     * access.</p>
     */
    @Override
    public void rebind() {
        super.rebind();
        this.cache = null;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns the cached content length from the remote server.</p>
     */
    @Override
    public long length() {
        refreshMetadata();
        return cache.contentLength();
    }
}
