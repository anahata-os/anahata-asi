/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TikaUtils;

/**
 * A filesystem-backed resource handle for physical local files.
 * <p>
 * This handle provides access to real files on the host filesystem and 
 * is categorized as a non-virtual (physical) source.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@RequiredArgsConstructor
public class PathHandle extends AbstractResourceHandle {

    /** The absolute path to the local file. */
    @NonNull
    private final String path;

    /** 
     * {@inheritDoc} 
     * <p>Extracts the file name from the path.</p>
     */
    @Override
    public String getName() {
        return new java.io.File(path).getName();
    }

    /** 
     * {@inheritDoc} 
     * <p>Returns the file:// URI for the path.</p>
     */
    @Override
    public URI getUri() {
        return Paths.get(path).toUri();
    }

    /** 
     * {@inheritDoc} 
     * <p>Uses Apache Tika for robust MIME detection of disk files.</p>
     */
    @Override
    public String getMimeType() {
        try {
            return TikaUtils.detectMimeType(new java.io.File(path));
        } catch (Exception e) {
            return "application/octet-stream";
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Retrieves the last modified time from the filesystem.</p>
     */
    @Override
    public long getLastModified() {
        try {
            return Files.getLastModifiedTime(Paths.get(path)).toMillis();
        } catch (IOException e) {
            return 0;
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Checks if the file exists on the filesystem.</p>
     */
    @Override
    public boolean exists() {
        return Files.exists(Paths.get(path));
    }

    /** 
     * {@inheritDoc} 
     * <p>Opens a standard filesystem input stream.</p>
     */
    @Override
    public InputStream openStream() throws IOException {
        return Files.newInputStream(Paths.get(path));
    }

    /** 
     * {@inheritDoc} 
     * <p>Determines writability based on OS permissions.</p>
     */
    @Override
    public boolean isWritable() {
        java.io.File file = new java.io.File(path);
        return file.exists() ? file.canWrite() : (file.getParentFile() != null && file.getParentFile().canWrite());
    }

    /** 
     * {@inheritDoc} 
     * <p>Writes content using atomic filesystem options (TRUNCATE_EXISTING).</p>
     */
    @Override
    public void write(String content) throws IOException {
        log.info("Persisting content to local file: {}", path);
        Files.writeString(Paths.get(path), content, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: returns {@code false} as this is a physical filesystem resource.</p>
     */
    @Override
    public boolean isVirtual() {
        return false;
    }
}
