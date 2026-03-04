/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.internal.TikaUtils;

/**
 * A resource handle that points to a file on the local filesystem.
 */
@Getter
@RequiredArgsConstructor
public class PathHandle extends AbstractResourceHandle {

    /** The absolute path to the local file. */
    @NonNull
    private final String path;

    /** {@inheritDoc} */
    @Override
    public String getName() {
        return new java.io.File(path).getName();
    }

    /** {@inheritDoc} */
    @Override
    public URI getUri() {
        return Paths.get(path).toUri();
    }

    /** {@inheritDoc} */
    @Override
    public String getMimeType() {
        try {
            return TikaUtils.detectMimeType(new java.io.File(path));
        } catch (Exception e) {
            return "application/octet-stream";
        }
    }

    /** {@inheritDoc} */
    @Override
    public long getLastModified() {
        try {
            return Files.getLastModifiedTime(Paths.get(path)).toMillis();
        } catch (IOException e) {
            return 0;
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists() {
        return Files.exists(Paths.get(path));
    }

    /** {@inheritDoc} */
    @Override
    public InputStream openStream() throws IOException {
        return Files.newInputStream(Paths.get(path));
    }

    /** {@inheritDoc} */
    @Override
    public boolean isLocal() {
        return true;
    }
}
