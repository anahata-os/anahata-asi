/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2.view;

import java.io.InputStream;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.resource.v2.handle.ResourceHandle;

/**
 * A resource view that interprets content as binary media (images, audio, etc.).
 */
@Slf4j
@Getter
@Setter
@NoArgsConstructor
public class MediaView extends AbstractResourceView {

    /** Cached binary data. */
    private byte[] cachedData;

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Reads all bytes from the handle. 
     * Includes a 10MB safety warning.</p>
     */
    @Override
    public void reload() throws Exception {
        ResourceHandle handle = owner.getHandle();
        log.debug("Reloading MediaView for: {}", handle.getUri());
        try (InputStream is = handle.openStream()) {
            this.cachedData = is.readAllBytes();
            if (cachedData.length > 10 * 1024 * 1024) {
                 log.warn("Media resource exceeds 10MB limit: {} ({} bytes)", handle.getUri(), cachedData.length);
            }
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Adds the cached binary data as a BlobPart to the RAG message.</p>
     */
    @Override
    public void populateRag(RagMessage ragMessage) throws Exception {
        if (cachedData != null) {
            ragMessage.addBlobPart(owner.getHandle().getMimeType(), cachedData);
        }
    }

    /** 
     * {@inheritDoc} 
     */
    @Override
    public int getTokenCount() {
        if (cachedData == null) {
            return 0;
        }
        return (int) (cachedData.length * 1.33 / 4);
    }
}
