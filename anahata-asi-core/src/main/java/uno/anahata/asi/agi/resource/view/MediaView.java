/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.resource.view;

import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import java.io.InputStream;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.provider.AbstractModel;

/**
 * A resource view that interprets content as binary media (images, audio, etc.).
 */
@Slf4j
@Getter
@Setter
@NoArgsConstructor
public class MediaView extends AbstractResourceView {

    /** Cached binary data. */
    private transient byte[] cachedData;

    /**
     * Returns the cached binary data, lazily reloading from the source handle if null
     * (e.g. following session deserialization from disk).
     *
     * @return The binary data, or null on read failure.
     */
    public byte[] getCachedData() {
        if (cachedData == null && owner != null && owner.getHandle() != null && owner.getHandle().exists()) {
            try {
                reload();
            } catch (Exception e) {
                log.error("Failed to lazily load media data for {}", owner.getHandle().getUri(), e);
            }
        }
        return cachedData;
    }

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
        byte[] data = getCachedData();
        if (data != null) {
            ragMessage.addBlobPart(owner.getHandle().getMimeType(), data);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Performs a lazy, model-specific token calculation of the active media content,
     * delegating to the selected model's generic raw bytes tokenizer.
     * </p>
     */
    @Override
    public int getTokenCount() {
        if (tokenCount == null) {
            AbstractModel model = getOwner() != null ? getOwner().getSelectedModel() : null;
            if (model == null) {
                return 0;
            }
            tokenCount = model.countTokens(getCachedData(), owner.getMimeType());
        }
        return tokenCount;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns 100.0 by default as the complete binary payload is provided.
     * Future releases will compute spatial bounding-box ratios for images or
     * temporal start/end second ratios for video and audio clipping.
     * </p>
     *
     * @return 100.0 by default.
     */
    @Override
    public double getVisiblePercentage() {
        return 100.0;
    }
}
