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
import uno.anahata.asi.persistence.Rebindable;

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
     * Authoritatively retrieves the binary data for this media view,
     * ensuring the owner resource has executed reloadIfNeeded() so that
     * the cache is guaranteed to be fresh and loaded.
     *
     * @return The binary data byte array.
     * @throws Exception if reading fails.
     */
    public byte[] getData() throws Exception {
        owner.reloadIfNeeded();
        return cachedData;
    }

    /**
     * Pure, side-effect-free getter for the currently cached binary data.
     *
     * @return The cached byte array, or null if not yet loaded into memory.
     */
    public byte[] getCachedData() {
        return cachedData;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Returns true if binary data is resident in memory.
     * </p>
     */
    @Override
    public boolean hasContent() {
        return cachedData != null && cachedData.length > 0;
    }

    /** 
     * {@inheritDoc} 
     * <p>Implementation details: Reads all bytes from the handle. 
     * Includes a 10MB safety warning.</p>
     */
    @Override
    public void reload() throws Exception {
        resetTokenCount();
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
        byte[] data = getData();
        if (data != null && data.length > 0) {
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
            try {
                tokenCount = model.countTokens(getData(), owner.getMimeType());
            } catch (Exception e) {
                log.error("Failed to load media data in getTokenCount for {}", owner.getName(), e);
                tokenCount = 0;
            }
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
