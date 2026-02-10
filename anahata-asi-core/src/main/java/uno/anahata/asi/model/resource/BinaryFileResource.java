/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.resource;

import io.swagger.v3.oas.annotations.media.Schema;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.imageio.ImageIO;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.model.core.BlobPart;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.resource.ResourceManager;

/**
 * A concrete resource representing a binary file (image, video, audio, etc.) 
 * loaded into the context.
 * It populates the context with {@link BlobPart}s, enabling multimodal 
 * capabilities in supported models.
 * 
 * @author anahata
 */
@Getter
@Setter
@Slf4j
@Schema(description = "A resource representing a binary file (image, video, etc.)")
public class BinaryFileResource extends AbstractPathResource<byte[], byte[]> {

    /** The detected MIME type of the binary data. */
    private String mimeType;
    
    /** Cached image dimensions for token estimation. */
    private int width = -1;
    private int height = -1;

    /**
     * Creates a new BinaryFileResource.
     * 
     * @param manager The parent resource manager.
     * @param path The path to the binary file.
     * @throws Exception if the initial setup fails.
     */
    public BinaryFileResource(ResourceManager manager, Path path) throws Exception {
        super(manager);
        this.setResource(path); 
        this.setPath(path.toAbsolutePath().toString());
        this.setName(path.getFileName().toString());
    }

    /**
     * {@inheritDoc}
     * Loads the file content as a byte array and detects its MIME type.
     */
    @Override
    public void reload() throws Exception {
        log.info("Reloading binary file resource: {}", getPath());
        byte[] data = Files.readAllBytes(getResource());
        this.setLoadLastModified(getCurrentLastModified());
        this.mimeType = TikaUtils.detectMimeType(getResource().toFile());
        this.cache = data;
        
        // Try to extract image dimensions for better token estimation
        if (mimeType != null && mimeType.startsWith("image/")) {
            try (ByteArrayInputStream bais = new ByteArrayInputStream(data)) {
                BufferedImage img = ImageIO.read(bais);
                if (img != null) {
                    this.width = img.getWidth();
                    this.height = img.getHeight();
                }
            } catch (Exception e) {
                log.warn("Failed to read image dimensions for token estimation: {}", getPath());
            }
        }
    }

    /**
     * {@inheritDoc}
     * Populates the message with a BlobPart containing the binary data.
     */
    @Override
    protected void populateFromCache(RagMessage rm) {
        if (cache != null && mimeType != null) {
            rm.addBlobPart(mimeType, cache);
        }
    }

    /** {@inheritDoc} */
    @Override
    public String getContentType() {
        return mimeType != null ? mimeType : "application/octet-stream";
    }

    /**
     * {@inheritDoc}
     * Returns a heuristic token count for binary data.
     */
    @Override
    public int getTokenCount() {
        if (cache == null) {
            return 0;
        }
        
        if (mimeType != null && mimeType.startsWith("image/")) {
            if (width > 0 && height > 0) {
                // Gemini-like heuristic: 258 tokens for base + tiles
                // A very rough guess: 258 + (pixels / 754)
                return 258 + (width * height / 754);
            }
            return 258; // Default for unknown image size
        }
        
        // For other binary data, assume Base64 overhead (~1.33x) 
        // and a rough 4 bytes per token estimate.
        return (int) (cache.length * 1.33 / 4);
    }
}
