/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.internal;

import java.io.ByteArrayInputStream;
import java.util.Iterator;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;

/**
 * High-performance, lightweight utilities for extracting image metadata.
 * <p>
 * This class uses a header-only image stream reader to parse dimensions without performing
 * a heavy, memory-intensive pixel decode of the entire image array.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class MediaMetadataUtils {

    /**
     * Immutable container for parsed image dimensions and metadata.
     */
    @Value
    public static class ImageMetadata {
        /**
         * The parsed width of the image in pixels.
         */
        int width;
        
        /**
         * The parsed height of the image in pixels.
         */
        int height;
        
        /**
         * The MIME type of the parsed image (e.g. "image/png").
         */
        String mimeType;
    }

    /**
     * Reads the dimensions and metadata of an image from its raw byte array.
     * <p>
     * Utilizes standard JDK ImageReader SPI to parse metadata from headers. This is
     * extremely fast (microsecond latency) and consumes virtually zero CPU compared to
     * full image decoding.
     * </p>
     * 
     * @param data The raw image file bytes.
     * @return The parsed {@link ImageMetadata}, or null if reading fails or format is unsupported.
     */
    public static ImageMetadata readImageMetadata(byte[] data) {
        if (data == null || data.length == 0) {
            return null;
        }
        try (ImageInputStream iis = ImageIO.createImageInputStream(new ByteArrayInputStream(data))) {
            Iterator<ImageReader> readers = ImageIO.getImageReaders(iis);
            if (readers.hasNext()) {
                ImageReader reader = readers.next();
                try {
                    reader.setInput(iis);
                    int width = reader.getWidth(0);
                    int height = reader.getHeight(0);
                    String format = reader.getFormatName().toLowerCase();
                    return new ImageMetadata(width, height, "image/" + format);
                } finally {
                    reader.dispose();
                }
            }
        } catch (Exception e) {
            log.warn("Failed to parse image dimensions from headers, returning null", e);
        }
        return null;
    }

}
