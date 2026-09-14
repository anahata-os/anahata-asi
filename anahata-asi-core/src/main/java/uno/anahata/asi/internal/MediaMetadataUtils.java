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

    /**
     * Universally reads metadata for any binary media asset (image, video, or audio).
     *
     * @param data The raw binary data.
     * @param mimeType The MIME type (e.g. "image/png", "video/mp4", "audio/wav").
     * @return The parsed {@link MediaMetadata}, or null if reading fails or format is unsupported.
     */
    public static MediaMetadata readMediaMetadata(byte[] data, String mimeType) {
        if (data == null || data.length == 0) {
            return null;
        }

        String cleanMime = mimeType != null ? mimeType.toLowerCase().split(";")[0].trim() : "";

        // 1. Visual Images
        if (cleanMime.startsWith("image/")) {
            ImageMetadata im = readImageMetadata(data);
            if (im != null) {
                return new MediaMetadata(im.getWidth(), im.getHeight(), 0.0, im.getMimeType());
            }
            return null;
        }

        // 2. MP4 / MOV Video or M4A Audio (ISO Base Media File Format)
        if (cleanMime.equals("video/mp4") || cleanMime.equals("video/quicktime")
                || cleanMime.equals("audio/mp4") || cleanMime.equals("audio/x-m4a")
                || cleanMime.equals("audio/aac") || cleanMime.startsWith("video/")) {
            MediaMetadata isoMeta = parseIsoBaseMedia(data, cleanMime);
            if (isoMeta != null) {
                return isoMeta;
            }
        }

        // 3. WAV Audio (RIFF Format)
        if (cleanMime.contains("wav")) {
            MediaMetadata wavMeta = parseWav(data, cleanMime);
            if (wavMeta != null) {
                return wavMeta;
            }
        }

        // 4. JDK AudioSystem Fallback
        if (cleanMime.startsWith("audio/")) {
            MediaMetadata audioSysMeta = parseViaAudioSystem(data, cleanMime);
            if (audioSysMeta != null) {
                return audioSysMeta;
            }
        }

        return null;
    }

    /**
     * Parses duration and dimensions from an ISO Base Media File (MP4, MOV, M4A).
     *
     * @param data The raw file bytes.
     * @param mimeType The MIME type.
     * @return The parsed metadata, or null if parsing fails.
     */
    private static MediaMetadata parseIsoBaseMedia(byte[] data, String mimeType) {
        if (data == null || data.length < 16) {
            return null;
        }

        double durationSeconds = 0.0;
        int videoWidth = 0;
        int videoHeight = 0;

        int offset = 0;
        int len = data.length;

        while (offset + 8 <= len) {
            long size = readUint32(data, offset);
            String type = readFourCc(data, offset + 4);

            int headerSize = 8;
            if (size == 1) {
                if (offset + 16 > len) break;
                size = readUint64(data, offset + 8);
                headerSize = 16;
            } else if (size == 0) {
                size = len - offset;
            }

            if (size < headerSize || offset + size > len) {
                break;
            }

            if ("moov".equals(type)) {
                int subOffset = offset + headerSize;
                int moovEnd = (int) (offset + size);

                while (subOffset + 8 <= moovEnd) {
                    long subSize = readUint32(data, subOffset);
                    String subType = readFourCc(data, subOffset + 4);

                    int subHeaderSize = 8;
                    if (subSize == 1) {
                        if (subOffset + 16 > moovEnd) break;
                        subSize = readUint64(data, subOffset + 8);
                        subHeaderSize = 16;
                    } else if (subSize == 0) {
                        subSize = moovEnd - subOffset;
                    }

                    if (subSize < subHeaderSize || subOffset + subSize > moovEnd) {
                        break;
                    }

                    if ("mvhd".equals(subType)) {
                        int boxDataOffset = subOffset + subHeaderSize;
                        if (boxDataOffset < moovEnd) {
                            int version = data[boxDataOffset] & 0xFF;
                            if (version == 0) {
                                if (boxDataOffset + 20 <= moovEnd) {
                                    long timeScale = readUint32(data, boxDataOffset + 12);
                                    long duration = readUint32(data, boxDataOffset + 16);
                                    if (timeScale > 0) {
                                        durationSeconds = (double) duration / timeScale;
                                    }
                                }
                            } else if (version == 1) {
                                if (boxDataOffset + 32 <= moovEnd) {
                                    long timeScale = readUint32(data, boxDataOffset + 20);
                                    long duration = readUint64(data, boxDataOffset + 24);
                                    if (timeScale > 0) {
                                        durationSeconds = (double) duration / timeScale;
                                    }
                                }
                            }
                        }
                    } else if ("trak".equals(subType)) {
                        int trakOffset = subOffset + subHeaderSize;
                        int trakEnd = (int) (subOffset + subSize);
                        while (trakOffset + 8 <= trakEnd) {
                            long tSize = readUint32(data, trakOffset);
                            String tType = readFourCc(data, trakOffset + 4);
                            int tHeaderSize = 8;
                            if (tSize == 1) {
                                if (trakOffset + 16 > trakEnd) break;
                                tSize = readUint64(data, trakOffset + 8);
                                tHeaderSize = 16;
                            } else if (tSize == 0) {
                                tSize = trakEnd - trakOffset;
                            }
                            if (tSize < tHeaderSize || trakOffset + tSize > trakEnd) break;

                            if ("tkhd".equals(tType)) {
                                int tDataOffset = trakOffset + tHeaderSize;
                                int tVersion = data[tDataOffset] & 0xFF;
                                int wOffset = (tVersion == 0) ? (tDataOffset + 76) : (tDataOffset + 88);
                                if (wOffset + 8 <= trakEnd) {
                                    int w = (int) (readUint32(data, wOffset) >> 16);
                                    int h = (int) (readUint32(data, wOffset + 4) >> 16);
                                    if (w > 0 && h > 0 && videoWidth == 0) {
                                        videoWidth = w;
                                        videoHeight = h;
                                    }
                                }
                            }
                            trakOffset += tSize;
                        }
                    }

                    subOffset += subSize;
                }
                break;
            }

            offset += size;
        }

        if (durationSeconds > 0.0 || videoWidth > 0) {
            return new MediaMetadata(videoWidth, videoHeight, durationSeconds, mimeType);
        }
        return null;
    }

    /**
     * Parses duration and audio parameters from a RIFF WAV byte array.
     *
     * @param data The raw WAV file bytes.
     * @param mimeType The MIME type.
     * @return The parsed metadata, or null if parsing fails.
     */
    private static MediaMetadata parseWav(byte[] data, String mimeType) {
        if (data == null || data.length < 44) {
            return null;
        }
        if (data[0] != 'R' || data[1] != 'I' || data[2] != 'F' || data[3] != 'F') {
            return null;
        }
        if (data[8] != 'W' || data[9] != 'A' || data[10] != 'V' || data[11] != 'E') {
            return null;
        }

        long byteRate = 0;
        long dataSize = 0;

        int offset = 12;
        int len = data.length;
        while (offset + 8 <= len) {
            String chunkId = new String(data, offset, 4, java.nio.charset.StandardCharsets.US_ASCII);
            long chunkSize = readUint32LittleEndian(data, offset + 4);
            int dataOffset = offset + 8;

            if ("fmt ".equals(chunkId)) {
                if (dataOffset + 16 <= len) {
                    byteRate = readUint32LittleEndian(data, dataOffset + 8);
                }
            } else if ("data".equals(chunkId)) {
                dataSize = Math.min(chunkSize, len - dataOffset);
            }

            offset = dataOffset + (int) chunkSize;
            if (chunkSize % 2 != 0) {
                offset++;
            }
        }

        if (byteRate > 0 && dataSize > 0) {
            double duration = (double) dataSize / byteRate;
            return new MediaMetadata(0, 0, duration, mimeType);
        }
        return null;
    }

    /**
     * Fallback audio duration lookup using the standard JDK AudioSystem SPI.
     *
     * @param data The raw audio bytes.
     * @param mimeType The MIME type.
     * @return The parsed metadata, or null on unsupported/headless runtimes.
     */
    private static MediaMetadata parseViaAudioSystem(byte[] data, String mimeType) {
        try (ByteArrayInputStream bais = new ByteArrayInputStream(data)) {
            javax.sound.sampled.AudioFileFormat format = javax.sound.sampled.AudioSystem.getAudioFileFormat(bais);
            if (format != null && format.getFrameLength() > 0 && format.getFormat().getFrameRate() > 0) {
                double duration = (double) format.getFrameLength() / format.getFormat().getFrameRate();
                return new MediaMetadata(0, 0, duration, mimeType);
            }
        } catch (Throwable ignored) {
            // Safe fallback for headless runtimes, minimal JREs, or unsupported audio formats
        }
        return null;
    }

    /**
     * Reads a 32-bit big-endian unsigned integer from the specified byte offset.
     *
     * @param b The source byte array.
     * @param offset The zero-based starting byte offset.
     * @return The parsed 32-bit unsigned value as a long.
     */
    private static long readUint32(byte[] b, int offset) {
        return ((long) (b[offset] & 0xFF) << 24)
                | ((long) (b[offset + 1] & 0xFF) << 16)
                | ((long) (b[offset + 2] & 0xFF) << 8)
                | ((long) (b[offset + 3] & 0xFF));
    }

    /**
     * Reads a 32-bit little-endian unsigned integer from the specified byte offset.
     *
     * @param b The source byte array.
     * @param offset The zero-based starting byte offset.
     * @return The parsed 32-bit unsigned value as a long.
     */
    private static long readUint32LittleEndian(byte[] b, int offset) {
        return ((long) (b[offset] & 0xFF))
                | ((long) (b[offset + 1] & 0xFF) << 8)
                | ((long) (b[offset + 2] & 0xFF) << 16)
                | ((long) (b[offset + 3] & 0xFF) << 24);
    }

    /**
     * Reads a 64-bit big-endian unsigned integer from the specified byte offset.
     *
     * @param b The source byte array.
     * @param offset The zero-based starting byte offset.
     * @return The parsed 64-bit unsigned value as a long.
     */
    private static long readUint64(byte[] b, int offset) {
        long high = readUint32(b, offset);
        long low = readUint32(b, offset + 4);
        return (high << 32) | (low & 0xFFFFFFFFL);
    }

    /**
     * Reads a 4-character ASCII FourCC string identifier from the specified byte offset.
     *
     * @param b The source byte array.
     * @param offset The zero-based starting byte offset.
     * @return The 4-character ASCII string.
     */
    private static String readFourCc(byte[] b, int offset) {
        return new String(b, offset, 4, java.nio.charset.StandardCharsets.US_ASCII);
    }

}
