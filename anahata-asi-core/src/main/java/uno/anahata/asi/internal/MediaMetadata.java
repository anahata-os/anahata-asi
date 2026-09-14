/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.internal;

/**
 * Immutable domain descriptor holding dimensions, duration, and MIME classification
 * for any binary media asset (image, video, or audio).
 * <p>
 * Provides both native record component accessors (e.g. {@link #durationSeconds()}) and
 * JavaBean-style getter aliases (e.g. {@link #getDurationSeconds()}) for complete compatibility
 * across both modern Java constructs and reflection-driven UI renderers.
 * </p>
 * 
 * @param width The visual width in pixels, or 0 if acoustic audio or unknown.
 * @param height The visual height in pixels, or 0 if acoustic audio or unknown.
 * @param durationSeconds The duration of the media stream in seconds, or 0.0 if static image or unknown.
 * @param mimeType The canonical MIME type string (e.g. "image/png", "video/mp4", "audio/wav").
 * 
 * @author anahata
 */
public record MediaMetadata(
        int width,
        int height,
        double durationSeconds,
        String mimeType
) {

    /**
     * Checks whether this media asset is a visual image.
     * 
     * @return true if the MIME type starts with {@code "image/"}.
     */
    public boolean isImage() {
        return mimeType != null && mimeType.startsWith("image/");
    }

    /**
     * Checks whether this media asset is a moving video stream.
     * 
     * @return true if the MIME type starts with {@code "video/"}.
     */
    public boolean isVideo() {
        return mimeType != null && mimeType.startsWith("video/");
    }

    /**
     * Checks whether this media asset is an acoustic audio stream.
     * 
     * @return true if the MIME type starts with {@code "audio/"}.
     */
    public boolean isAudio() {
        return mimeType != null && mimeType.startsWith("audio/");
    }

    /**
     * JavaBean-compatible getter alias for visual width.
     * 
     * @return The width in pixels, or 0.
     */
    public int getWidth() {
        return width;
    }

    /**
     * JavaBean-compatible getter alias for visual height.
     * 
     * @return The height in pixels, or 0.
     */
    public int getHeight() {
        return height;
    }

    /**
     * JavaBean-compatible getter alias for stream duration.
     * 
     * @return The duration in seconds, or 0.0.
     */
    public double getDurationSeconds() {
        return durationSeconds;
    }

    /**
     * JavaBean-compatible getter alias for MIME type.
     * 
     * @return The MIME type string.
     */
    public String getMimeType() {
        return mimeType;
    }
}
