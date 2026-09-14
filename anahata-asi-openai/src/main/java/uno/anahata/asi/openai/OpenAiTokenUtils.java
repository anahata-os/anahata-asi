/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import uno.anahata.asi.internal.MediaMetadataUtils.ImageMetadata;

/**
 * Token counting utilities specific to OpenAI models and protocols.
 * <p>
 * Centralizes OpenAI-specific vision tiling algorithms and multimodal billing calculations,
 * keeping the core framework strictly provider-agnostic.
 * </p>
 * 
 * @author anahata
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class OpenAiTokenUtils {

    /**
     * Calculates the token consumption of an image under OpenAI's vision billing scheme.
     * <p>
     * Implements the official OpenAI High-Detail scaling and tiling algorithm:
     * </p>
     * <ol>
     *   <li>Proportional downscale to fit within a 2048x2048 bounding box.</li>
     *   <li>Proportional scale such that the shortest side is exactly 768px.</li>
     *   <li>Count of 512x512 pixel tiles needed to cover the scaled image.</li>
     *   <li>Billing of 170 tokens per tile plus a flat 85 tokens base cost.</li>
     * </ol>
     * 
     * @param metadata The parsed image dimensions and metadata.
     * @return The calculated high-detail token count, or 85 as a low-detail fallback.
     */
    public static int calculateOpenAiTileTokens(ImageMetadata metadata) {
        if (metadata == null) {
            return 85; // Low-detail flat-rate fallback
        }
        int width = metadata.getWidth();
        int height = metadata.getHeight();

        // 1. Proportional scale to fit within a 2048x2048 box
        if (width > 2048 || height > 2048) {
            double maxRatio = 2048.0 / Math.max(width, height);
            width = (int) (width * maxRatio);
            height = (int) (height * maxRatio);
        }

        // 2. Proportional scale shortest side to 768px
        double minRatio = 768.0 / Math.min(width, height);
        width = (int) (width * minRatio);
        height = (int) (height * minRatio);

        // 3. Count 512x512 pixel tiles
        int tilesW = (int) Math.ceil(width / 512.0);
        int tilesH = (int) Math.ceil(height / 512.0);

        // 4. Calculate final billing (170 tokens per tile + 85 tokens flat base cost)
        return (tilesW * tilesH) * 170 + 85;
    }
}
