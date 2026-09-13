/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.swing.agi.render;

import java.net.URI;
import javax.swing.JComponent;

/**
 * Universal interface for multimodal media viewer components across the Swing UI.
 * <p>
 * Unifies the visual rendering and interaction lifecycle for images, audio, and video
 * across chat message parts ({@code BlobPartPanel}), tool attachments
 * ({@code ToolResponseAttachmentsPanel}), and managed context resources ({@code ResourcePanel}).
 * </p>
 * 
 * @author anahata
 */
public interface MediaViewerComponent {

    /**
     * Returns the Swing JComponent to be embedded in the UI hierarchy.
     *
     * @return the Swing component.
     */
    JComponent getComponent();

    /**
     * Loads and initializes the media data for display and playback.
     *
     * @param data The raw binary data.
     * @param mimeType The detected MIME type (e.g. {@code "video/mp4"}, {@code "image/png"}, {@code "audio/wav"}).
     * @param displayName An optional human-readable name or file name for the media item.
     * @param sourceUri An optional source URI on disk or remote network.
     */
    void load(byte[] data, String mimeType, String displayName, URI sourceUri);

    /**
     * Stops active audio or video playback, pausing decoding threads.
     */
    void stop();

    /**
     * Permanently disposes of the component, releasing native decoders, textures, and timers.
     */
    void dispose();
}
