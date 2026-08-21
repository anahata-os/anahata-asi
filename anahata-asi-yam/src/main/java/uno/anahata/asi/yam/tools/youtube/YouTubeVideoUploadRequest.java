/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.youtube;

import java.util.Collections;
import java.util.List;
import lombok.Builder;

/**
 * Request DTO encapsulating the metadata and source video file for uploading to YouTube.
 * <p>
 * Defines the local video file path, title, description, tags, target playlist ID,
 * and YouTube privacy status ({@code "unlisted"}, {@code "public"}, {@code "private"}).
 * </p>
 *
 * @param videoFilePath The absolute path of the video file (.mp4, .webm, .mov) on disk.
 * @param title The title of the video.
 * @param description The detailed description text.
 * @param tags The list of keyword tags for search indexing.
 * @param playlistId The optional target playlist ID to add the video to upon upload.
 * @param privacyStatus The privacy status: {@code "unlisted"} (default), {@code "public"}, or {@code "private"}.
 * 
 * @author anahata
 */
@Builder
public record YouTubeVideoUploadRequest(
        String videoFilePath,
        String title,
        String description,
        List<String> tags,
        String playlistId,
        String privacyStatus
) {

    /**
     * Canonical constructor providing defaults and unmodifiable collection copy.
     *
     * @param videoFilePath The video file path.
     * @param title The title.
     * @param description The description.
     * @param tags The tags list.
     * @param playlistId The playlist ID.
     * @param privacyStatus The privacy status.
     */
    public YouTubeVideoUploadRequest {
        tags = tags != null ? Collections.unmodifiableList(tags) : Collections.emptyList();
        if (privacyStatus == null || privacyStatus.isBlank()) {
            privacyStatus = "unlisted";
        }
    }

    /**
     * Creates a standard unlisted benchmark upload request.
     *
     * @param videoFilePath The video file path.
     * @param title The title.
     * @param description The description.
     * @param tags The tags.
     * @param playlistId The playlist ID.
     * @return The configured upload request.
     */
    public static YouTubeVideoUploadRequest of(String videoFilePath, String title, String description, List<String> tags, String playlistId) {
        return YouTubeVideoUploadRequest.builder()
                .videoFilePath(videoFilePath)
                .title(title)
                .description(description)
                .tags(tags)
                .playlistId(playlistId)
                .privacyStatus("unlisted")
                .build();
    }
}
