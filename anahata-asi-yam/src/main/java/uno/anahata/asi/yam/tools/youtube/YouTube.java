/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.youtube;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * Pure Java YouTube Data API v3 toolkit providing autonomous video uploads and playlist management.
 * <p>
 * Implements the official Google YouTube Resumable Upload protocol using standard {@link HttpClient}
 * without external client libraries. Integrates with {@link YouTubeAuthHelper} for automated OAuth2 token
 * refreshment and browser-based login.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Pure Java YouTube Data API v3 toolkit for video uploads and playlist management. (Beta)")
public class YouTube extends AnahataToolkit {

    /**
     * Resumable upload initiation endpoint URL for YouTube Data API v3.
     */
    private static final String YOUTUBE_UPLOAD_ENDPOINT = "https://www.googleapis.com/upload/youtube/v3/videos?uploadType=resumable&part=snippet,status";

    /**
     * YouTube custom thumbnail upload endpoint URL.
     */
    private static final String YOUTUBE_THUMBNAIL_ENDPOINT = "https://www.googleapis.com/upload/youtube/v3/thumbnails/set?videoId=";

    /**
     * YouTube playlist items API endpoint URL.
     */
    private static final String YOUTUBE_PLAYLIST_ITEMS_ENDPOINT = "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet";

    /**
     * Shared JSON object mapper for request serialization and response parsing.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Shared HTTP client configured with a 30-second connect timeout and HTTP/2 support.
     */
    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_2)
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    /**
     * Default constructor for the YouTube toolkit.
     */
    public YouTube() {
    }

    /**
     * disabled on startup
     */
    @Override
    public void initialize() {
        super.initialize(); 
        getToolkit().setEnabled(false);
    }

    /**
     * Checks if YouTube OAuth2 credentials are configured and authenticated.
     *
     * @return A status message indicating authentication state.
     * @throws Exception If reading credentials fails.
     */
    @AgiTool("Checks if YouTube OAuth2 credentials and refresh tokens are configured.")
    public String getAuthStatus() throws Exception {
        if (!YouTubeCredentials.exists()) {
            return "YouTube credentials not configured. Use 'loginInteractive' to authenticate.";
        }
        YouTubeCredentials creds = YouTubeCredentials.load();
        if (creds.isAuthenticated()) {
            return "YouTube is fully authenticated (Client ID: " + creds.clientId()
                    + ", Playlist ID: " + (creds.playlistId() != null ? creds.playlistId() : "none") + ").";
        }
        return "YouTube credentials exist but lack refresh token. Run 'loginInteractive' to complete authorization.";
    }

    /**
     * Initiates the 1-click interactive browser login flow for YouTube.
     *
     * @param clientId The Google Cloud OAuth 2.0 Client ID.
     * @param clientSecret The Google Cloud OAuth 2.0 Client Secret.
     * @param playlistId The optional default YouTube playlist ID.
     * @return Confirmation message with saved credentials details.
     * @throws Exception If authentication fails or is cancelled.
     */
    @AgiTool("Launches interactive browser login to authorize YouTube video uploads.")
    public String loginInteractive(
            @AgiToolParam("The Google Cloud OAuth 2.0 Client ID.") String clientId,
            @AgiToolParam("The Google Cloud OAuth 2.0 Client Secret.") String clientSecret,
            @AgiToolParam("The optional default YouTube playlist ID.") String playlistId) throws Exception {
        log("Initiating YouTube OAuth2 browser login...");
        YouTubeCredentials creds = YouTubeAuthHelper.loginInteractive(clientId, clientSecret, playlistId);
        return "Successfully authenticated YouTube for client: " + creds.clientId()
                + ". Credentials saved to ~/.anahata/asi/youtube/credentials.json";
    }

    /**
     * Uploads a video to YouTube using the Resumable Upload protocol.
     *
     * @param request The upload request containing file path, metadata, tags, and playlist.
     * @return The public or unlisted URL of the uploaded video (e.g. {@code "https://youtu.be/..."}).
     * @throws Exception If the upload or authorization fails.
     */
    @AgiTool("Uploads a video to YouTube with metadata, tags, and optional playlist assignment.")
    public String uploadVideo(
            @AgiToolParam("The video upload request DTO.") YouTubeVideoUploadRequest request) throws Exception {
        Path videoPath = Paths.get(request.videoFilePath());
        if (!Files.exists(videoPath)) {
            throw new AgiToolException("Video file not found at path: " + request.videoFilePath());
        }

        long fileSize = Files.size(videoPath);
        if (fileSize == 0) {
            throw new AgiToolException("Video file is empty (0 bytes): " + request.videoFilePath());
        }

        YouTubeCredentials credentials = YouTubeCredentials.load();
        String accessToken = YouTubeAuthHelper.getValidAccessToken(credentials);

        log("Step 1: Initializing YouTube resumable upload session for: " + request.title() + " (" + fileSize + " bytes)");
        String uploadUrl = initializeResumableUpload(accessToken, request, fileSize);

        log("Step 2: Streaming video payload (" + (fileSize / (1024 * 1024)) + " MB) to YouTube...");
        String videoId = streamVideoFile(uploadUrl, videoPath, fileSize);

        String finalPlaylistId = request.playlistId() != null && !request.playlistId().isBlank()
                ? request.playlistId()
                : credentials.playlistId();

        if (finalPlaylistId != null && !finalPlaylistId.isBlank()) {
            log("Step 3: Adding uploaded video (" + videoId + ") to playlist: " + finalPlaylistId);
            try {
                addVideoToPlaylistInternal(accessToken, videoId, finalPlaylistId);
            } catch (Exception e) {
                log.error("Failed to add video to playlist, but video upload succeeded", e);
            }
        }

        String videoUrl = "https://youtu.be/" + videoId;
        log("YouTube video upload completed successfully: " + videoUrl);
        return videoUrl;
    }

    /**
     * Convenience tool to upload a video with discrete primitive parameters.
     *
     * @param videoFilePath The absolute path of the video file.
     * @param title The video title.
     * @param description The video description.
     * @param tags The list of tags.
     * @param playlistId The optional target playlist ID.
     * @param privacyStatus The privacy status ("unlisted", "public", "private").
     * @return The resulting YouTube URL.
     * @throws Exception If upload fails.
     */
    @AgiTool("Uploads a video to YouTube with discrete parameters.")
    public String uploadVideoToPlaylist(
            @AgiToolParam("The absolute path of the video file on disk.") String videoFilePath,
            @AgiToolParam("The title of the video.") String title,
            @AgiToolParam("The description of the video.") String description,
            @AgiToolParam("List of search tags.") List<String> tags,
            @AgiToolParam("The target playlist ID.") String playlistId,
            @AgiToolParam("The privacy status (unlisted, public, private).") String privacyStatus) throws Exception {
        YouTubeVideoUploadRequest request = YouTubeVideoUploadRequest.builder()
                .videoFilePath(videoFilePath)
                .title(title)
                .description(description)
                .tags(tags)
                .playlistId(playlistId)
                .privacyStatus(privacyStatus)
                .build();
        return uploadVideo(request);
    }

    /**
     * Sets the custom video thumbnail for an uploaded YouTube video.
     *
     * @param videoId The 11-character YouTube video ID.
     * @param imagePath The absolute path of the thumbnail image file (.png or .jpg).
     * @return Confirmation message with the video ID.
     * @throws Exception If thumbnail upload or authorization fails.
     */
    @AgiTool("Sets the custom thumbnail image for a YouTube video.")
    public String setThumbnail(
            @AgiToolParam("The YouTube video ID.") String videoId,
            @AgiToolParam(value = "The absolute path of the image file (.png or .jpg).", rendererId = "path") String imagePath) throws Exception {
        Path thumbPath = Paths.get(imagePath);
        if (!Files.exists(thumbPath)) {
            throw new AgiToolException("Thumbnail image not found at: " + imagePath);
        }

        YouTubeCredentials credentials = YouTubeCredentials.load();
        String accessToken = YouTubeAuthHelper.getValidAccessToken(credentials);
        setThumbnailInternal(accessToken, videoId, thumbPath);
        return "Successfully set custom thumbnail for video " + videoId + " from " + imagePath;
    }

    /**
     * Adds an existing YouTube video to a specified playlist.
     *
     * @param videoId The 11-character YouTube video ID.
     * @param playlistId The target YouTube playlist ID.
     * @return Confirmation message.
     * @throws Exception If the playlist insertion fails.
     */
    @AgiTool("Adds an uploaded YouTube video to a specified playlist.")
    public String addVideoToPlaylist(
            @AgiToolParam("The YouTube video ID.") String videoId,
            @AgiToolParam("The target playlist ID.") String playlistId) throws Exception {
        YouTubeCredentials credentials = YouTubeCredentials.load();
        String accessToken = YouTubeAuthHelper.getValidAccessToken(credentials);
        addVideoToPlaylistInternal(accessToken, videoId, playlistId);
        return "Successfully added video " + videoId + " to playlist " + playlistId;
    }

    /**
     * Initiates the resumable upload session with YouTube Data API v3 and retrieves the upload URL.
     *
     * @param accessToken The OAuth2 access token.
     * @param request The video upload metadata request.
     * @param fileSize The size of the video file in bytes.
     * @return The resumable upload target URI.
     * @throws Exception If initiation fails.
     */
    private String initializeResumableUpload(String accessToken, YouTubeVideoUploadRequest request, long fileSize) throws Exception {
        ObjectNode root = MAPPER.createObjectNode();

        // Snippet
        ObjectNode snippet = root.putObject("snippet");
        snippet.put("title", request.title());
        snippet.put("description", request.description());
        snippet.put("categoryId", "28"); // Science & Technology

        if (request.tags() != null && !request.tags().isEmpty()) {
            ArrayNode tagsArray = snippet.putArray("tags");
            request.tags().forEach(tagsArray::add);
        }

        // Status
        ObjectNode status = root.putObject("status");
        status.put("privacyStatus", request.privacyStatus());
        status.put("selfDeclaredMadeForKids", false);

        String jsonBody = MAPPER.writeValueAsString(root);

        HttpRequest initRequest = HttpRequest.newBuilder()
                .uri(URI.create(YOUTUBE_UPLOAD_ENDPOINT))
                .header("Authorization", "Bearer " + accessToken)
                .header("Content-Type", "application/json; charset=UTF-8")
                .header("X-Upload-Content-Type", "video/mp4")
                .header("X-Upload-Content-Length", String.valueOf(fileSize))
                .POST(HttpRequest.BodyPublishers.ofString(jsonBody))
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(initRequest, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
            log.error("Resumable upload initialization failed: HTTP {} - {}", response.statusCode(), response.body());
            throw new AgiToolException("YouTube resumable upload initialization failed: HTTP "
                    + response.statusCode() + " - " + response.body());
        }

        return response.headers().firstValue("Location")
                .orElseThrow(() -> new AgiToolException("YouTube API did not return Location header for resumable upload."));
    }

    /**
     * Streams the binary video file content to the resumable upload URL.
     *
     * @param uploadUrl The upload URL returned by the initialization step.
     * @param videoPath The path to the local video file.
     * @param fileSize The size of the file in bytes.
     * @return The extracted YouTube video ID.
     * @throws Exception If binary streaming fails.
     */
    private String streamVideoFile(String uploadUrl, Path videoPath, long fileSize) throws Exception {
        HttpRequest streamRequest = HttpRequest.newBuilder()
                .uri(URI.create(uploadUrl))
                .header("Content-Type", "video/mp4")
                .header("Content-Length", String.valueOf(fileSize))
                .PUT(HttpRequest.BodyPublishers.ofFile(videoPath))
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(streamRequest, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200 && response.statusCode() != 201) {
            log.error("Video stream upload failed: HTTP {} - {}", response.statusCode(), response.body());
            throw new AgiToolException("Video stream upload failed: HTTP " + response.statusCode() + " - " + response.body());
        }

        JsonNode json = MAPPER.readTree(response.body());
        if (!json.has("id")) {
            throw new AgiToolException("YouTube response did not contain video ID: " + response.body());
        }

        return json.get("id").asText();
    }

    /**
     * Uploads the thumbnail binary stream to YouTube's thumbnail endpoint.
     *
     * @param accessToken The OAuth2 access token.
     * @param videoId The video ID.
     * @param thumbPath The path to the image file.
     * @throws Exception If thumbnail upload fails.
     */
    private void setThumbnailInternal(String accessToken, String videoId, Path thumbPath) throws Exception {
        String mimeType = thumbPath.toString().toLowerCase().endsWith(".png") ? "image/png" : "image/jpeg";

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(YOUTUBE_THUMBNAIL_ENDPOINT + videoId))
                .header("Authorization", "Bearer " + accessToken)
                .header("Content-Type", mimeType)
                .POST(HttpRequest.BodyPublishers.ofFile(thumbPath))
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200 && response.statusCode() != 201) {
            log.warn("Failed to set custom thumbnail for video {}: HTTP {} - {}", videoId, response.statusCode(), response.body());
            throw new IOException("Failed to set YouTube custom thumbnail: HTTP " + response.statusCode() + " - " + response.body());
        }
        log.info("Successfully set custom thumbnail for YouTube video {}", videoId);
    }

    /**
     * Adds an uploaded video to a YouTube playlist via playlistItems API.
     *
     * @param accessToken The OAuth2 access token.
     * @param videoId The video ID.
     * @param playlistId The playlist ID.
     * @throws Exception If insertion fails.
     */
    private void addVideoToPlaylistInternal(String accessToken, String videoId, String playlistId) throws Exception {
        ObjectNode root = MAPPER.createObjectNode();
        ObjectNode snippet = root.putObject("snippet");
        snippet.put("playlistId", playlistId);
        ObjectNode resourceId = snippet.putObject("resourceId");
        resourceId.put("kind", "youtube#video");
        resourceId.put("videoId", videoId);

        String jsonBody = MAPPER.writeValueAsString(root);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(YOUTUBE_PLAYLIST_ITEMS_ENDPOINT))
                .header("Authorization", "Bearer " + accessToken)
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(jsonBody))
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200 && response.statusCode() != 201) {
            log.warn("Failed to insert video into playlist: HTTP {} - {}", response.statusCode(), response.body());
            throw new IOException("Failed to add video to playlist: HTTP " + response.statusCode() + " - " + response.body());
        }
        log.info("Successfully added video {} to playlist {}", videoId, playlistId);
    }
}
