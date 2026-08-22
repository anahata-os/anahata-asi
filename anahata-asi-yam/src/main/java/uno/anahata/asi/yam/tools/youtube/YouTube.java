/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.youtube;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.awt.GraphicsEnvironment;
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.yam.tools.screenrecording.ScreenRecordingOverlay;
import uno.anahata.asi.yam.tools.screenrecording.ScreenRecorder;
import uno.anahata.asi.yam.tools.screenrecording.RecordedSession;

/**
 * Pure Java YouTube Data API v3 toolkit providing autonomous video uploads,
 * screen recording publishing, playlist management, and channel analytics.
 * <p>
 * Implements the official Google YouTube Resumable Upload protocol using standard {@link HttpClient}
 * without external client libraries. Integrates with {@link YouTubeAuthHelper} for automated OAuth2 token
 * refreshment and browser-based login.
 * </p>
 *
 * @author anahata
 */
@Slf4j
// NOTE TO AI: Do NOT remove (Beta) or remove getToolkit().setEnabled(false). Keep disabled on startup by default.
@AgiToolkit("Pure Java YouTube Data API v3 toolkit for video uploads, screen recording, and channel management (Beta).")
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
     * Active standalone screen recorder instance.
     */
    private transient ScreenRecorder activeRecorder;

    /**
     * Default constructor for the YouTube toolkit.
     */
    public YouTube() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Disables the YouTube toolkit on startup by default.
     * </p>
     */
    @Override
    public void initialize() {
        getToolkit().setEnabled(false);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Injects live YouTube channel metadata, statistics, playlists, and recent uploads into the RAG message.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        if (!YouTubeCredentials.exists()) {
            ragMessage.addTextPart("## YouTube Status\n- **Authenticated**: ❌ NO (Not configured)\n- **Action**: Run `YouTube.login()` to authenticate via browser.\n");
            return;
        }

        try {
            YouTubeCredentials creds = YouTubeCredentials.load();
            if (!creds.isAuthenticated()) {
                ragMessage.addTextPart("## YouTube Status\n- **Authenticated**: ❌ NO (Missing refresh token)\n- **Action**: Run `YouTube.login()` to authorize channel.\n");
                return;
            }

            String accessToken = YouTubeAuthHelper.getValidAccessToken(creds);
            StringBuilder sb = new StringBuilder("## YouTube Channel & Telemetry\n");

            // 1. Fetch Channel Details & Statistics
            String channelUrl = "https://www.googleapis.com/youtube/v3/channels?part=snippet,statistics,contentDetails&mine=true";
            HttpRequest chReq = HttpRequest.newBuilder().uri(URI.create(channelUrl)).header("Authorization", "Bearer " + accessToken).GET().build();
            HttpResponse<String> chRes = HTTP_CLIENT.send(chReq, HttpResponse.BodyHandlers.ofString());

            String uploadsPlaylistId = null;
            if (chRes.statusCode() == 200) {
                JsonNode chJson = MAPPER.readTree(chRes.body());
                JsonNode items = chJson.path("items");
                if (!items.isEmpty()) {
                    JsonNode ch = items.get(0);
                    JsonNode snip = ch.path("snippet");
                    JsonNode stats = ch.path("statistics");
                    uploadsPlaylistId = ch.path("contentDetails").path("relatedPlaylists").path("uploads").asText(null);

                    sb.append("- **Channel**: ").append(snip.path("title").asText()).append(" (Handle: `").append(snip.path("customUrl").asText("@unknown")).append("`)\n");
                    sb.append("- **Subscribers**: ").append(stats.path("subscriberCount").asText("0"))
                      .append(" | **Total Views**: ").append(stats.path("viewCount").asText("0"))
                      .append(" | **Total Videos**: ").append(stats.path("videoCount").asText("0")).append("\n");
                    sb.append("- **Default Playlist ID**: ").append(creds.playlistId() != null ? creds.playlistId() : "None").append("\n");
                }
            } else {
                sb.append("- **Authenticated**: ✅ YES\n");
            }

            // 2. Fetch Playlists
            String plUrl = "https://www.googleapis.com/youtube/v3/playlists?part=snippet,contentDetails&mine=true&maxResults=50";
            HttpRequest plReq = HttpRequest.newBuilder().uri(URI.create(plUrl)).header("Authorization", "Bearer " + accessToken).GET().build();
            HttpResponse<String> plRes = HTTP_CLIENT.send(plReq, HttpResponse.BodyHandlers.ofString());

            if (plRes.statusCode() == 200) {
                JsonNode plJson = MAPPER.readTree(plRes.body());
                JsonNode plItems = plJson.path("items");
                if (!plItems.isEmpty()) {
                    sb.append("\n### Channel Playlists (").append(plItems.size()).append(")\n");
                    for (JsonNode pl : plItems) {
                        String id = pl.path("id").asText();
                        String title = pl.path("snippet").path("title").asText();
                        int count = pl.path("contentDetails").path("itemCount").asInt(0);
                        sb.append("- **").append(title).append("** (ID: `").append(id).append("`) — ").append(count).append(" videos\n");
                    }
                }
            }

            // 3. Fetch Top 20 Most Recent Uploads
            if (uploadsPlaylistId != null && !uploadsPlaylistId.isBlank()) {
                String vUrl = "https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&playlistId=" + uploadsPlaylistId + "&maxResults=20";
                HttpRequest vReq = HttpRequest.newBuilder().uri(URI.create(vUrl)).header("Authorization", "Bearer " + accessToken).GET().build();
                HttpResponse<String> vRes = HTTP_CLIENT.send(vReq, HttpResponse.BodyHandlers.ofString());

                if (vRes.statusCode() == 200) {
                    JsonNode vJson = MAPPER.readTree(vRes.body());
                    JsonNode vItems = vJson.path("items");
                    if (!vItems.isEmpty()) {
                        sb.append("\n### Recent Uploads (Top ").append(vItems.size()).append(")\n");
                        for (JsonNode v : vItems) {
                            JsonNode vSnip = v.path("snippet");
                            String title = vSnip.path("title").asText();
                            String vid = vSnip.path("resourceId").path("videoId").asText();
                            String pubDate = vSnip.path("publishedAt").asText();
                            if (pubDate.length() > 10) {
                                pubDate = pubDate.substring(0, 10);
                            }
                            sb.append("- [").append(pubDate).append("] **").append(title).append("** — https://youtu.be/").append(vid).append("\n");
                        }
                    }
                }
            }

            ragMessage.addTextPart(sb.toString());

        } catch (Exception e) {
            log.error("Error populating YouTube RAG message", e);
            ragMessage.addTextPart("## YouTube Status\n- ⚠️ Telemetry fetch error: " + ExceptionUtils.getStackTrace(e) + "\n");
        }
    }

    /**
     * Launches the screen recording overlay to record the screen and publish directly to YouTube.
     *
     * @param videoTitle The title for the uploaded video.
     * @param playlistId The optional target playlist ID.
     * @param privacyStatus The privacy status: "unlisted", "public", or "private".
     * @return Confirmation with resulting video URL upon completion.
     * @throws Exception If recording or upload fails.
     */
    @AgiTool(value = "Launches the screen recording overlay to record desktop activity and publish to YouTube.", permission = ToolPermission.APPROVE_ALWAYS)
    public String startScreenRecording(
            @AgiToolParam("The title for the recorded YouTube video.") String videoTitle,
            @AgiToolParam(value = "Optional target playlist ID.", required = false) String playlistId,
            @AgiToolParam(value = "Privacy status (unlisted, public, private).", required = false) String privacyStatus,
            @AgiToolParam(value = "Optional custom target MP4 file path on disk.", required = false) String customTargetFilePath) throws Exception {
        if (GraphicsEnvironment.isHeadless()) {
            throw new AgiToolException("Screen recording is not supported in a headless environment.");
        }

        ScreenRecorder recorder = new ScreenRecorder();
        this.activeRecorder = recorder;
        CompletableFuture<String> uploadFuture = new CompletableFuture<>();

        ScreenRecordingOverlay[] overlayHolder = new ScreenRecordingOverlay[1];
        overlayHolder[0] = new ScreenRecordingOverlay(
                "Recording",
                videoTitle,
                // onStartAction
                () -> {
                    try {
                        int devIdx = overlayHolder[0].getSelectedDeviceIndex();
                        log("Starting screen recording on Screen " + devIdx + " for title: " + videoTitle);
                        if (customTargetFilePath != null && !customTargetFilePath.isBlank()) {
                            recorder.startRecording(Paths.get(customTargetFilePath), devIdx);
                        } else {
                            recorder.startRecording("RECORDING", "desktop", devIdx);
                        }
                    } catch (Exception e) {
                        log.error("Failed to start screen recording", e);
                        error(e);
                    }
                },
                // onSaveLocalAction
                () -> {
                    try {
                        RecordedSession session = recorder.stopRecording(true, null);
                        uploadFuture.complete("Video saved locally to: " + (session != null ? session.videoPath() : "unknown"));
                    } catch (Exception e) {
                        uploadFuture.completeExceptionally(e);
                    }
                },
                // onUploadAction
                () -> {
                    try {
                        RecordedSession session = recorder.stopRecording(true, null);
                        if (session != null && session.videoPath() != null) {
                            YouTubeVideoUploadRequest req = YouTubeVideoUploadRequest.builder()
                                    .videoFilePath(session.videoPath().toString())
                                    .title(videoTitle)
                                    .description("Recorded via Anahata ASI Screen Recorder.\n\nhttps://asi.anahata.uno")
                                    .playlistId(playlistId)
                                    .privacyStatus(privacyStatus != null ? privacyStatus : "unlisted")
                                    .build();
                            String url = uploadVideo(req);
                            if (session.thumbnailPath() != null) {
                                try {
                                    String videoId = url.substring(url.lastIndexOf('/') + 1);
                                    setThumbnail(videoId, session.thumbnailPath().toString());
                                } catch (Exception ignored) {
                                    /* Best effort thumbnail assignment */
                                }
                            }
                            uploadFuture.complete("Video published to YouTube: " + url);
                        } else {
                            uploadFuture.complete("No video recorded.");
                        }
                    } catch (Exception e) {
                        uploadFuture.completeExceptionally(e);
                    }
                },
                // onCancelAction
                () -> {
                    recorder.cancelRecording();
                    uploadFuture.completeExceptionally(new AgiToolException("Recording cancelled by user."));
                }
        );

        overlayHolder[0].showPreLaunch();
        return uploadFuture.get(600, TimeUnit.SECONDS);
    }

    /**
     * Initiates the 1-click interactive browser login using the official Anahata Desktop OAuth client.
     * <p>
     * Launches a local callback listener, opens the user's default browser to Google OAuth consent,
     * exchanges the code for a permanent refresh token, and saves credentials to disk.
     * </p>
     *
     * @return Confirmation message indicating successful authentication.
     * @throws Exception If authorization fails or is cancelled.
     */
    @AgiTool(value = "Launches interactive 1-click browser login to authorize YouTube video uploads with official Anahata credentials. No secrets required.", permission = ToolPermission.APPROVE_ALWAYS)
    public String login() throws Exception {
        return loginInteractive(null, null, null);
    }

    /**
     * Checks if YouTube OAuth2 credentials are configured and authenticated.
     *
     * @return A status message indicating authentication state.
     * @throws Exception If reading credentials fails.
     */
    @AgiTool(value = "Checks if YouTube OAuth2 credentials and refresh tokens are configured.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getAuthStatus() throws Exception {
        if (!YouTubeCredentials.exists()) {
            return "YouTube credentials not configured. Use 'login' to authenticate.";
        }
        YouTubeCredentials creds = YouTubeCredentials.load();
        if (creds.isAuthenticated()) {
            return "YouTube is fully authenticated (Client ID: " + creds.clientId()
                    + ", Playlist ID: " + (creds.playlistId() != null ? creds.playlistId() : "none") + ").";
        }
        return "YouTube credentials exist but lack refresh token. Run 'login' to complete authorization.";
    }

    /**
     * Initiates the interactive browser login flow for YouTube with custom OAuth client credentials.
     *
     * @param clientId The optional Google Cloud OAuth 2.0 Client ID.
     * @param clientSecret The optional Google Cloud OAuth 2.0 Client Secret.
     * @param playlistId The optional default YouTube playlist ID.
     * @return Confirmation message with saved credentials details.
     * @throws Exception If authentication fails or is cancelled.
     */
    @AgiTool(value = "Launches interactive browser login to authorize YouTube video uploads with custom client credentials. Use this if you have your own Google Cloud project.", permission = ToolPermission.APPROVE_ALWAYS)
    public String loginInteractive(
            @AgiToolParam(value = "The optional Google Cloud OAuth 2.0 Client ID (leave empty for default).", required = false) String clientId,
            @AgiToolParam(value = "The optional Google Cloud OAuth 2.0 Client Secret (leave empty for default).", required = false) String clientSecret,
            @AgiToolParam(value = "The optional default YouTube playlist ID.", required = false) String playlistId) throws Exception {
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
    @AgiTool(value = "Uploads a video to YouTube with metadata, tags, and optional playlist assignment.", permission = ToolPermission.APPROVE_ALWAYS)
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
    @AgiTool(value = "Uploads a video to YouTube with discrete parameters.", permission = ToolPermission.APPROVE_ALWAYS)
    public String uploadVideoToPlaylist(
            @AgiToolParam("The absolute path of the video file on disk.") String videoFilePath,
            @AgiToolParam("The title of the video.") String title,
            @AgiToolParam("The description of the video.") String description,
            @AgiToolParam(value = "List of search tags.", required = false) List<String> tags,
            @AgiToolParam(value = "The target playlist ID.", required = false) String playlistId,
            @AgiToolParam(value = "The privacy status (unlisted, public, private).", required = false) String privacyStatus) throws Exception {
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
    @AgiTool(value = "Sets the custom thumbnail image for a YouTube video.", permission = ToolPermission.APPROVE_ALWAYS)
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
    @AgiTool(value = "Adds an uploaded YouTube video to a specified playlist.", permission = ToolPermission.APPROVE_ALWAYS)
    public String addVideoToPlaylist(
            @AgiToolParam("The YouTube video ID.") String videoId,
            @AgiToolParam("The target playlist ID.") String playlistId) throws Exception {
        YouTubeCredentials credentials = YouTubeCredentials.load();
        String accessToken = YouTubeAuthHelper.getValidAccessToken(credentials);
        addVideoToPlaylistInternal(accessToken, videoId, playlistId);
        return "Successfully added video " + videoId + " to playlist " + playlistId;
    }

    /**
     * Retrieves live statistics and engagement metrics (views, likes, comments) for a YouTube video.
     *
     * @param videoId The 11-character YouTube video ID.
     * @return Formatted statistics report.
     * @throws Exception If querying the YouTube API fails.
     */
    @AgiTool(value = "Retrieves live statistics and engagement metrics (views, likes, comments) for a YouTube video.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getVideoStats(
            @AgiToolParam("The 11-character YouTube video ID (e.g. 'dQw4w9WgXcQ').") String videoId) throws Exception {
        YouTubeCredentials credentials = YouTubeCredentials.load();
        String accessToken = YouTubeAuthHelper.getValidAccessToken(credentials);

        String url = "https://www.googleapis.com/youtube/v3/videos?part=snippet,statistics,status&id=" + videoId;
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Authorization", "Bearer " + accessToken)
                .GET()
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
            throw new AgiToolException("Failed to get video stats: HTTP " + response.statusCode() + " - " + response.body());
        }

        JsonNode json = MAPPER.readTree(response.body());
        JsonNode items = json.path("items");
        if (items.isEmpty()) {
            return "No video found with ID: " + videoId;
        }

        JsonNode item = items.get(0);
        JsonNode snippet = item.path("snippet");
        JsonNode stats = item.path("statistics");
        JsonNode status = item.path("status");

        StringBuilder sb = new StringBuilder("📊 **YouTube Video Stats: " + videoId + "**\n");
        sb.append("- **Title**: ").append(snippet.path("title").asText()).append("\n");
        sb.append("- **Channel**: ").append(snippet.path("channelTitle").asText()).append("\n");
        sb.append("- **Published**: ").append(snippet.path("publishedAt").asText()).append("\n");
        sb.append("- **Privacy Status**: ").append(status.path("privacyStatus").asText()).append("\n");
        sb.append("- **👁️ Views**: ").append(stats.path("viewCount").asText("0")).append("\n");
        sb.append("- **👍 Likes**: ").append(stats.path("likeCount").asText("0")).append("\n");
        sb.append("- **💬 Comments**: ").append(stats.path("commentCount").asText("0")).append("\n");
        sb.append("- **URL**: https://youtu.be/").append(videoId);

        return sb.toString();
    }

    /**
     * Lists all YouTube playlists owned by the authenticated channel.
     *
     * @return Formatted list of playlists with IDs, titles, and item counts.
     * @throws Exception If querying playlists fails.
     */
    @AgiTool(value = "Lists all YouTube playlists owned by the authenticated channel.", permission = ToolPermission.APPROVE_ALWAYS)
    public String listPlaylists() throws Exception {
        YouTubeCredentials credentials = YouTubeCredentials.load();
        String accessToken = YouTubeAuthHelper.getValidAccessToken(credentials);

        String url = "https://www.googleapis.com/youtube/v3/playlists?part=snippet,contentDetails&mine=true&maxResults=50";
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Authorization", "Bearer " + accessToken)
                .GET()
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
            throw new AgiToolException("Failed to list playlists: HTTP " + response.statusCode() + " - " + response.body());
        }

        JsonNode json = MAPPER.readTree(response.body());
        JsonNode items = json.path("items");
        if (items.isEmpty()) {
            return "No playlists found for the authenticated channel.";
        }

        StringBuilder sb = new StringBuilder("📋 **Channel Playlists (" + items.size() + ")**\n");
        for (JsonNode item : items) {
            String id = item.path("id").asText();
            String title = item.path("snippet").path("title").asText();
            int itemCount = item.path("contentDetails").path("itemCount").asInt(0);
            sb.append("- **").append(title).append("** (ID: `").append(id).append("`) — ").append(itemCount).append(" videos\n");
        }

        return sb.toString();
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
