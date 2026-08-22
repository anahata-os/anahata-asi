/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.youtube;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;

/**
 * Encapsulates Google OAuth2 and YouTube Data API v3 credentials stored on disk.
 * <p>
 * Manages reading and writing client ID, client secret, long-lived refresh token,
 * and default playlist ID in {@code ~/.anahata/asi/youtube/credentials.json}.
 * </p>
 *
 * @param clientId The Google Cloud OAuth 2.0 Client ID.
 * @param clientSecret The Google Cloud OAuth 2.0 Client Secret.
 * @param refreshToken The long-lived refresh token obtained after browser authorization.
 * @param playlistId The default YouTube playlist ID (e.g. for Anahata-AGI-1 benchmark runs).
 * 
 * @author anahata
 */
@Slf4j
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public record YouTubeCredentials(
        String clientId,
        String clientSecret,
        String refreshToken,
        String playlistId
) {

    /**
     * Shared JSON object mapper configured for formatted indentation and resilient deserialization.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

    /**
     * Resolves the credentials storage file path: {@code ~/.anahata/asi/youtube/credentials.json}.
     *
     * @return The Path to the credentials file.
     */
    public static Path getCredentialsPath() {
        return AbstractAsiContainer.getWorkDirSubDir("youtube").resolve("credentials.json");
    }

    /**
     * Checks if the credentials file exists on disk.
     *
     * @return {@code true} if the credentials file exists.
     */
    public static boolean exists() {
        return Files.exists(getCredentialsPath());
    }

    /**
     * Loads the stored YouTube credentials from disk.
     *
     * @return The loaded {@link YouTubeCredentials}.
     * @throws IOException If the credentials file is missing or reading/deserialization fails.
     */
    public static YouTubeCredentials load() throws IOException {
        Path path = getCredentialsPath();
        if (!Files.exists(path)) {
            throw new IOException("YouTube credentials file not found at: " + path);
        }
        byte[] data = Files.readAllBytes(path);
        return MAPPER.readValue(data, YouTubeCredentials.class);
    }

    /**
     * Persists the credentials to {@code ~/.anahata/asi/youtube/credentials.json}.
     *
     * @throws IOException If writing to disk fails.
     */
    public void save() throws IOException {
        Path path = getCredentialsPath();
        Files.createDirectories(path.getParent());
        MAPPER.writeValue(path.toFile(), this);
        log.info("Saved YouTube credentials to {}", path);
    }

    /**
     * Checks if client ID and client secret are configured.
     *
     * @return {@code true} if OAuth client details are present.
     */
    @JsonIgnore
    public boolean hasClientSecrets() {
        return clientId != null && !clientId.isBlank() && clientSecret != null && !clientSecret.isBlank();
    }

    /**
     * Checks if a refresh token is stored and ready for autonomous API calls.
     *
     * @return {@code true} if authenticated.
     */
    @JsonIgnore
    public boolean isAuthenticated() {
        return hasClientSecrets() && refreshToken != null && !refreshToken.isBlank();
    }
}
