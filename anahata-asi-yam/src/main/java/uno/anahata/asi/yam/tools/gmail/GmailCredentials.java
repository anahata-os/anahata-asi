/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.gmail;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * Encapsulates Google OAuth 2.0 credentials and metadata for the Gmail API.
 * <p>
 * Manages loading and persisting client ID, client secret, long-lived refresh token,
 * and the authorized user email address in {@code ~/.anahata/asi/gmail/credentials.json}.
 * Also supports importing client credentials from official Google Cloud Console secrets JSON.
 * </p>
 *
 * @param clientId The Google Cloud OAuth 2.0 Client ID.
 * @param clientSecret The Google Cloud OAuth 2.0 Client Secret.
 * @param refreshToken The long-lived refresh token obtained after browser authorization.
 * @param emailAddress The primary Gmail address authorized by this session.
 *
 * @author anahata
 */
@Slf4j
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public record GmailCredentials(
        String clientId,
        String clientSecret,
        String refreshToken,
        String emailAddress
) {

    /**
     * Shared JSON object mapper configured for formatted indentation and lenient property parsing.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

    /**
     * Resolves the canonical credentials file path: {@code ~/.anahata/asi/gmail/credentials.json}.
     *
     * @return The canonical Path to the Gmail credentials file.
     */
    public static Path getCredentialsPath() {
        return AbstractAsiContainer.getWorkDirSubDir("gmail").resolve("credentials.json");
    }

    /**
     * Resolves the active credentials path, checking canonical and fallback locations.
     *
     * @return The Path to an existing credentials file, or canonical path if none exist.
     */
    public static Path resolveExistingCredentialsPath() {
        Path canonical = getCredentialsPath();
        if (Files.exists(canonical)) {
            return canonical;
        }

        String userHome = System.getProperty("user.home");
        List<Path> fallbacks = List.of(
                Paths.get(userHome, ".anahata", "gmail-credentials.json"),
                Paths.get(userHome, ".credentials", "gmail", "credentials.json"),
                Paths.get(userHome, ".credentials", "gmail.json"),
                Paths.get(userHome, ".credentials", "gmail-credentials.json")
        );

        for (Path candidate : fallbacks) {
            if (Files.exists(candidate)) {
                return candidate;
            }
        }

        return canonical;
    }

    /**
     * Checks if a credentials file exists in either canonical or fallback paths.
     *
     * @return {@code true} if credentials exist on disk.
     */
    public static boolean exists() {
        return Files.exists(resolveExistingCredentialsPath());
    }

    /**
     * Loads the stored Gmail credentials from disk.
     *
     * @return The loaded {@link GmailCredentials}.
     * @throws IOException If the credentials file cannot be found or read.
     */
    public static GmailCredentials load() throws IOException {
        Path path = resolveExistingCredentialsPath();
        if (!Files.exists(path)) {
            throw new IOException("Gmail credentials file not found at: " + path);
        }
        byte[] data = Files.readAllBytes(path);
        return MAPPER.readValue(data, GmailCredentials.class);
    }

    /**
     * Parses client ID and client secret from a Google Cloud Console credentials JSON file.
     * <p>
     * Supports standard Google formats including {@code installed} (Desktop app),
     * {@code web} (Web app), or flat JSON representations.
     * </p>
     *
     * @param clientSecretsPath The path to the client secrets JSON file.
     * @return A {@link GmailCredentials} instance containing parsed client secrets.
     * @throws IOException If file reading or JSON parsing fails.
     */
    public static GmailCredentials fromClientSecrets(Path clientSecretsPath) throws IOException {
        if (!Files.exists(clientSecretsPath)) {
            throw new AgiToolException("Client secrets file not found: " + clientSecretsPath);
        }

        JsonNode root = MAPPER.readTree(clientSecretsPath.toFile());
        JsonNode container = root.has("installed") ? root.path("installed")
                : root.has("web") ? root.path("web") : root;

        String clientId = container.path("client_id").asText(null);
        String clientSecret = container.path("client_secret").asText(null);

        if (clientId == null || clientId.isBlank() || clientSecret == null || clientSecret.isBlank()) {
            throw new AgiToolException("Failed to find 'client_id' and 'client_secret' in JSON: " + clientSecretsPath);
        }

        String refreshToken = container.path("refresh_token").asText(null);
        String email = container.path("email_address").asText(null);

        return GmailCredentials.builder()
                .clientId(clientId.trim())
                .clientSecret(clientSecret.trim())
                .refreshToken(refreshToken)
                .emailAddress(email)
                .build();
    }

    /**
     * Persists these credentials to the canonical storage location:
     * {@code ~/.anahata/asi/gmail/credentials.json}.
     *
     * @throws IOException If writing to disk fails.
     */
    public void save() throws IOException {
        Path path = getCredentialsPath();
        Files.createDirectories(path.getParent());
        MAPPER.writeValue(path.toFile(), this);
        log.info("Saved Gmail credentials to {}", path);
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
     * Checks if a refresh token is stored and ready for autonomous API access.
     *
     * @return {@code true} if authenticated with a refresh token.
     */
    @JsonIgnore
    public boolean isAuthenticated() {
        return hasClientSecrets() && refreshToken != null && !refreshToken.isBlank();
    }
}
