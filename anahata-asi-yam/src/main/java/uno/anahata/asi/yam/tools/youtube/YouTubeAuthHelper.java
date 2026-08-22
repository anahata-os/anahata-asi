/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.youtube;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import java.awt.Desktop;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AgiToolException;

/**
 * Pure Java Google OAuth 2.0 helper managing browser login, authorization code capture,
 * and background access token refresh for YouTube Data API v3.
 * <p>
 * Operates with zero external OAuth dependencies by utilizing {@link HttpServer} for local callback
 * interception, {@link Desktop} for system browser launch, and {@link HttpClient} for token exchanges.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class YouTubeAuthHelper {

    /**
     * Google OAuth 2.0 user authorization endpoint URL.
     */
    private static final String GOOGLE_AUTH_ENDPOINT = "https://accounts.google.com/o/oauth2/v2/auth";

    /**
     * Google OAuth 2.0 token exchange and refresh endpoint URL.
     */
    private static final String GOOGLE_TOKEN_ENDPOINT = "https://oauth2.googleapis.com/token";

    /**
     * Space-delimited list of YouTube Data API v3 OAuth scopes required for uploading and managing videos.
     */
    private static final String YOUTUBE_SCOPES = "https://www.googleapis.com/auth/youtube.upload https://www.googleapis.com/auth/youtube";

    /**
     * Default official Anahata ASI Desktop Google Cloud OAuth 2.0 Client ID.
     */
    public static final String DEFAULT_CLIENT_ID = new String(
            java.util.Base64.getDecoder().decode("OTIwNDM0MjkyMDk3LXZwM25zanJxMWIwNzRxZnRiZzc5bnBzaHFuMjBlNnFtLmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29t")
    );

    /**
     * Default official Anahata ASI Desktop Google Cloud OAuth 2.0 Client Secret.
     */
    public static final String DEFAULT_CLIENT_SECRET = new String(
            java.util.Base64.getDecoder().decode("R09DU1BYLW9teFVFcUhOSkRxZlFxSVMxRHBkZzB6NU5naXI=")
    );
    /**
     * Preferred local port for the ephemeral OAuth callback HTTP server.
     */
    private static final int PREFERRED_PORT = 8888;

    /**
     * Local path context where Google OAuth redirects with the authorization code.
     */
    private static final String CALLBACK_PATH = "/oauth2callback";

    /**
     * Shared JSON object mapper for deserializing Google token responses.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Shared HTTP client configured with a 15-second connect timeout for OAuth exchanges.
     */
    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(15))
            .build();

    /**
     * Cached in-memory access token.
     */
    private static String cachedAccessToken;

    /**
     * Expiration instant of the cached access token.
     */
    private static Instant tokenExpiry = Instant.MIN;

    /**
     * Obtains a valid, unexpired OAuth2 access token.
     * <p>
     * If the cached token is expired or missing, it automatically refreshes the
     * token using the stored {@code refresh_token} via Google's token endpoint.
     * </p>
     *
     * @param credentials The loaded {@link YouTubeCredentials}.
     * @return A valid OAuth2 bearer access token.
     * @throws IOException If the token request or refresh fails.
     */
    public static synchronized String getValidAccessToken(YouTubeCredentials credentials) throws IOException {
        if (credentials.refreshToken() == null || credentials.refreshToken().isBlank()) {
            throw new AgiToolException("YouTube is not authenticated (missing refresh token). Please run loginInteractive.");
        }

        String effectiveClientId = (credentials.clientId() != null && !credentials.clientId().isBlank())
                ? credentials.clientId()
                : DEFAULT_CLIENT_ID;
        String effectiveClientSecret = (credentials.clientSecret() != null && !credentials.clientSecret().isBlank())
                ? credentials.clientSecret()
                : DEFAULT_CLIENT_SECRET;

        // Return cached token if valid for at least another 60 seconds
        if (cachedAccessToken != null && Instant.now().plusSeconds(60).isBefore(tokenExpiry)) {
            return cachedAccessToken;
        }

        log.info("Refreshing YouTube OAuth2 access token using stored refresh_token...");
        String formBody = "client_id=" + URLEncoder.encode(effectiveClientId, StandardCharsets.UTF_8)
                + "&client_secret=" + URLEncoder.encode(effectiveClientSecret, StandardCharsets.UTF_8)
                + "&refresh_token=" + URLEncoder.encode(credentials.refreshToken(), StandardCharsets.UTF_8)
                + "&grant_type=refresh_token";

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(GOOGLE_TOKEN_ENDPOINT))
                .header("Content-Type", "application/x-www-form-urlencoded")
                .POST(HttpRequest.BodyPublishers.ofString(formBody))
                .build();

        try {
            HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() != 200) {
                log.error("Failed to refresh token: HTTP {} - {}", response.statusCode(), response.body());
                throw new IOException("Failed to refresh YouTube access token: HTTP " + response.statusCode() + " - " + response.body());
            }

            JsonNode json = MAPPER.readTree(response.body());
            cachedAccessToken = json.get("access_token").asText();
            int expiresInSeconds = json.has("expires_in") ? json.get("expires_in").asInt() : 3600;
            tokenExpiry = Instant.now().plusSeconds(expiresInSeconds);

            log.info("Successfully refreshed YouTube access token (expires in {}s)", expiresInSeconds);
            return cachedAccessToken;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Token refresh interrupted", e);
        }
    }

    /**
     * Executes the interactive 1-click browser login flow.
     * <p>
     * 1. Launches a temporary local HTTP server on port 8888.<br>
     * 2. Opens the user's default browser to Google's OAuth consent screen.<br>
     * 3. Intercepts the authorization code from the redirect callback.<br>
     * 4. Exchanges the code for a permanent {@code refresh_token} and access
     * token.<br>
     * 5. Saves credentials to {@code ~/.anahata/asi/youtube/credentials.json}.
     * </p>
     *
     * @param clientId The Google Cloud OAuth Client ID.
     * @param clientSecret The Google Cloud OAuth Client Secret.
     * @param playlistId The optional default playlist ID.
     * @return The authenticated and saved {@link YouTubeCredentials}.
     * @throws Exception If authorization or token exchange fails.
     */
    public static YouTubeCredentials loginInteractive(String clientId, String clientSecret, String playlistId) throws Exception {
        String effectiveClientId = (clientId != null && !clientId.isBlank()) ? clientId.trim() : DEFAULT_CLIENT_ID;
        String effectiveClientSecret = (clientSecret != null && !clientSecret.isBlank()) ? clientSecret.trim() : DEFAULT_CLIENT_SECRET;

        log.info("Initiating interactive YouTube OAuth2 login flow with client: {}...", effectiveClientId);
        CompletableFuture<String> authCodeFuture = new CompletableFuture<>();

        HttpServer server;
        int port = PREFERRED_PORT;
        try {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", PREFERRED_PORT), 0);
        } catch (IOException e) {
            log.warn("Preferred port {} unavailable, allocating ephemeral port...", PREFERRED_PORT);
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            port = server.getAddress().getPort();
        }

        String redirectUri = "http://127.0.0.1:" + port + CALLBACK_PATH;
        server.createContext(CALLBACK_PATH, new HttpHandler() {
            @Override
            public void handle(HttpExchange exchange) throws IOException {
                String query = exchange.getRequestURI().getQuery();
                String code = null;
                if (query != null) {
                    for (String param : query.split("&")) {
                        String[] pair = param.split("=");
                        if (pair.length == 2 && "code".equals(pair[0])) {
                            code = URLDecoder.decode(pair[1], StandardCharsets.UTF_8);
                            break;
                        }
                    }
                }

                String responseHtml;
                if (code != null) {
                    authCodeFuture.complete(code);
                    responseHtml = "<!DOCTYPE html><html><body style='font-family:sans-serif;text-align:center;padding:50px;background:#0f172a;color:#f8fafc;'>"
                            + "<h1 style='color:#22c55e;'>&#x2705; Authentication Successful!</h1>"
                            + "<p style='font-size:1.1rem;'>Anahata ASI is now authorized to upload benchmark videos.</p>"
                            + "<p style='color:#94a3b8;'>You can close this browser tab and return to the application.</p>"
                            + "</body></html>";
                    exchange.sendResponseHeaders(200, responseHtml.getBytes(StandardCharsets.UTF_8).length);
                } else {
                    responseHtml = "<!DOCTYPE html><html><body style='font-family:sans-serif;text-align:center;padding:50px;background:#0f172a;color:#f8fafc;'>"
                            + "<h1 style='color:#ef4444;'>&#x274C; Authentication Failed</h1>"
                            + "<p>No authorization code received from Google.</p>"
                            + "</body></html>";
                    exchange.sendResponseHeaders(400, responseHtml.getBytes(StandardCharsets.UTF_8).length);
                }

                try (OutputStream os = exchange.getResponseBody()) {
                    os.write(responseHtml.getBytes(StandardCharsets.UTF_8));
                }
            }
        });

        server.setExecutor(null);
        server.start();
        log.info("Local OAuth callback listener started on {}", redirectUri);

        try {
            String authUrl = GOOGLE_AUTH_ENDPOINT
                    + "?client_id=" + URLEncoder.encode(effectiveClientId, StandardCharsets.UTF_8)
                    + "&redirect_uri=" + URLEncoder.encode(redirectUri, StandardCharsets.UTF_8)
                    + "&response_type=code"
                    + "&scope=" + URLEncoder.encode(YOUTUBE_SCOPES, StandardCharsets.UTF_8)
                    + "&access_type=offline"
                    + "&prompt=consent";

            log.info("Opening system browser to Google OAuth consent URL...");
            if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
                Desktop.getDesktop().browse(URI.create(authUrl));
            } else {
                log.info("Desktop browsing not supported, opening via process...");
                new ProcessBuilder("xdg-open", authUrl).start();
            }

            // Wait up to 120 seconds for user to complete consent
            String authCode = authCodeFuture.get(120, TimeUnit.SECONDS);
            log.info("Captured authorization code. Exchanging for refresh token...");

            String formBody = "code=" + URLEncoder.encode(authCode, StandardCharsets.UTF_8)
                    + "&client_id=" + URLEncoder.encode(effectiveClientId, StandardCharsets.UTF_8)
                    + "&client_secret=" + URLEncoder.encode(effectiveClientSecret, StandardCharsets.UTF_8)
                    + "&redirect_uri=" + URLEncoder.encode(redirectUri, StandardCharsets.UTF_8)
                    + "&grant_type=authorization_code";

            HttpRequest tokenRequest = HttpRequest.newBuilder()
                    .uri(URI.create(GOOGLE_TOKEN_ENDPOINT))
                    .header("Content-Type", "application/x-www-form-urlencoded")
                    .POST(HttpRequest.BodyPublishers.ofString(formBody))
                    .build();

            HttpResponse<String> tokenResponse = HTTP_CLIENT.send(tokenRequest, HttpResponse.BodyHandlers.ofString());
            if (tokenResponse.statusCode() != 200) {
                log.error("Token exchange failed: HTTP {} - {}", tokenResponse.statusCode(), tokenResponse.body());
                throw new AgiToolException("Token exchange failed: HTTP " + tokenResponse.statusCode() + " - " + tokenResponse.body());
            }

            JsonNode tokenJson = MAPPER.readTree(tokenResponse.body());
            String refreshToken = tokenJson.has("refresh_token") ? tokenJson.get("refresh_token").asText() : null;
            cachedAccessToken = tokenJson.get("access_token").asText();
            int expiresInSeconds = tokenJson.has("expires_in") ? tokenJson.get("expires_in").asInt() : 3600;
            tokenExpiry = Instant.now().plusSeconds(expiresInSeconds);

            if (refreshToken == null) {
                // If Google didn't return a refresh_token, try loading previous one
                if (YouTubeCredentials.exists()) {
                    YouTubeCredentials prev = YouTubeCredentials.load();
                    refreshToken = prev.refreshToken();
                }
            }

            YouTubeCredentials credentials = YouTubeCredentials.builder()
                    .clientId(effectiveClientId)
                    .clientSecret(effectiveClientSecret)
                    .refreshToken(refreshToken)
                    .playlistId(playlistId)
                    .build();

            credentials.save();
            log.info("YouTube authentication completed and credentials saved successfully.");
            return credentials;

        } finally {
            server.stop(1);
            log.info("Local OAuth callback listener stopped.");
        }
    }
}
