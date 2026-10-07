/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.gmail;

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
 * token exchange, and automatic access token refresh for the Gmail API.
 * <p>
 * Implements a local loopback callback server via {@link HttpServer} without external
 * OAuth dependencies.
 * </p>
 *
 * @author anahata
 */
@Slf4j
public class GmailAuthHelper {

    /**
     * Google OAuth 2.0 user authorization endpoint URL.
     */
    private static final String GOOGLE_AUTH_ENDPOINT = "https://accounts.google.com/o/oauth2/v2/auth";

    /**
     * Google OAuth 2.0 token exchange and refresh endpoint URL.
     */
    private static final String GOOGLE_TOKEN_ENDPOINT = "https://oauth2.googleapis.com/token";

    /**
     * Gmail profile endpoint URL to inspect the authorized email address.
     */
    private static final String GMAIL_PROFILE_ENDPOINT = "https://gmail.googleapis.com/gmail/v1/users/me/profile";

    /**
     * Space-delimited OAuth scope required for read-only access to user emails, threads, and attachments.
     */
    public static final String GMAIL_READONLY_SCOPE = "https://www.googleapis.com/auth/gmail.readonly";

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
     * Local redirect path context where Google OAuth sends the authorization code.
     */
    private static final String CALLBACK_PATH = "/oauth2callback";

    /**
     * Shared JSON object mapper for deserializing Google token and profile responses.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Shared HTTP client configured with a 20-second timeout for OAuth operations.
     */
    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_2)
            .connectTimeout(Duration.ofSeconds(20))
            .build();

    /**
     * Cached in-memory bearer access token.
     */
    private static String cachedAccessToken;

    /**
     * Expiration instant of the currently cached access token.
     */
    private static Instant tokenExpiry = Instant.MIN;

    /**
     * Private constructor to prevent instantiation of static utility class.
     */
    private GmailAuthHelper() {
    }

    /**
     * Obtains a valid, unexpired OAuth2 access token for the given credentials.
     * <p>
     * If the cached token is expired or absent, automatically refreshes it
     * against Google's token endpoint using the stored refresh token.
     * </p>
     *
     * @param credentials The loaded {@link GmailCredentials}.
     * @return A valid bearer access token string.
     * @throws IOException If token refresh fails or network errors occur.
     */
    public static synchronized String getValidAccessToken(GmailCredentials credentials) throws IOException {
        if (credentials.refreshToken() == null || credentials.refreshToken().isBlank()) {
            throw new AgiToolException("Gmail is not authenticated (missing refresh token). Run login() first.");
        }

        if (cachedAccessToken != null && Instant.now().plusSeconds(60).isBefore(tokenExpiry)) {
            return cachedAccessToken;
        }

        log.info("Refreshing Gmail OAuth2 access token using stored refresh_token...");
        String formBody = "client_id=" + URLEncoder.encode(credentials.clientId(), StandardCharsets.UTF_8)
                + "&client_secret=" + URLEncoder.encode(credentials.clientSecret(), StandardCharsets.UTF_8)
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
                log.error("Failed to refresh Gmail token: HTTP {} - {}", response.statusCode(), response.body());
                throw new IOException("Failed to refresh Gmail access token: HTTP " + response.statusCode() + " - " + response.body());
            }

            JsonNode json = MAPPER.readTree(response.body());
            cachedAccessToken = json.path("access_token").asText();
            int expiresInSeconds = json.has("expires_in") ? json.path("expires_in").asInt() : 3600;
            tokenExpiry = Instant.now().plusSeconds(expiresInSeconds);

            log.info("Successfully refreshed Gmail access token (expires in {}s)", expiresInSeconds);
            return cachedAccessToken;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("Gmail token refresh interrupted", e);
        }
    }

    /**
     * Executes the interactive browser OAuth2 login flow using provided client credentials.
     * <p>
     * 1. Starts a temporary local HTTP server on port 8888 (or ephemeral fallback).<br>
     * 2. Launches the user's default browser to Google's consent screen.<br>
     * 3. Intercepts the authorization code redirect.<br>
     * 4. Exchanges the code for access and refresh tokens.<br>
     * 5. Queries the authorized account email address.<br>
     * 6. Persists credentials to {@code ~/.anahata/asi/gmail/credentials.json}.
     * </p>
     *
     * @param clientId The Google Cloud OAuth 2.0 Client ID.
     * @param clientSecret The Google Cloud OAuth 2.0 Client Secret.
     * @return The authenticated and persisted {@link GmailCredentials}.
     * @throws Exception If user authorization, redirect capture, or token exchange fails.
     */
    public static GmailCredentials loginInteractive(String clientId, String clientSecret) throws Exception {
        String effectiveClientId = (clientId != null && !clientId.isBlank()) ? clientId.trim() : DEFAULT_CLIENT_ID;
        String effectiveClientSecret = (clientSecret != null && !clientSecret.isBlank()) ? clientSecret.trim() : DEFAULT_CLIENT_SECRET;

        log.info("Initiating interactive Gmail OAuth 2.0 login flow...");
        CompletableFuture<String> authCodeFuture = new CompletableFuture<>();

        HttpServer server;
        int port = PREFERRED_PORT;
        try {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", PREFERRED_PORT), 0);
        } catch (IOException e) {
            log.warn("Preferred port {} busy, allocating ephemeral port...", PREFERRED_PORT);
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            port = server.getAddress().getPort();
        }

        String redirectUri = "http://127.0.0.1:" + port + CALLBACK_PATH;
        server.createContext(CALLBACK_PATH, exchange -> {
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
                            + "<p style='font-size:1.1rem;'>Anahata ASI is now authorized to access your Gmail mailbox.</p>"
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
        });

        server.setExecutor(null);
        server.start();
        log.info("Local OAuth callback listener started on {}", redirectUri);

        try {
            String authUrl = GOOGLE_AUTH_ENDPOINT
                    + "?client_id=" + URLEncoder.encode(effectiveClientId, StandardCharsets.UTF_8)
                    + "&redirect_uri=" + URLEncoder.encode(redirectUri, StandardCharsets.UTF_8)
                    + "&response_type=code"
                    + "&scope=" + URLEncoder.encode(GMAIL_READONLY_SCOPE, StandardCharsets.UTF_8)
                    + "&access_type=offline"
                    + "&prompt=consent";

            log.info("Opening browser to Google OAuth consent screen...");
            if (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
                Desktop.getDesktop().browse(URI.create(authUrl));
            } else {
                new ProcessBuilder("xdg-open", authUrl).start();
            }

            String authCode = authCodeFuture.get(120, TimeUnit.SECONDS);
            log.info("Captured authorization code. Exchanging for tokens...");

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
            String refreshToken = tokenJson.has("refresh_token") ? tokenJson.path("refresh_token").asText() : null;
            cachedAccessToken = tokenJson.path("access_token").asText();
            int expiresInSeconds = tokenJson.has("expires_in") ? tokenJson.path("expires_in").asInt() : 3600;
            tokenExpiry = Instant.now().plusSeconds(expiresInSeconds);

            if (refreshToken == null && GmailCredentials.exists()) {
                GmailCredentials prev = GmailCredentials.load();
                refreshToken = prev.refreshToken();
            }

            String emailAddress = fetchUserEmail(cachedAccessToken);

            GmailCredentials credentials = GmailCredentials.builder()
                    .clientId(effectiveClientId)
                    .clientSecret(effectiveClientSecret)
                    .refreshToken(refreshToken)
                    .emailAddress(emailAddress)
                    .build();

            credentials.save();
            log.info("Gmail credentials saved successfully for {}", emailAddress);
            return credentials;

        } finally {
            server.stop(1);
            log.info("Local OAuth callback listener stopped.");
        }
    }

    /**
     * Queries the user profile to fetch the primary email address.
     *
     * @param accessToken The valid bearer access token.
     * @return The user's Gmail address, or {@code null} if unresolved.
     */
    public static String fetchUserEmail(String accessToken) {
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(GMAIL_PROFILE_ENDPOINT))
                    .header("Authorization", "Bearer " + accessToken)
                    .GET()
                    .build();

            HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() == 200) {
                JsonNode profile = MAPPER.readTree(response.body());
                return profile.path("emailAddress").asText(null);
            }
        } catch (IOException e) {
            log.warn("Failed to retrieve Gmail profile email address: {}", e.getMessage());
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.warn("Interrupted while retrieving Gmail profile: {}", e.getMessage());
        }
        return null;
    }
}
