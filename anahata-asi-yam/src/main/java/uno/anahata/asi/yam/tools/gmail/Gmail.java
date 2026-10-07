/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.yam.tools.gmail;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.ToolPermission;

/**
 * Pure Java Gmail API toolkit providing email search, RFC 822 MIME inspection,
 * chronological thread Markdown export, and batch raw EML and attachment harvesting.
 * <p>
 * Implements direct HTTP integration with the official Google Gmail REST API v1
 * using standard {@link HttpClient} without external Google client libraries.
 * Integrates with {@link GmailAuthHelper} for local loopback OAuth2 authorization
 * and automated token refresh.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Toolkit for email search, inspection, forensic harvesting, and export via Gmail API.")
public class Gmail extends AnahataToolkit {

    /**
     * Gmail REST API base URL for user endpoints.
     */
    private static final String GMAIL_BASE_URL = "https://gmail.googleapis.com/gmail/v1/users/me";

    /**
     * Shared JSON object mapper for response deserialization.
     */
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Shared HTTP client configured with a 30-second timeout and HTTP/2 support.
     */
    private static final HttpClient HTTP_CLIENT = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_2)
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    /**
     * The last search query executed during this session.
     */
    private String lastSearchQuery;

    /**
     * The number of results found in the last search query.
     */
    private int lastSearchResultCount;

    /**
     * Default constructor for the Gmail toolkit.
     */
    public Gmail() {
    }

    /**
     * {@inheritDoc}
     * <p>
     * Initializes the Gmail toolkit.
     * </p>
     */
    @Override
    public void initialize() {
        log.info("Initializing Gmail toolkit: {}", getName());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Injects live Gmail authentication status, authorized account address,
     * total message and thread metrics, and last search telemetry into the RAG message.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        if (!GmailCredentials.exists()) {
            ragMessage.addTextPart("## Gmail Status\n- **Authenticated**: ❌ NO (Not configured)\n- **Action**: Run `Gmail.login(\"/path/to/client_secret.json\")` or `Gmail.loginInteractive(clientId, clientSecret)` to authorize mailbox access.\n");
            return;
        }

        try {
            GmailCredentials credentials = GmailCredentials.load();
            if (!credentials.isAuthenticated()) {
                ragMessage.addTextPart("## Gmail Status\n- **Authenticated**: ❌ NO (Missing refresh token)\n- **Action**: Run `Gmail.login()` to complete browser authorization.\n");
                return;
            }

            String accessToken = GmailAuthHelper.getValidAccessToken(credentials);
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(GMAIL_BASE_URL + "/profile"))
                    .header("Authorization", "Bearer " + accessToken)
                    .GET()
                    .build();

            HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() == 200) {
                JsonNode profile = MAPPER.readTree(response.body());
                String email = profile.path("emailAddress").asText(credentials.emailAddress());
                long messagesTotal = profile.path("messagesTotal").asLong(0);
                long threadsTotal = profile.path("threadsTotal").asLong(0);

                StringBuilder sb = new StringBuilder("## Gmail Telemetry\n");
                sb.append("- **Account**: `").append(email).append("`\n");
                sb.append("- **Authenticated**: ✅ YES\n");
                sb.append("- **Total Messages**: ").append(messagesTotal).append(" | **Total Threads**: ").append(threadsTotal).append("\n");

                if (lastSearchQuery != null && !lastSearchQuery.isBlank()) {
                    sb.append("- **Last Search Query**: `").append(lastSearchQuery).append("` (")
                            .append(lastSearchResultCount).append(" results)\n");
                }
                ragMessage.addTextPart(sb.toString());
            } else {
                ragMessage.addTextPart("## Gmail Status\n- **Account**: `" + credentials.emailAddress() + "`\n- **Authenticated**: ✅ YES\n");
            }

        } catch (IOException e) {
            log.error("Error populating Gmail RAG message telemetry", e);
            ragMessage.addTextPart("## Gmail Status\n- ⚠️ Telemetry fetch error: " + e.getMessage() + "\n");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.error("Interrupted while populating Gmail RAG message telemetry", e);
            ragMessage.addTextPart("## Gmail Status\n- ⚠️ Telemetry fetch interrupted\n");
        }
    }

    /**
     * Authorizes the Gmail session using official Google Client Secrets JSON or stored credentials.
     * <p>
     * If a path is provided, parses {@code client_id} and {@code client_secret} from the file
     * and initiates the browser OAuth2 consent flow. If no path is provided, attempts to load
     * credentials from {@code ~/.anahata/asi/gmail/credentials.json}.
     * </p>
     *
     * @param credentialsJsonPath The optional path to the Google Cloud OAuth 2.0 client_secret.json file.
     * @return Confirmation message indicating successful authorization.
     * @throws Exception If file reading, browser launch, or authorization fails.
     */
    @AgiTool(value = "Authorizes the Gmail session using official Google Client Secrets JSON or stored credentials.", permission = ToolPermission.APPROVE_ALWAYS)
    public String login(
            @AgiToolParam(value = "Optional path to Google Cloud OAuth 2.0 client_secret.json (leave empty to use stored credentials).", required = false) String credentialsJsonPath) throws Exception {
        if (credentialsJsonPath != null && !credentialsJsonPath.isBlank()) {
            Path path = Paths.get(credentialsJsonPath.trim());
            log("Importing Google Cloud credentials from: " + path.toAbsolutePath());
            GmailCredentials parsed = GmailCredentials.fromClientSecrets(path);

            if (parsed.isAuthenticated()) {
                String token = GmailAuthHelper.getValidAccessToken(parsed);
                String email = GmailAuthHelper.fetchUserEmail(token);
                GmailCredentials withEmail = GmailCredentials.builder()
                        .clientId(parsed.clientId())
                        .clientSecret(parsed.clientSecret())
                        .refreshToken(parsed.refreshToken())
                        .emailAddress(email != null ? email : parsed.emailAddress())
                        .build();
                withEmail.save();
                log("Saved credentials with existing refresh token for: " + withEmail.emailAddress());
                return "Successfully authenticated Gmail for account: " + withEmail.emailAddress();
            }

            log("Client secrets parsed. Launching interactive browser login...");
            GmailCredentials authorized = GmailAuthHelper.loginInteractive(parsed.clientId(), parsed.clientSecret());
            return "Successfully authorized Gmail for account: " + authorized.emailAddress();
        }

        if (GmailCredentials.exists()) {
            GmailCredentials existing = GmailCredentials.load();
            if (existing.isAuthenticated()) {
                String token = GmailAuthHelper.getValidAccessToken(existing);
                String email = GmailAuthHelper.fetchUserEmail(token);
                return "Already authenticated to Gmail as: " + (email != null ? email : existing.emailAddress());
            }

            if (existing.hasClientSecrets()) {
                log("Found stored client secrets. Launching interactive browser login...");
                GmailCredentials authorized = GmailAuthHelper.loginInteractive(existing.clientId(), existing.clientSecret());
                return "Successfully authorized Gmail for account: " + authorized.emailAddress();
            }
        }

        log("Using official Anahata Desktop client credentials. Launching interactive browser login...");
        GmailCredentials authorized = GmailAuthHelper.loginInteractive(null, null);
        return "Successfully authorized Gmail for account: " + authorized.emailAddress();
    }

    /**
     * Initiates the 1-click interactive browser login using the official Anahata Desktop OAuth client.
     *
     * @return Confirmation message indicating successful authentication.
     * @throws Exception If authorization fails or is cancelled.
     */
    @AgiTool(value = "Launches 1-click browser login to authorize Gmail using official Anahata Desktop credentials. No secrets JSON required.", permission = ToolPermission.APPROVE_ALWAYS)
    public String login() throws Exception {
        return login(null);
    }

    /**
     * Launches interactive browser login to authorize Gmail access with custom client credentials.
     *
     * @param clientId The Google Cloud OAuth 2.0 Client ID.
     * @param clientSecret The Google Cloud OAuth 2.0 Client Secret.
     * @return Confirmation message indicating successful authorization.
     * @throws Exception If user authorization or token exchange fails.
     */
    @AgiTool(value = "Launches interactive browser login to authorize Gmail access with custom client credentials.", permission = ToolPermission.APPROVE_ALWAYS)
    public String loginInteractive(
            @AgiToolParam("Google Cloud OAuth 2.0 Client ID.") String clientId,
            @AgiToolParam("Google Cloud OAuth 2.0 Client Secret.") String clientSecret) throws Exception {
        log("Initiating interactive Gmail OAuth2 login flow...");
        GmailCredentials credentials = GmailAuthHelper.loginInteractive(clientId, clientSecret);
        return "Successfully authorized Gmail for account: " + credentials.emailAddress()
                + ". Credentials saved to ~/.anahata/asi/gmail/credentials.json";
    }

    /**
     * Checks if Gmail OAuth2 credentials and refresh tokens are configured.
     *
     * @return Status report indicating whether Gmail is authenticated and ready.
     * @throws Exception If reading credentials fails.
     */
    @AgiTool(value = "Checks if Gmail OAuth2 credentials and refresh tokens are configured.", permission = ToolPermission.APPROVE_ALWAYS)
    public String getAuthStatus() throws Exception {
        if (!GmailCredentials.exists()) {
            return "Gmail credentials not configured. Run login() or loginInteractive() to authenticate.";
        }
        GmailCredentials credentials = GmailCredentials.load();
        if (credentials.isAuthenticated()) {
            return "Gmail is fully authenticated for account: " + credentials.emailAddress()
                    + " (Client ID: " + credentials.clientId() + ").";
        }
        return "Gmail credentials exist but lack refresh token. Run login() to complete browser authorization.";
    }

    /**
     * Executes standard Gmail search queries and returns a structured list of email headers.
     *
     * @param query Standard Gmail search query (e.g. {@code from:example.com}, {@code subject:invoice}, {@code has:attachment}).
     * @param maxResults Maximum number of messages to return (defaults to 50, maximum 500).
     * @return A list of {@link GmailMessageSummary} objects matching the query.
     * @throws Exception If the search or API call fails.
     */
    @AgiTool(value = "Executes standard Gmail search queries and returns a structured list of email headers.", permission = ToolPermission.APPROVE_ALWAYS)
    public List<GmailMessageSummary> searchEmails(
            @AgiToolParam("Standard Gmail search query (e.g. 'from:example.com', 'subject:invoice', 'has:attachment').") String query,
            @AgiToolParam(value = "Maximum number of messages to return (defaults to 50, max 500).", required = false) Integer maxResults) throws Exception {
        if (query == null || query.isBlank()) {
            throw new AgiToolException("Search query cannot be empty.");
        }

        int limit = (maxResults != null && maxResults > 0) ? Math.min(maxResults, 500) : 50;
        GmailCredentials credentials = GmailCredentials.load();
        String accessToken = GmailAuthHelper.getValidAccessToken(credentials);

        log("Executing Gmail search query: \"" + query + "\" (maxResults: " + limit + ")...");
        String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8);

        String listUrl = GMAIL_BASE_URL + "/messages?q=" + encodedQuery + "&maxResults=" + limit;
        HttpRequest listReq = HttpRequest.newBuilder()
                .uri(URI.create(listUrl))
                .header("Authorization", "Bearer " + accessToken)
                .GET()
                .build();

        HttpResponse<String> listRes = HTTP_CLIENT.send(listReq, HttpResponse.BodyHandlers.ofString());
        if (listRes.statusCode() != 200) {
            throw new AgiToolException("Gmail search failed: HTTP " + listRes.statusCode() + " - " + listRes.body());
        }

        JsonNode listJson = MAPPER.readTree(listRes.body());
        JsonNode messagesNode = listJson.path("messages");

        if (messagesNode.isEmpty()) {
            this.lastSearchQuery = query;
            this.lastSearchResultCount = 0;
            log("No messages matched the search query.");
            return Collections.emptyList();
        }

        List<GmailMessageSummary> results = new ArrayList<>();
        for (JsonNode item : messagesNode) {
            String messageId = item.path("id").asText();
            try {
                String metaUrl = GMAIL_BASE_URL + "/messages/" + messageId
                        + "?format=metadata&metadataHeaders=Date&metadataHeaders=From&metadataHeaders=To&metadataHeaders=Subject";
                HttpRequest metaReq = HttpRequest.newBuilder()
                        .uri(URI.create(metaUrl))
                        .header("Authorization", "Bearer " + accessToken)
                        .GET()
                        .build();

                HttpResponse<String> metaRes = HTTP_CLIENT.send(metaReq, HttpResponse.BodyHandlers.ofString());
                if (metaRes.statusCode() == 200) {
                    JsonNode msgJson = MAPPER.readTree(metaRes.body());
                    JsonNode headers = msgJson.path("payload").path("headers");

                    boolean hasAttachments = false;
                    for (JsonNode part : msgJson.path("payload").path("parts")) {
                        String filename = part.path("filename").asText("");
                        if (!filename.isBlank()) {
                            hasAttachments = true;
                            break;
                        }
                    }

                    results.add(GmailMessageSummary.builder()
                            .messageId(messageId)
                            .threadId(msgJson.path("threadId").asText())
                            .date(extractHeader(headers, "Date"))
                            .from(extractHeader(headers, "From"))
                            .to(extractHeader(headers, "To"))
                            .subject(extractHeader(headers, "Subject"))
                            .hasAttachments(hasAttachments)
                            .snippet(msgJson.path("snippet").asText(""))
                            .build());
                }
            } catch (IOException e) {
                log.warn("Failed to retrieve metadata for message {}: {}", messageId, e.getMessage());
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.warn("Interrupted while retrieving metadata for message {}: {}", messageId, e.getMessage());
            }
        }

        this.lastSearchQuery = query;
        this.lastSearchResultCount = results.size();
        log("Retrieved " + results.size() + " messages for query: \"" + query + "\"");
        return results;
    }

    /**
     * Retrieves full RFC 822 email details: headers, plain and HTML bodies, and attachment descriptors.
     *
     * @param messageId The unique Gmail message ID.
     * @return The complete {@link GmailMessageDetails} object.
     * @throws Exception If message retrieval or payload decoding fails.
     */
    @AgiTool(value = "Retrieves full RFC 822 email details: headers, plain and HTML bodies, and attachment descriptors.", permission = ToolPermission.APPROVE_ALWAYS)
    public GmailMessageDetails getEmailDetails(
            @AgiToolParam("The unique Gmail message ID (e.g. '18f1234abcd').") String messageId) throws Exception {
        if (messageId == null || messageId.isBlank()) {
            throw new AgiToolException("Message ID cannot be empty.");
        }

        GmailCredentials credentials = GmailCredentials.load();
        String accessToken = GmailAuthHelper.getValidAccessToken(credentials);

        String url = GMAIL_BASE_URL + "/messages/" + messageId.trim() + "?format=full";
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Authorization", "Bearer " + accessToken)
                .GET()
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
            throw new AgiToolException("Failed to get email details: HTTP " + response.statusCode() + " - " + response.body());
        }

        JsonNode json = MAPPER.readTree(response.body());
        JsonNode payload = json.path("payload");
        JsonNode headers = payload.path("headers");

        StringBuilder plainBuilder = new StringBuilder();
        StringBuilder htmlBuilder = new StringBuilder();
        List<GmailAttachmentInfo> attachments = new ArrayList<>();

        extractPayloadData(payload, plainBuilder, htmlBuilder, attachments);

        List<String> labels = new ArrayList<>();
        for (JsonNode label : json.path("labelIds")) {
            labels.add(label.asText());
        }

        String plainText = plainBuilder.toString().trim();
        String htmlText = htmlBuilder.toString().trim();

        if (plainText.isEmpty() && !htmlText.isEmpty()) {
            plainText = sanitizeHtml(htmlText);
        }

        return GmailMessageDetails.builder()
                .messageId(messageId)
                .threadId(json.path("threadId").asText())
                .date(extractHeader(headers, "Date"))
                .from(extractHeader(headers, "From"))
                .to(extractHeader(headers, "To"))
                .cc(extractHeader(headers, "Cc"))
                .bcc(extractHeader(headers, "Bcc"))
                .subject(extractHeader(headers, "Subject"))
                .snippet(json.path("snippet").asText(""))
                .bodyPlain(plainText)
                .bodyHtml(htmlText)
                .hasAttachments(!attachments.isEmpty())
                .attachments(attachments)
                .labels(labels)
                .build();
    }

    /**
     * Downloads an individual email attachment to disk on demand.
     * <p>
     * Allows surgical, on-the-fly extraction of a specific attachment while inspecting an email
     * without having to perform a full bulk export.
     * </p>
     *
     * @param messageId The unique Gmail message ID containing the attachment.
     * @param attachmentId The attachment ID obtained via {@link #getEmailDetails(String)}.
     * @param destinationPath The destination file path on the local filesystem.
     * @return Confirmation message with the saved file path and size.
     * @throws Exception If downloading, decoding, or file creation fails.
     */
    @AgiTool(value = "Downloads an individual email attachment to disk on demand.", permission = ToolPermission.APPROVE_ALWAYS)
    public String downloadAttachment(
            @AgiToolParam("The unique Gmail message ID containing the attachment.") String messageId,
            @AgiToolParam("The attachment ID obtained via getEmailDetails.") String attachmentId,
            @AgiToolParam(value = "The destination file path on the local filesystem.", rendererId = "path") String destinationPath) throws Exception {
        if (messageId == null || messageId.isBlank()) {
            throw new AgiToolException("Message ID cannot be empty.");
        }
        if (attachmentId == null || attachmentId.isBlank()) {
            throw new AgiToolException("Attachment ID cannot be empty.");
        }
        if (destinationPath == null || destinationPath.isBlank()) {
            throw new AgiToolException("Destination path cannot be empty.");
        }

        Path targetFile = Paths.get(destinationPath.trim());
        if (Files.isDirectory(targetFile)) {
            throw new AgiToolException("Destination path must be a target file path, not an existing directory: " + targetFile.toAbsolutePath());
        }

        if (targetFile.getParent() != null) {
            Files.createDirectories(targetFile.getParent());
        }

        GmailCredentials credentials = GmailCredentials.load();
        String accessToken = GmailAuthHelper.getValidAccessToken(credentials);

        log("Downloading attachment " + attachmentId + " for message " + messageId + " to: " + targetFile.toAbsolutePath());
        downloadAttachmentBinary(accessToken, messageId.trim(), attachmentId.trim(), targetFile);

        long size = Files.size(targetFile);
        log("Downloaded attachment (" + formatFileSize(size) + ") successfully to: " + targetFile.toAbsolutePath());
        return "Attachment downloaded successfully (" + formatFileSize(size) + ") to: " + targetFile.toAbsolutePath();
    }

    /**
     * Dumps an entire email thread chronologically into a single, structured Markdown file.
     *
     * @param threadId The unique Gmail thread ID.
     * @param targetDirectory The destination directory folder path on the host filesystem.
     * @return The absolute path to the generated Markdown file.
     * @throws Exception If thread fetching or file writing fails.
     */
    @AgiTool(value = "Dumps an entire email thread chronologically into a single, structured Markdown file.", permission = ToolPermission.APPROVE_ALWAYS)
    public String exportThreadToMarkdown(
            @AgiToolParam("The unique Gmail thread ID.") String threadId,
            @AgiToolParam(value = "The destination directory folder path.", rendererId = "path") String targetDirectory) throws Exception {
        if (threadId == null || threadId.isBlank()) {
            throw new AgiToolException("Thread ID cannot be empty.");
        }
        if (targetDirectory == null || targetDirectory.isBlank()) {
            throw new AgiToolException("Target directory cannot be empty.");
        }

        Path targetDir = Paths.get(targetDirectory.trim());
        Files.createDirectories(targetDir);

        GmailCredentials credentials = GmailCredentials.load();
        String accessToken = GmailAuthHelper.getValidAccessToken(credentials);

        String url = GMAIL_BASE_URL + "/threads/" + threadId.trim() + "?format=full";
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Authorization", "Bearer " + accessToken)
                .GET()
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
            throw new AgiToolException("Failed to retrieve thread: HTTP " + response.statusCode() + " - " + response.body());
        }

        JsonNode threadJson = MAPPER.readTree(response.body());
        JsonNode messagesNode = threadJson.path("messages");

        if (messagesNode.isEmpty()) {
            throw new AgiToolException("Thread " + threadId + " contains no messages.");
        }

        String threadSubject = "";
        Set<String> participants = new LinkedHashSet<>();
        List<GmailMessageDetails> detailsList = new ArrayList<>();

        for (JsonNode msgNode : messagesNode) {
            String msgId = msgNode.path("id").asText();
            JsonNode payload = msgNode.path("payload");
            JsonNode headers = payload.path("headers");

            String from = extractHeader(headers, "From");
            String to = extractHeader(headers, "To");
            String subject = extractHeader(headers, "Subject");
            if (threadSubject.isEmpty() && !subject.isEmpty()) {
                threadSubject = subject;
            }
            if (!from.isEmpty()) {
                participants.add(from);
            }
            if (!to.isEmpty()) {
                participants.add(to);
            }

            StringBuilder plainBuilder = new StringBuilder();
            StringBuilder htmlBuilder = new StringBuilder();
            List<GmailAttachmentInfo> attachments = new ArrayList<>();
            extractPayloadData(payload, plainBuilder, htmlBuilder, attachments);

            String plainText = plainBuilder.toString().trim();
            if (plainText.isEmpty() && !htmlBuilder.isEmpty()) {
                plainText = sanitizeHtml(htmlBuilder.toString());
            }

            detailsList.add(GmailMessageDetails.builder()
                    .messageId(msgId)
                    .threadId(threadId)
                    .date(extractHeader(headers, "Date"))
                    .from(from)
                    .to(to)
                    .cc(extractHeader(headers, "Cc"))
                    .bcc(extractHeader(headers, "Bcc"))
                    .subject(subject)
                    .snippet(msgNode.path("snippet").asText(""))
                    .bodyPlain(plainText)
                    .hasAttachments(!attachments.isEmpty())
                    .attachments(attachments)
                    .build());
        }

        StringBuilder md = new StringBuilder();
        md.append("# Email Thread: ").append(threadSubject.isEmpty() ? "No Subject" : threadSubject).append("\n\n");
        md.append("- **Thread ID**: `").append(threadId).append("`\n");
        md.append("- **Total Messages**: ").append(detailsList.size()).append("\n");
        md.append("- **Participants**: ").append(String.join("; ", participants)).append("\n");
        md.append("- **Exported Timestamp**: ").append(Instant.now()).append("\n\n");
        md.append("---\n\n");

        for (int i = 0; i < detailsList.size(); i++) {
            GmailMessageDetails d = detailsList.get(i);
            md.append("## Message ").append(i + 1).append(": ").append(d.subject()).append("\n\n");
            md.append("- **Date**: ").append(d.date()).append("\n");
            md.append("- **From**: ").append(d.from()).append("\n");
            md.append("- **To**: ").append(d.to()).append("\n");
            if (d.cc() != null && !d.cc().isBlank()) {
                md.append("- **Cc**: ").append(d.cc()).append("\n");
            }
            md.append("- **Message ID**: `").append(d.messageId()).append("`\n");

            if (d.hasAttachments()) {
                md.append("- **Attachments**: ");
                List<String> attStrs = new ArrayList<>();
                for (GmailAttachmentInfo att : d.attachments()) {
                    attStrs.add("`" + att.filename() + "` (" + formatFileSize(att.size()) + ")");
                }
                md.append(String.join(", ", attStrs)).append("\n");
            } else {
                md.append("- **Attachments**: None\n");
            }
            md.append("\n### Content\n\n");
            md.append(d.bodyPlain().isEmpty() ? "*(No plain text body content)*" : d.bodyPlain()).append("\n\n");
            md.append("---\n\n");
        }

        Path outputPath = targetDir.resolve("thread_" + threadId + ".md");
        Files.writeString(outputPath, md.toString(), StandardCharsets.UTF_8);
        log.info("Exported thread {} to {}", threadId, outputPath.toAbsolutePath());
        return outputPath.toAbsolutePath().toString();
    }

    /**
     * Executes a search query, downloads all matching emails as .eml and .md, optionally downloads attachments,
     * and generates an INDEX.md catalog.
     *
     * @param query Standard Gmail search query (e.g. {@code after:2024/01/01 to:me}).
     * @param targetDirectory The destination directory folder path for harvested files.
     * @param downloadAttachments Whether to download binary file attachments into an 'attachments' subfolder.
     * @return Comprehensive summary report of harvested artifacts.
     * @throws Exception If network errors, API failures, or disk write issues occur.
     */
    @AgiTool(value = "Executes a search query, downloads all matching emails as .eml and .md, optionally downloads attachments, and generates an INDEX.md catalog.", permission = ToolPermission.APPROVE_ALWAYS)
    public String bulkExportQuery(
            @AgiToolParam("Standard Gmail search query (e.g. 'after:2024/01/01 to:me').") String query,
            @AgiToolParam(value = "The destination directory folder path for harvested files.", rendererId = "path") String targetDirectory,
            @AgiToolParam(value = "Whether to download binary file attachments into an 'attachments' subfolder.", required = false) Boolean downloadAttachments) throws Exception {
        if (query == null || query.isBlank()) {
            throw new AgiToolException("Search query cannot be empty.");
        }
        if (targetDirectory == null || targetDirectory.isBlank()) {
            throw new AgiToolException("Target directory cannot be empty.");
        }

        boolean withAttachments = Boolean.TRUE.equals(downloadAttachments);
        Path targetDir = Paths.get(targetDirectory.trim());
        Path emlDir = targetDir.resolve("eml");
        Path mdDir = targetDir.resolve("md");
        Path attDir = targetDir.resolve("attachments");

        Files.createDirectories(emlDir);
        Files.createDirectories(mdDir);
        if (withAttachments) {
            Files.createDirectories(attDir);
        }

        GmailCredentials credentials = GmailCredentials.load();
        String accessToken = GmailAuthHelper.getValidAccessToken(credentials);

        log("Executing bulk email harvest for query: \"" + query + "\"...");
        List<String> messageIds = new ArrayList<>();
        String pageToken = null;

        do {
            StringBuilder urlBuilder = new StringBuilder(GMAIL_BASE_URL + "/messages?q=" + URLEncoder.encode(query, StandardCharsets.UTF_8) + "&maxResults=100");
            if (pageToken != null && !pageToken.isBlank()) {
                urlBuilder.append("&pageToken=").append(pageToken);
            }

            HttpRequest listReq = HttpRequest.newBuilder()
                    .uri(URI.create(urlBuilder.toString()))
                    .header("Authorization", "Bearer " + accessToken)
                    .GET()
                    .build();

            HttpResponse<String> listRes = HTTP_CLIENT.send(listReq, HttpResponse.BodyHandlers.ofString());
            if (listRes.statusCode() != 200) {
                throw new AgiToolException("Failed to list messages: HTTP " + listRes.statusCode() + " - " + listRes.body());
            }

            JsonNode listJson = MAPPER.readTree(listRes.body());
            for (JsonNode item : listJson.path("messages")) {
                messageIds.add(item.path("id").asText());
            }
            pageToken = listJson.hasNonNull("nextPageToken") ? listJson.path("nextPageToken").asText() : null;

        } while (pageToken != null && !pageToken.isBlank() && messageIds.size() < 1000);

        if (messageIds.isEmpty()) {
            return "No emails found matching query: \"" + query + "\"";
        }

        log("Found " + messageIds.size() + " messages. Downloading EML, generating Markdown, and harvesting attachments...");

        StringBuilder indexMd = new StringBuilder();
        indexMd.append("# Forensic Email Catalog\n\n");
        indexMd.append("- **Search Query**: `").append(query).append("`\n");
        indexMd.append("- **Total Messages Harvested**: ").append(messageIds.size()).append("\n");
        indexMd.append("- **Export Timestamp**: ").append(Instant.now()).append("\n");
        indexMd.append("- **Attachments Harvested**: ").append(withAttachments ? "Yes" : "No").append("\n\n");
        indexMd.append("| # | Date | From | To | Subject | Attachments | Details | Raw EML |\n");
        indexMd.append("|---|---|---|---|---|---|---|---|\n");

        int processedCount = 0;
        int totalAttachmentsDownloaded = 0;

        for (int i = 0; i < messageIds.size(); i++) {
            String msgId = messageIds.get(i);
            try {
                // 1. Fetch raw RFC 822 for EML preservation
                String rawUrl = GMAIL_BASE_URL + "/messages/" + msgId + "?format=raw";
                HttpRequest rawReq = HttpRequest.newBuilder().uri(URI.create(rawUrl)).header("Authorization", "Bearer " + accessToken).GET().build();
                HttpResponse<String> rawRes = HTTP_CLIENT.send(rawReq, HttpResponse.BodyHandlers.ofString());

                if (rawRes.statusCode() == 200) {
                    JsonNode rawJson = MAPPER.readTree(rawRes.body());
                    String rawBase64 = rawJson.path("raw").asText("").replaceAll("\\s+", "");
                    byte[] emlBytes = Base64.getUrlDecoder().decode(rawBase64);
                    Files.write(emlDir.resolve(msgId + ".eml"), emlBytes);
                }

                // 2. Fetch full details for Markdown and attachments
                GmailMessageDetails details = getEmailDetails(msgId);

                // 3. Save single message markdown
                StringBuilder msgMd = new StringBuilder();
                msgMd.append("# ").append(details.subject().isEmpty() ? "No Subject" : details.subject()).append("\n\n");
                msgMd.append("- **Message ID**: `").append(details.messageId()).append("`\n");
                msgMd.append("- **Thread ID**: `").append(details.threadId()).append("`\n");
                msgMd.append("- **Date**: ").append(details.date()).append("\n");
                msgMd.append("- **From**: ").append(details.from()).append("\n");
                msgMd.append("- **To**: ").append(details.to()).append("\n");
                if (details.cc() != null && !details.cc().isBlank()) {
                    msgMd.append("- **Cc**: ").append(details.cc()).append("\n");
                }
                msgMd.append("- **Raw EML**: [Download](../eml/").append(msgId).append(".eml)\n\n");

                List<String> attachmentLinks = new ArrayList<>();
                if (details.hasAttachments()) {
                    msgMd.append("### Attachments\n");
                    for (GmailAttachmentInfo att : details.attachments()) {
                        String safeName = sanitizeFilename(att.filename());
                        String attFile = msgId + "_" + safeName;
                        if (withAttachments && att.attachmentId() != null) {
                            try {
                                downloadAttachmentBinary(accessToken, msgId, att.attachmentId(), attDir.resolve(attFile));
                                totalAttachmentsDownloaded++;
                            } catch (Exception e) {
                                log.warn("Failed to download attachment {} for message {}: {}", att.filename(), msgId, e.getMessage());
                            }
                        }
                        String link = "[" + att.filename() + "](../attachments/" + attFile + ") (" + formatFileSize(att.size()) + ")";
                        attachmentLinks.add(link);
                        msgMd.append("- ").append(link).append("\n");
                    }
                    msgMd.append("\n");
                }

                msgMd.append("### Content\n\n");
                msgMd.append(details.bodyPlain().isEmpty() ? "*(No plain text body content)*" : details.bodyPlain()).append("\n");
                Files.writeString(mdDir.resolve(msgId + ".md"), msgMd.toString(), StandardCharsets.UTF_8);

                // 4. Append to index table
                String attCol = attachmentLinks.isEmpty() ? "None" : String.join("<br>", attachmentLinks);
                String safeSubj = details.subject().replace("|", "\\|");
                String safeFrom = details.from().replace("|", "\\|");
                String safeTo = details.to().replace("|", "\\|");

                indexMd.append("| ").append(i + 1)
                        .append(" | ").append(details.date())
                        .append(" | ").append(safeFrom)
                        .append(" | ").append(safeTo)
                        .append(" | ").append(safeSubj)
                        .append(" | ").append(attCol)
                        .append(" | [View](md/").append(msgId).append(".md)")
                        .append(" | [EML](eml/").append(msgId).append(".eml)")
                        .append(" |\n");

                processedCount++;

            } catch (Exception e) {
                log.error("Failed to harvest message {}: {}", msgId, e.getMessage());
            }
        }

        Path indexPath = targetDir.resolve("INDEX.md");
        Files.writeString(indexPath, indexMd.toString(), StandardCharsets.UTF_8);
        log.info("Bulk harvest completed. Processed {} messages, index written to {}", processedCount, indexPath.toAbsolutePath());

        StringBuilder report = new StringBuilder("### Bulk Email Harvest Complete\n");
        report.append("- **Query**: `").append(query).append("`\n");
        report.append("- **Messages Processed**: ").append(processedCount).append(" of ").append(messageIds.size()).append("\n");
        report.append("- **Attachments Downloaded**: ").append(totalAttachmentsDownloaded).append("\n");
        report.append("- **Output Directory**: `").append(targetDir.toAbsolutePath()).append("`\n");
        report.append("- **Index Catalog**: `").append(indexPath.toAbsolutePath()).append("`\n");
        report.append("- **EML Storage**: `").append(emlDir.toAbsolutePath()).append("`\n");
        report.append("- **Markdown Storage**: `").append(mdDir.toAbsolutePath()).append("`\n");
        return report.toString();
    }

    /**
     * Downloads an individual attachment binary from Gmail and writes it to disk.
     *
     * @param accessToken The OAuth2 access token.
     * @param messageId The message ID.
     * @param attachmentId The attachment ID.
     * @param destination The file destination path.
     * @throws Exception If downloading or decoding fails.
     */
    private void downloadAttachmentBinary(String accessToken, String messageId, String attachmentId, Path destination) throws Exception {
        String url = GMAIL_BASE_URL + "/messages/" + messageId + "/attachments/" + attachmentId;
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Authorization", "Bearer " + accessToken)
                .GET()
                .build();

        HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() == 200) {
            JsonNode json = MAPPER.readTree(response.body());
            String data = json.path("data").asText("").replaceAll("\\s+", "");
            byte[] bytes = Base64.getUrlDecoder().decode(data);
            Files.write(destination, bytes);
        } else {
            throw new IOException("HTTP " + response.statusCode() + " when downloading attachment: " + response.body());
        }
    }

    /**
     * Recursively traverses MIME payload trees to extract text bodies and attachment metadata.
     *
     * @param part The current JSON payload part node.
     * @param plainBuilder Accumulator for plain text body.
     * @param htmlBuilder Accumulator for HTML body.
     * @param attachments Accumulator for attachment metadata.
     */
    private static void extractPayloadData(JsonNode part, StringBuilder plainBuilder, StringBuilder htmlBuilder, List<GmailAttachmentInfo> attachments) {
        if (part == null) {
            return;
        }

        String mimeType = part.path("mimeType").asText("");
        String filename = part.path("filename").asText("");
        JsonNode body = part.path("body");
        String attachmentId = body.path("attachmentId").asText(null);

        if (filename != null && !filename.isBlank() && attachmentId != null) {
            attachments.add(GmailAttachmentInfo.builder()
                    .attachmentId(attachmentId)
                    .filename(filename)
                    .mimeType(mimeType)
                    .size(body.path("size").asLong(0))
                    .build());
        }

        if (mimeType.equalsIgnoreCase("text/plain") && body.hasNonNull("data")) {
            String rawData = body.path("data").asText().replaceAll("\\s+", "");
            try {
                byte[] decoded = Base64.getUrlDecoder().decode(rawData);
                plainBuilder.append(new String(decoded, StandardCharsets.UTF_8));
            } catch (Exception e) {
                log.warn("Failed to decode text/plain body segment: {}", e.getMessage());
            }
        }

        if (mimeType.equalsIgnoreCase("text/html") && body.hasNonNull("data")) {
            String rawData = body.path("data").asText().replaceAll("\\s+", "");
            try {
                byte[] decoded = Base64.getUrlDecoder().decode(rawData);
                htmlBuilder.append(new String(decoded, StandardCharsets.UTF_8));
            } catch (Exception e) {
                log.warn("Failed to decode text/html body segment: {}", e.getMessage());
            }
        }

        JsonNode subParts = part.path("parts");
        if (subParts.isArray()) {
            for (JsonNode subPart : subParts) {
                extractPayloadData(subPart, plainBuilder, htmlBuilder, attachments);
            }
        }
    }

    /**
     * Extracts a named header value from a Gmail message headers JSON array.
     *
     * @param headersNode The JSON array node of headers.
     * @param name The case-insensitive header name to extract.
     * @return The header value, or an empty string if not found.
     */
    private static String extractHeader(JsonNode headersNode, String name) {
        if (headersNode == null || !headersNode.isArray()) {
            return "";
        }
        for (JsonNode header : headersNode) {
            if (name.equalsIgnoreCase(header.path("name").asText())) {
                return header.path("value").asText("");
            }
        }
        return "";
    }

    /**
     * Strips HTML tags and unescapes standard entities for clean plain text rendering.
     *
     * @param html The raw HTML string.
     * @return The clean plain text representation.
     */
    private static String sanitizeHtml(String html) {
        if (html == null || html.isBlank()) {
            return "";
        }
        return html.replaceAll("(?i)<style[^>]*>[^<]*</style>", "")
                .replaceAll("(?i)<script[^>]*>[^<]*</script>", "")
                .replaceAll("(?i)<br\\s*/?>", "\n")
                .replaceAll("(?i)</p>", "\n\n")
                .replaceAll("(?i)</div>", "\n")
                .replaceAll("(?i)</tr>", "\n")
                .replaceAll("(?i)<li[^>]*>", "- ")
                .replaceAll("(?i)</li>", "\n")
                .replaceAll("<[^>]+>", "")
                .replace("&nbsp;", " ")
                .replace("&amp;", "&")
                .replace("&lt;", "<")
                .replace("&gt;", ">")
                .replace("&quot;", "\"")
                .replace("&#39;", "'")
                .replace("&apos;", "'")
                .trim();
    }

    /**
     * Sanitizes a filename by replacing illegal characters with underscores.
     *
     * @param filename The raw filename.
     * @return The sanitized, filesystem-safe filename.
     */
    private static String sanitizeFilename(String filename) {
        if (filename == null || filename.isBlank()) {
            return "attachment";
        }
        return filename.replaceAll("[\\\\/:*?\"<>|]", "_");
    }

    /**
     * Formats a byte size into human-readable units (B, KB, MB, GB).
     *
     * @param bytes The size in bytes.
     * @return Formatted string representation.
     */
    private static String formatFileSize(long bytes) {
        if (bytes < 1024) {
            return bytes + " B";
        }
        int exp = (int) (Math.log(bytes) / Math.log(1024));
        char unit = "KMGTPE".charAt(exp - 1);
        return String.format("%.1f %sB", bytes / Math.pow(1024, exp), unit);
    }
}
