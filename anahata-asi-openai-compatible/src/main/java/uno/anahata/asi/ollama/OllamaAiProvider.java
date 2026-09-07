/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.ollama;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.openai.compatible.OpenAiChatCompletionsProvider;
import uno.anahata.asi.openai.compatible.OpenAiCompatibleModel;
import uno.anahata.asi.persistence.Rebindable;

/**
 * Dedicated AI Provider for local and self-hosted Ollama servers.
 * <p>
 * Seamlessly manages Ollama instances by querying native management endpoints ({@code /api/tags},
 * {@code /api/ps}, {@code /api/version}, {@code /api/show}) to extract exact model capabilities,
 * GGUF parameter sizes, quantization formats, and allocated context lengths, while routing chat completions
 * through the standardized OpenAI-compatible pipeline.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class OllamaAiProvider extends OpenAiChatCompletionsProvider {

    /**
     * Constructs a default Ollama provider targeting localhost port 11434 with API keys disabled by default.
     */
    public OllamaAiProvider() {
        super("Ollama", "Ollama", "http://localhost:11434/v1", "https://ollama.com");
        setApiKeyRequired(false);
        setDescription("Local and self-hosted LLM runner supporting models like Llama, Qwen, DeepSeek, and Mistral.");
    }

    /**
     * Constructs an Ollama provider instance with custom UUID, display name, and base URL.
     * 
     * @param uuid the unique provider identifier
     * @param displayName the human-readable provider name
     * @param baseUrl the API base URL
     */
    public OllamaAiProvider(String uuid, String displayName, String baseUrl) {
        super(uuid, displayName, baseUrl, "https://ollama.com");
        setApiKeyRequired(false);
        setDescription("Local and self-hosted LLM runner supporting models like Llama, Qwen, DeepSeek, and Mistral.");
    }

    /**
     * Resolves the server root URL without the {@code /v1} or {@code /api} suffix.
     * 
     * @return the base HTTP host and port (e.g. "http://localhost:11434")
     */
    public String getServerUrl() {
        String url = getBaseUrl();
        if (url == null || url.isBlank()) {
            return "http://localhost:11434";
        }
        String clean = url.trim();
        while (clean.endsWith("/")) {
            clean = clean.substring(0, clean.length() - 1);
        }
        if (clean.endsWith("/v1")) {
            clean = clean.substring(0, clean.length() - "/v1".length());
        } else if (clean.endsWith("/api/chat")) {
            clean = clean.substring(0, clean.length() - "/api/chat".length());
        } else if (clean.endsWith("/api")) {
            clean = clean.substring(0, clean.length() - "/api".length());
        }
        while (clean.endsWith("/")) {
            clean = clean.substring(0, clean.length() - 1);
        }
        return clean;
    }

    /**
     * Automatically normalizes user-entered URLs by stripping trailing slashes or native {@code /api/chat} endpoints
     * and ensuring it terminates with {@code /v1} for OpenAI chat completion compliance.
     * 
     * @param raw the raw URL entered by the user
     */
    @Override
    public void setBaseUrl(String raw) {
        if (raw == null || raw.isBlank()) {
            super.setBaseUrl("http://localhost:11434/v1");
            return;
        }
        String clean = raw.trim();
        while (clean.endsWith("/")) {
            clean = clean.substring(0, clean.length() - 1);
        }
        if (clean.endsWith("/api/chat")) {
            clean = clean.substring(0, clean.length() - "/api/chat".length());
        } else if (clean.endsWith("/api")) {
            clean = clean.substring(0, clean.length() - "/api".length());
        }
        while (clean.endsWith("/")) {
            clean = clean.substring(0, clean.length() - 1);
        }
        if (!clean.endsWith("/v1")) {
            clean = clean + "/v1";
        }
        super.setBaseUrl(clean);
    }

    /**
     * Queries the Ollama server for its running version string via {@code GET /api/version}.
     * 
     * @return the version string (e.g. "0.30.11"), or null if unreachable
     * @throws Exception if network communication fails
     */
    public String getServerVersion() throws Exception {
        String url = getServerUrl() + "/api/version";
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(10))
                .header("Accept", "application/json")
                .GET()
                .build();
        try (HttpClient client = createHttpClient()) {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() == 200) {
                JsonNode node = JacksonUtils.parse(response.body(), JsonNode.class);
                return node.path("version").asText(null);
            }
        }
        return null;
    }

    /**
     * Retrieves the list of models currently loaded in host memory and GPU VRAM via {@code GET /api/ps}.
     * 
     * @return a list of {@link OllamaRunningModel} descriptors
     * @throws Exception if network communication or parsing fails
     */
    public List<OllamaRunningModel> getRunningModels() throws Exception {
        String url = getServerUrl() + "/api/ps";
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(10))
                .header("Accept", "application/json")
                .GET()
                .build();
        try (HttpClient client = createHttpClient()) {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() == 200) {
                JsonNode root = JacksonUtils.parse(response.body(), JsonNode.class);
                JsonNode modelsNode = root.get("models");
                if (modelsNode != null && modelsNode.isArray()) {
                    List<OllamaRunningModel> running = new ArrayList<>();
                    for (JsonNode m : modelsNode) {
                        running.add(OllamaRunningModel.fromNode(m));
                    }
                    return running;
                }
            }
        }
        return Collections.emptyList();
    }

    /**
     * Fetches the list of publicly available models from the official Ollama online registry
     * ({@code https://ollama.com/api/tags}), sorted by last modified date in descending order (newest first).
     *
     * @return a sorted list of remote models
     * @throws Exception if network communication or parsing fails
     */
    public static List<OllamaRemoteModel> fetchRemoteModels() throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://ollama.com/api/tags"))
                .timeout(Duration.ofSeconds(15))
                .header("User-Agent", "Mozilla/5.0")
                .header("Accept", "application/json")
                .GET()
                .build();
        try (HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()) {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() == 200) {
                JsonNode root = JacksonUtils.parse(response.body(), JsonNode.class);
                JsonNode modelsNode = root.get("models");
                if (modelsNode != null && modelsNode.isArray()) {
                    List<OllamaRemoteModel> list = new ArrayList<>();
                    for (JsonNode mNode : modelsNode) {
                        list.add(OllamaRemoteModel.fromNode(mNode));
                    }
                    list.sort((a, b) -> {
                        if (a.modifiedAt() == null && b.modifiedAt() == null) {
                            return a.name().compareToIgnoreCase(b.name());
                        }
                        if (a.modifiedAt() == null) {
                            return 1;
                        }
                        if (b.modifiedAt() == null) {
                            return -1;
                        }
                        return b.modifiedAt().compareTo(a.modifiedAt());
                    });
                    return list;
                }
            }
            throw new RuntimeException("HTTP " + response.statusCode() + ": " + response.body());
        }
    }

    /**
     * Set of model names currently being pulled in the background.
     * Marked transient so Kryo does not attempt to serialize active thread listeners when persisting provider state.
     */
    private transient Map<String, Consumer<OllamaPullProgress>> activePulls = new ConcurrentHashMap<>();

    /**
     * {@inheritDoc}
     * <p>Re-initializes transient active pull tracking map after Kryo deserialization.</p>
     */
    @Override
    public void rebind() {
        super.rebind();
        if (activePulls == null) {
            activePulls = new ConcurrentHashMap<>();
        }
    }

    /**
     * Checks if a specific model is currently being pulled by this provider.
     * 
     * @param modelName the model identifier
     * @return true if an active pull is in progress for this model
     */
    public boolean isPulling(String modelName) {
        return modelName != null && activePulls.containsKey(modelName.trim());
    }

    /**
     * Checks if any model pull operation is currently active on this provider.
     * 
     * @return true if at least one pull is active
     */
    public boolean hasActivePulls() {
        return !activePulls.isEmpty();
    }

    /**
     * Initiates a resilient background model pull managed at the provider level.
     * 
     * @param modelName the model name and tag to pull
     * @param listener a consumer notified on each progress update
     * @param onComplete runnable executed when the pull completes successfully
     * @param onError consumer notified if the pull fails
     */
    public void startPull(String modelName, Consumer<OllamaPullProgress> listener, Runnable onComplete, Consumer<Throwable> onError) {
        String cleanName = modelName.trim();
        activePulls.put(cleanName, listener != null ? listener : p -> {});
        
        ExecutorService exec = getAsiContainer() != null ? getAsiContainer().getExecutor() : ForkJoinPool.commonPool();
        exec.submit(() -> {
            try {
                pullModel(cleanName, progress -> {
                    Consumer<OllamaPullProgress> activeListener = activePulls.get(cleanName);
                    if (activeListener != null) {
                        activeListener.accept(progress);
                    }
                });
                activePulls.remove(cleanName);
                refreshCachedApiModels();
                if (onComplete != null) {
                    onComplete.run();
                }
            } catch (Throwable t) {
                activePulls.remove(cleanName);
                log.error("Failed to pull model '{}'", cleanName, t);
                if (onError != null) {
                    onError.accept(t);
                }
            }
        });
    }

    /**
     * Downloads and installs a model onto the Ollama server by streaming progress from {@code POST /api/pull}.
     *
     * @param modelName the repository name and tag of the model to pull (e.g. "qwen2.5-coder:7b")
     * @param progressConsumer a consumer notified of each progress event
     * @throws Exception if network communication fails or Ollama returns an error
     */
    public void pullModel(String modelName, Consumer<OllamaPullProgress> progressConsumer) throws Exception {
        String url = getServerUrl() + "/api/pull";
        ObjectNode payload = JsonNodeFactory.instance.objectNode();
        payload.put("name", modelName);
        payload.put("stream", true);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofHours(2))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(payload.toString()))
                .build();

        try (HttpClient client = createHttpClient()) {
            HttpResponse<Stream<String>> response = client.send(request, HttpResponse.BodyHandlers.ofLines());
            if (response.statusCode() != 200) {
                String errorBody = "HTTP " + response.statusCode();
                try (Stream<String> lines = response.body()) {
                    errorBody = lines.collect(Collectors.joining("\n"));
                }
                throw new RuntimeException("Failed to pull model: " + errorBody);
            }
            try (Stream<String> lines = response.body()) {
                lines.forEach(line -> {
                    if (line != null && !line.isBlank()) {
                        try {
                            JsonNode node = JacksonUtils.parse(line.trim(), JsonNode.class);
                            OllamaPullProgress progress = OllamaPullProgress.fromNode(node);
                            if (progress.error() != null) {
                                throw new RuntimeException(progress.error());
                            }
                            if (progressConsumer != null) {
                                progressConsumer.accept(progress);
                            }
                        } catch (Exception e) {
                            if (e instanceof RuntimeException re) {
                                throw re;
                            }
                            log.error("Failed to parse pull progress line: {}", line, e);
                        }
                    }
                });
            }
        }
    }

    /**
     * Instructs Ollama to immediately unload a model from memory/VRAM by passing {@code keep_alive: 0}.
     * 
     * @param modelName the identifier of the model to unload
     * @throws Exception if the request fails
     */
    public void unloadModel(String modelName) throws Exception {
        String url = getServerUrl() + "/api/generate";
        ObjectNode payload = JsonNodeFactory.instance.objectNode();
        payload.put("model", modelName);
        payload.put("keep_alive", 0);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(15))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(payload.toString()))
                .build();
        try (HttpClient client = createHttpClient()) {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            log.info("Unloaded model {} with status {}", modelName, response.statusCode());
        }
    }

    /**
     * Queries detailed architectural and template parameters for a model via {@code POST /api/show}.
     * 
     * @param modelName the identifier of the model to inspect
     * @return the raw JsonNode containing parameters, template, license, and modelfile
     * @throws Exception if the request fails
     */
    public JsonNode getModelInfo(String modelName) throws Exception {
        String url = getServerUrl() + "/api/show";
        ObjectNode payload = JsonNodeFactory.instance.objectNode();
        payload.put("name", modelName);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(15))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(payload.toString()))
                .build();
        try (HttpClient client = createHttpClient()) {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() == 200) {
                return JacksonUtils.parse(response.body(), JsonNode.class);
            }
            throw new RuntimeException("HTTP " + response.statusCode() + ": " + response.body());
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Queries native Ollama model tags via {@code GET /api/tags}. Falls back gracefully to standard
     * OpenAI {@code /models} if the native endpoint is unavailable.
     * </p>
     */
    @Override
    public List<? extends AbstractModel> listModels() {
        String tagsUrl = getServerUrl() + "/api/tags";
        log.info("Querying native Ollama model tags from {}", tagsUrl);
        try {
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(tagsUrl))
                    .timeout(Duration.ofSeconds(15))
                    .header("Accept", "application/json")
                    .GET()
                    .build();
            try (HttpClient client = createHttpClient()) {
                HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
                if (response.statusCode() == 200) {
                    JsonNode root = JacksonUtils.parse(response.body(), JsonNode.class);
                    JsonNode modelsNode = root.get("models");
                    if (modelsNode != null && modelsNode.isArray()) {
                        List<OllamaModel> list = new ArrayList<>();
                        for (JsonNode mNode : modelsNode) {
                            list.add(new OllamaModel(this, mNode));
                        }
                        log.info("Discovered {} models from native Ollama /api/tags at {}", list.size(), tagsUrl);
                        return list;
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Failed to query /api/tags from {}, falling back to standard /models: {}", tagsUrl, e.getMessage());
        }

        return super.listModels();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Creates an {@link OllamaModel} instance from the supplied JSON node.
     * </p>
     */
    @Override
    protected OpenAiCompatibleModel createModel(JsonNode node) {
        return new OllamaModel(this, node);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Connects to Ollama, inspects the server version, and summarizes discovered models.
     * </p>
     */
    @Override
    public String testConnection() throws Exception {
        String version = null;
        try {
            version = getServerVersion();
        } catch (Exception e) {
            log.warn("Could not retrieve Ollama version: {}", e.getMessage());
        }
        List<? extends AbstractModel> discovered = refreshCachedApiModels();
        StringBuilder sb = new StringBuilder();
        if (version != null && !version.isBlank()) {
            sb.append("Connected to Ollama v").append(version).append(" at ").append(getServerUrl()).append("!\n\n");
        } else {
            sb.append("Connected to Ollama at ").append(getServerUrl()).append("!\n\n");
        }
        sb.append("Discovered ").append(discovered.size()).append(" model(s):\n");
        for (AbstractModel m : discovered) {
            sb.append(" • ").append(m.getDisplayName()).append("\n");
        }
        return sb.toString();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Provides an informational notice for the API keys configuration.
     * </p>
     */
    @Override
    public String getApiKeyHint() {
        return "# Ollama typically does not require an API key.\n"
                + "# If your reverse proxy requires authorization, enter keys below (one per line).";
    }
}
