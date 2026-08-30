/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.agi.provider;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;
import uno.anahata.asi.persistence.kryo.KryoUtils;

/**
 * The abstract base class for a specific AI model (e.g.,
 * "gemini-1.5-pro-latest"). In the V2 architecture, this class is the
 * definitive entry point for generating content, creating a clean,
 * object-oriented API where the model itself is the actor.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@Getter
@Setter
public abstract class AbstractModel {

    /**
     * The parent AI provider that owns this model instance. Mark transient to prevent serialization of provider graph in model caches.
     */
    protected transient AbstractAiProvider provider;

    /**
     * Whether this model is enabled and offered in model selectors and tool listings.
     */
    protected boolean enabled = true;

    /**
     * The optional model-specific tokenizer. If null, the provider's default is
     * used.
     */
    protected TokenizerType tokenizerType;

    /**
     * Checks if this model is registered in its parent provider's master persisted models list.
     *
     * @return true if this model exists in {@code provider.getModels()}, false otherwise.
     */
    public boolean isRegistered() {
        if (provider == null) {
            return false;
        }
        return provider.getModels().contains(this);
    }

    /**
     * Checks if this locally registered model entity has different configuration values compared to its
     * corresponding cached API model in the provider.
     *
     * @return true if any field value differs, false if identical or no cached API model is available.
     * @throws IllegalStateException if called on an unregistered model.
     */
    public boolean hasDiscrepancy() {
        if (!isRegistered()) {
            throw new IllegalStateException("hasDiscrepancy should only be called in locally registered models");
        }
        AbstractModel other = provider.getCachedApiModel(getModelId());
        if (other == null) {
            return false;
        }
        return !Objects.equals(getDisplayName(), other.getDisplayName())
                || !Objects.equals(getDescription(), other.getDescription())
                || !Objects.equals(getVersion(), other.getVersion())
                || !Objects.equals(getMaxInputTokens(), other.getMaxInputTokens())
                || !Objects.equals(getMaxOutputTokens(), other.getMaxOutputTokens())
                || !Objects.equals(getDefaultTemperature(), other.getDefaultTemperature())
                || !Objects.equals(getDefaultTopP(), other.getDefaultTopP())
                || !Objects.equals(getDefaultTopK(), other.getDefaultTopK())
                || !Objects.equals(getSupportedResponseModalities(), other.getSupportedResponseModalities())
                || !Objects.equals(getSupportedActions(), other.getSupportedActions());
    }

    /**
     * Resets this locally registered model entity from its cached API model counterpart and persists changes to disk.
     *
     * @throws IOException if persisting the updated model fails.
     * @throws IllegalStateException if this model is not registered or no cached API model is available.
     */
    public synchronized void resetFromApi() throws IOException {
        if (!isRegistered()) {
            throw new IllegalStateException("resetFromApi should only be called in locally registered models");
        }
        AbstractModel other = provider.getCachedApiModel(getModelId());
        if (other == null) {
            throw new IllegalStateException("No cached API model available for ID: " + getModelId());
        }
        other.setTokenizerType(this.getTokenizerType());
        provider.removeModel(this);
        provider.addModel(other);
    }

    /**
     * Converts a model ID into a safe filename with .kryo extension.
     *
     * @param modelId The raw model ID string.
     * @return A safe filename for disk storage.
     */
    public static String toSafeModelFileName(String modelId) {
        if (modelId == null || modelId.isBlank()) {
            return "unknown_model.kryo";
        }
        return modelId.replace('/', '_')
                .replace('\\', '_')
                .replace(':', '_')
                .replace('*', '_')
                .replace('?', '_')
                .replace('"', '_')
                .replace('<', '_')
                .replace('>', '_')
                .replace('|', '_') + ".kryo";
    }

    /**
     * Persists this model entity directly to disk in its parent provider's models directory (~/.anahata/asi/&lt;provider&gt;/models/&lt;model_id&gt;.kryo).
     *
     * @throws IOException If creating the directory or saving the file fails.
     */
    public synchronized void persist() throws IOException {
        if (provider == null) {
            throw new IllegalStateException("Cannot persist model '" + getModelId() + "' because parent provider is null");
        }
        String id = getModelId();
        if (id == null || id.isBlank()) {
            throw new IllegalStateException("Cannot persist model because model ID is null or blank");
        }
        Path targetFile = provider.getModelsDirectory().resolve(toSafeModelFileName(id));
        KryoUtils.saveToFile(this, targetFile);
        log.debug("Persisted model '{}' to {}", id, targetFile);
    }

    /**
     * Deletes the persisted .kryo file for this model from disk and removes it from its parent provider.
     *
     * @throws IOException If deleting the file fails.
     */
    public synchronized void remove() throws IOException {
        if (provider == null) {
            throw new IllegalStateException("Cannot remove model '" + getModelId() + "' because parent provider is null");
        }
        String id = getModelId();
        if (id == null || id.isBlank()) {
            throw new IllegalStateException("Cannot remove model because model ID is null or blank");
        }
        Path targetFile = provider.getModelsDirectory().resolve(toSafeModelFileName(id));
        Files.deleteIfExists(targetFile);
        log.info("Deleted persisted model file for '{}' at {}", id, targetFile);
        provider.removeModel(this);
    }

    /**
     * Gets the effective tokenizer type for this model.
     *
     * @return The model's tokenizer if set, otherwise the parent provider's
     * tokenizer.
     */
    public TokenizerType getTokenizerType() {
        return tokenizerType != null ? tokenizerType : getProvider().getTokenizerType();
    }

    /**
     * Counts the number of tokens in the given text string using this model's
     * specific tokenizer.
     *
     * @param text The text to count tokens for.
     * @return The number of tokens, or 0 if the text is null or empty.
     */
    public abstract int countTokens(java.lang.String text);

    /**
     * Counts the number of tokens consumed by the given tool call in the
     * context window.
     * <p>
     * Model subclasses override this method to serialize the tool call into its
     * exact wire-format (JSON, Protobuf, etc.) to ensure 100% accurate billing.
     * </p>
     *
     * @param toolCall The tool call to count tokens for.
     * @return The precise token count.
     */
    public abstract int countTokens(AbstractToolCall<?, ?> toolCall);


    /**
     * Counts the number of tokens consumed by raw binary data based on its MIME type
     * and model-specific multimodal billing rules.
     * <p>
     * This generic signature provides complete decoupling from domain part classes,
     * allowing the model to tokenize any binary payload (such as blob parts or tool attachments).
     * </p>
     * @param mimeType The MIME type of the binary data (e.g. "image/png").
     * @param data The raw binary data.
     * @return The precise token count, or 0 if no model is active or the data is null.
     */
    public abstract int countTokens(byte[] data, String mimeType);

    /**
     * Counts the number of tokens consumed by the given tool execution
     * response.
     * <p>
     * Model subclasses override this to serialize the response into its exact
     * wire-format (Protobuf FunctionResponse, JSON, etc.) to ensure 100%
     * accurate billing.
     * </p>
     *
     * @param toolResponse The tool response to count tokens for.
     * @return The exact number of tokens.
     */
    public abstract int countTokens(uno.anahata.asi.agi.tool.spi.AbstractToolResponse<?> toolResponse);

    /**
     * Gets the unique identifier for this model (e.g.,
     * "models/gemini-1.5-pro").
     *
     * @return The model ID.
     */
    public abstract String getModelId();

    /**
     * Gets the human-readable display name for this model.
     *
     * @return The display name.
     */
    public abstract String getDisplayName();

    /**
     * Gets a detailed description of the model's capabilities and limitations.
     *
     * @return The model description.
     */
    public abstract String getDescription();

    /**
     * Gets the version string for this model.
     *
     * @return The version.
     */
    public abstract String getVersion();

    /**
     * Gets the maximum number of input tokens supported by this model.
     *
     * @return The input token limit, or null if unspecified/unknown by provider.
     */
    public abstract Integer getMaxInputTokens();

    /**
     * Gets the maximum number of output tokens this model can generate in a
     * single turn.
     *
     * @return The output token limit, or null if unspecified/unknown by provider.
     */
    public abstract Integer getMaxOutputTokens();

    /**
     * Gets the list of supported API actions for this model (e.g.,
     * "generateContent").
     *
     * @return A list of supported actions.
     */
    public abstract List<String> getSupportedActions();

    /**
     * Gets a rich, potentially HTML-formatted description of the model,
     * including all its metadata.
     *
     * @return The raw description string.
     */
    public abstract String getRawDescription();

    /**
     * Delegate method to get the id of this models provider.
     *
     * @return The unique ID of the provider.
     */
    public final String getProviderId() {
        return getProvider().getProviderId();
    }

    // --- Abstract Capability Methods ---
    /**
     * Checks if this model supports native function calling (tools).
     *
     * @return true if supported.
     */
    public abstract boolean isSupportsFunctionCalling();

    /**
     * Checks if this model supports content generation.
     *
     * @return true if supported.
     */
    public abstract boolean isSupportsContentGeneration();

    /**
     * Checks if this model supports batch embedding generation.
     *
     * @return true if supported.
     */
    public abstract boolean isSupportsBatchEmbeddings();

    /**
     * Checks if this model supports single content embedding generation.
     *
     * @return true if supported.
     */
    public abstract boolean isSupportsEmbeddings();

    /**
     * Checks if this model supports content caching.
     *
     * @return true if supported.
     */
    public abstract boolean isSupportsCachedContent();

    /**
     * Gets the list of response modalities supported by this model (e.g.,
     * TEXT, IMAGE, AUDIO, VIDEO).
     *
     * @return A list of supported response modalities.
     */
    public abstract List<ResponseModality> getSupportedResponseModalities();

    /**
     * Checks if this model matches a search query (regex or substring) AND all target response modalities.
     *
     * @param queryPattern Optional compiled regex pattern to match against model attributes. If null, matches all.
     * @param targetModalities Optional set of required response modalities. If not empty, the model must support ALL specified modalities.
     * @return true if the model satisfies all active criteria (AND logic).
     */
    public boolean matches(Pattern queryPattern, Set<ResponseModality> targetModalities) {
        if (queryPattern != null) {
            String id = getModelId() != null ? getModelId() : "";
            String name = getDisplayName() != null ? getDisplayName() : "";
            String desc = getDescription() != null ? getDescription() : "";
            String actions = getSupportedActions() != null ? String.join(" ", getSupportedActions()) : "";
            String provName = getProvider() != null ? getProvider().getDisplayName() : "";
            String provUuid = getProvider() != null ? getProvider().getUuid() : "";
            String version = getVersion() != null ? getVersion() : "";
            String mods = getSupportedResponseModalities() != null
                    ? getSupportedResponseModalities().stream().map(Enum::name).collect(Collectors.joining(" "))
                    : "";

            boolean queryMatch = queryPattern.matcher(id).find()
                    || queryPattern.matcher(name).find()
                    || queryPattern.matcher(desc).find()
                    || queryPattern.matcher(actions).find()
                    || queryPattern.matcher(provName).find()
                    || queryPattern.matcher(provUuid).find()
                    || queryPattern.matcher(version).find()
                    || queryPattern.matcher(mods).find();

            if (!queryMatch) {
                return false;
            }
        }

        if (targetModalities != null && !targetModalities.isEmpty()) {
            List<ResponseModality> supported = getSupportedResponseModalities();
            if (supported == null || !supported.containsAll(targetModalities)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Gets the list of server-side tools available for this model.
     *
     * @return A list of available server tools.
     */
    public abstract List<ServerTool> getAvailableServerTools();

    /**
     * Gets the list of server-side tools that should be enabled by default for
     * this model.
     *
     * @return The list of default server tools.
     */
    public abstract List<ServerTool> getDefaultServerTools();

    /**
     * Gets the default temperature for this model.
     * <p>
     * Temperature controls the randomness of the response. Higher values (e.g.,
     * 1.0) make the output more random, while lower values (e.g., 0.2) make it
     * more deterministic.
     * </p>
     *
     * @return The default temperature, or null if not specified.
     */
    public abstract Float getDefaultTemperature();

    /**
     * Gets the default topK for this model.
     * <p>
     * Top-K sampling limits the model's choices to the K most likely next
     * tokens.
     * </p>
     *
     * @return The default topK, or null if not specified.
     */
    public abstract Integer getDefaultTopK();

    /**
     * Gets the default topP for this model.
     * <p>
     * Top-P (nucleus) sampling selects tokens whose cumulative probability adds
     * up to the threshold P.
     * </p>
     *
     * @return The default topP, or null if not specified.
     */
    public abstract Float getDefaultTopP();

    /**
     * The core method for interacting with an AI model. It takes a
     * configuration object and a list of messages and returns a standardized
     * Response.
     *
     * @param request The generation request containing config and history.
     * @return A standardized {@link Response} object.
     */
    public abstract Response generateContent(GenerationRequest request);

    /**
     * Generates content asynchronously using token streaming.
     *
     * @param request The generation request containing config and history.
     * @param observer The observer that will receive the streaming response
     * chunks.
     */
    public abstract void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer);

    /**
     * Gets the provider-specific JSON representation of a tool's declaration.
     * This is used by the UI to show exactly what is being sent to the model.
     *
     * @param tool The tool to inspect.
     * @param config The request configuration (e.g. to check useNativeSchemas).
     * @return The JSON string representing the tool declaration.
     */
    public abstract String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config);

    /**
     * Returning the display name
     *
     * @return the display name
     */
    @Override
    public String toString() {
        return getDisplayName();
    }

    /**
     * Markdown table header for model listings.
     */
    public static final String MARKUP_TABLE_HEADER =
            "| Provider UUID | Model ID | Display Name | Enabled | Version | Modalities | In Tokens | Out Tokens | Actions | Description |\n"
            + "|---|---|---|---|---|---|---|---|---|---|\n";

    /**
     * Formats this model as a Markdown table row for model listing and discovery tools.
     *
     * @return A Markdown row representing this model.
     */
    public String toMarkupRow() {
        String provUuid = getProvider() != null ? getProvider().getUuid() : "N/A";
        String id = getModelId();
        String displayName = getDisplayName() != null && !getDisplayName().isBlank() ? getDisplayName() : "N/A";
        String enabledStr = isEnabled() ? "✅ YES" : "❌ NO";
        String version = getVersion() != null && !getVersion().isBlank() ? getVersion() : "N/A";
        String modalities = getSupportedResponseModalities() != null && !getSupportedResponseModalities().isEmpty()
                ? getSupportedResponseModalities().stream().map(Enum::name).collect(java.util.stream.Collectors.joining(", ")) : "TEXT";
        String inTokens = getMaxInputTokens() != null ? String.valueOf(getMaxInputTokens()) : "N/A";
        String outTokens = getMaxOutputTokens() != null ? String.valueOf(getMaxOutputTokens()) : "N/A";
        String actions = getSupportedActions() != null && !getSupportedActions().isEmpty()
                ? String.join(", ", getSupportedActions()) : "N/A";
        String desc = getDescription() != null && !getDescription().isBlank() ? getDescription().replace("\n", " ").trim() : "N/A";

        return "| " + provUuid
                + " | " + id
                + " | " + displayName
                + " | " + enabledStr
                + " | " + version
                + " | " + modalities
                + " | " + inTokens
                + " | " + outTokens
                + " | " + actions
                + " | " + desc
                + " |\n";
    }
}
