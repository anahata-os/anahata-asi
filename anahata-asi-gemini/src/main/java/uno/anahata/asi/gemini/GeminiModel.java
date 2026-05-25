/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.Client;
import com.google.genai.LocalTokenizer;
import com.google.genai.ResponseStream;
import com.google.genai.types.Candidate;
import com.google.genai.types.Citation;
import com.google.genai.types.Content;
import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.GenerateContentResponseUsageMetadata;
import com.google.genai.types.GoogleSearch;
import com.google.genai.types.ListModelsConfig;
import com.google.genai.types.Model;
import com.google.genai.types.Part;
import com.google.genai.types.ToolCodeExecution;
import java.io.InterruptedIOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.gemini.adapter.GeminiContentAdapter;
import uno.anahata.asi.gemini.adapter.GeminiFunctionDeclarationAdapter;
import uno.anahata.asi.gemini.adapter.RequestConfigAdapter;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.Response;
import uno.anahata.asi.agi.provider.StreamObserver;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ApiCallInterruptedException;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.provider.RetryableApiException;
import uno.anahata.asi.agi.tool.ToolResponseAttachment;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.gemini.adapter.GeminiPartAdapter;
import uno.anahata.asi.internal.ImageMetadataUtils;
import uno.anahata.asi.internal.ImageMetadataUtils.ImageMetadata;
import uno.anahata.asi.internal.JacksonUtils;

/**
 * Gemini-specific implementation of the {@code AbstractModel}. It wraps the
 * native Google GenAI {@code Model} object and implements the abstract methods
 * from the superclass by delegating to the wrapped object.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
public class GeminiModel extends AbstractModel {

    /**
     * The owning provider instance.
     */
    private final GeminiAiProvider provider;
    /**
     * The unique model identifier (e.g. 'models/gemini-1.5-flash').
     */
    private final String modelId;
    /**
     * The transient native model metadata. transient to avoid serialization of
     * SDK types.
     */
    private transient Model genaiModel;

    private transient com.google.genai.LocalTokenizer localTokenizer;

    /**
     * Constructs a new GeminiModel adapter.
     *
     * @param provider the owning provider instance.
     * @param genaiModel the native Google GenAI model metadata.
     */
    public GeminiModel(GeminiAiProvider provider, Model genaiModel) {
        this.provider = provider;
        this.genaiModel = genaiModel;
        this.modelId = genaiModel.name().orElseThrow(() -> new IllegalArgumentException("Model name is required"));
    }

    /**
     * Lazily restores or returns the native GenAI model metadata.
     *
     * @return The active Model instance.
     */
    private synchronized Model getGenaiModel() {
        if (genaiModel == null) {
            log.info("Restoring transient Gemini model: {}", modelId);
            var pager = provider.getClient().models.list(ListModelsConfig.builder().build());
            genaiModel = StreamSupport.stream(pager.spliterator(), false)
                    .filter(m -> modelId.equals(m.name().orElse(null)))
                    .findFirst()
                    .orElseThrow(() -> new IllegalStateException("Could not restore Gemini model: " + modelId));
        }
        return genaiModel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public AbstractAiProvider getProvider() {
        return provider;
    }

    /**
     * Lazily retrieves and caches the Google GenAI LocalTokenizer.
     * <p>
     * This helper method encapsulates the initialization of Google's native
     * LocalTokenizer, ensuring we fall back to a standard model
     * (gemini-2.5-flash) if the requested model is not supported by the local
     * tokenizer loader.
     * </p>
     *
     * @return The cached LocalTokenizer instance.
     */
    private synchronized LocalTokenizer getLocalTokenizer() {
        if (localTokenizer == null) {
            try {
                localTokenizer = new LocalTokenizer(getModelId());
            } catch (IllegalArgumentException e) {
                log.info("Model ID '{}' not supported by LocalTokenizerLoader, falling back to gemini-2.5-flash.", getModelId());
                localTokenizer = new LocalTokenizer("gemini-2.5-flash");
            }
        }
        return localTokenizer;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Utilizes Google's native LocalTokenizer to perform 100% accurate, offline
     * token counting.</p>
     *
     * @param text The text to count tokens for.
     * @return The token count, or 0 if the text is null or empty.
     */
    @Override
    public int countTokens(java.lang.String text) {
        if (text == null || text.isEmpty()) {
            return 0;
        }
        return getLocalTokenizer().countTokens(text).totalTokens().orElse(0);
    }

    /**
     * {@inheritDoc}
     * <p>
     * Translates the generic Anahata tool call into its native Google GenAI
     * Part representation (excluding any thought signatures to avoid token
     * inflation), serializes it to JSON, and tokenizes the JSON payload using
     * the cached LocalTokenizer.
     * </p>
     *
     * @param toolCall The tool call instance to tokenize.
     * @return The precise token count.
     */
    @Override
    public int countTokens(AbstractToolCall<?, ?> toolCall) {
        if (toolCall == null) {
            return 0;
        }
        try {
            Part googlePart = new GeminiPartAdapter(toolCall, false).toGoogle();
            if (googlePart != null) {
                return getLocalTokenizer().countTokens(googlePart.toJson()).totalTokens().orElse(0);
            }
        } catch (Exception e) {
            log.warn("Failed to count tokens for Gemini tool call, falling back to raw args", e);
        }
        return countTokens(JacksonUtils.serialize(toolCall.getEffectiveArgs()));
    }


    /**
     * {@inheritDoc}
     * <p>
     * Counts the multimodal tokens for raw binary data under Gemini's flat-rate billing scheme.
     * Gemini typically bills a model-independent flat-rate of 258 tokens per standard image, 
     * which is preserved here.
     * </p>
     * @param mimeType The MIME type of the binary data (e.g. "image/png").
     * @param data The raw binary data.
     * @return The precise token count, or 0 if the data is null or empty.
     */
    @Override public int countTokens(byte[] data, java.lang.String mimeType) {
        if (data == null || data.length == 0) {
            return 0;
        }
        if (mimeType != null && mimeType.startsWith("image/")) {
            ImageMetadata metadata = ImageMetadataUtils.readMetadata(data);
            if (metadata != null) {
                int width = metadata.getWidth();
                int height = metadata.getHeight();

                // 1. Low-Resolution check (both dimensions <= 384px)
                if (width <= 384 && height <= 384) {
                    return 258;
                }

                // 2. High-Resolution scaling: scale so shorter side is exactly 768px
                double scaleRatio = 768.0 / Math.min(width, height);
                int scaledWidth = (int) Math.ceil(width * scaleRatio);
                int scaledHeight = (int) Math.ceil(height * scaleRatio);

                // 3. Count 768x768 pixel tiles
                int tilesW = (int) Math.ceil(scaledWidth / 768.0);
                int tilesH = (int) Math.ceil(scaledHeight / 768.0);

                // 4. Each tile costs 258 tokens
                return (tilesW * tilesH) * 258;
            }
        }
        return 258;
    }
    /**
     * {@inheritDoc}
     * <p>
     * Translates the generic Anahata tool response into a native Google GenAI
     * FunctionResponse. It calculates the base tokens using the local offline tokenizer 
     * and manually aggregates the model-specific flat-rate tokens for each binary 
     * attachment (since Google's LocalTokenizer completely ignores FunctionResponse parts).
     * </p>
     * @param toolResponse The tool response instance to count.
     * @return The precise, billing-identical token count.
     */
    @Override public int countTokens(uno.anahata.asi.agi.tool.spi.AbstractToolResponse<?> toolResponse) {
        if (toolResponse == null) {
                    return 0;
                }
                try {
                    Part googlePart = GeminiPartAdapter.toGoogleFunctionResponsePart(toolResponse);
                    if (googlePart != null) {
                        // 1. Count the base tokens of the function response name and map using the local tokenizer.
                        int total = getLocalTokenizer().countTokens(googlePart.toJson()).totalTokens().orElse(0);

                        // 2. LocalTokenizer ignores parts() inside FunctionResponse. We must manually
                        // accumulate the flat-rate token cost for each binary attachment to match billing.
                        for (ToolResponseAttachment att : toolResponse.getAttachments()) {
                            total += countTokens(att.getData(), att.getMimeType());
                        }
                        return total;
                    }
                } catch (Exception e) {
                    log.warn("Failed to count tokens for Gemini tool response, falling back to raw serialization", e);
                }
                return countTokens(JacksonUtils.serialize(toolResponse));
    }
    /**
     * {@inheritDoc}
     */
    @Override
    public String getModelId() {
        return modelId;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDisplayName() {
        return getGenaiModel().displayName().orElse("");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        String desc = getGenaiModel().description().orElse("");
        String displayName = getDisplayName();
        if (desc.isEmpty() || desc.equalsIgnoreCase(displayName)) {
            return "";
        }
        return desc;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getVersion() {
        return getGenaiModel().version().orElse("");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMaxInputTokens() {
        return getGenaiModel().inputTokenLimit().orElse(0);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getMaxOutputTokens() {
        return getGenaiModel().outputTokenLimit().orElse(8192);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedActions() {
        return getGenaiModel().supportedActions().orElse(Collections.emptyList());
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Escapes special HTML characters in the model
     * metadata to ensure safe rendering in the NetBeans HTML view.</p>
     */
    @Override
    public String getRawDescription() {
        Model m = getGenaiModel();
        String json = m.toJson();
        String toString = m.toString();

        // Return only the inner content. WrappingHtmlPane add the <html><body> tags.
        return "<html><b>ID: </b>" + escapeHtml(getModelId()) + "<br>"
                + "<b>Display Name: </b>" + escapeHtml(getDisplayName()) + "<br>"
                + "<b>Version: </b>" + escapeHtml(getVersion()) + "<br>"
                + "<b>Description: </b>" + escapeHtml(getDescription()) + "<br>"
                + "<b>Supported Actions: </b>" + getSupportedActions() + "<br>"
                + "<b>Labels: </b>" + m.labels().orElse(Collections.EMPTY_MAP) + "<br>"
                + "<b>TunedModelInfo: </b>" + m.tunedModelInfo().orElse(null) + "<br>"
                + "<hr>"
                + "<b>toString():</b><pre style='white-space: pre-wrap; word-wrap: break-word;'></pre>"
                + "<div style='width: 300px;'>"
                + toString
                + "</pre></div></html>";
    }

    /**
     * Escapes special HTML characters in a string.
     *
     * @param text The text to escape.
     * @return The escaped text.
     */
    private String escapeHtml(String text) {
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#x27;")
                .replace("/", "&#x2F;");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsFunctionCalling() {
        // Currently we have no way of knowing if a model supports tool calling or not 
        // (because 'tool' is never listed as a supported action). Just always return true for now.
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsContentGeneration() {
        return getSupportedActions().contains("generateContent");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsBatchEmbeddings() {
        return getSupportedActions().contains("batchEmbedContents");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsEmbeddings() {
        return getSupportedActions().contains("embedContent");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isSupportsCachedContent() {
        return getSupportedActions().contains("createCachedContent");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getSupportedResponseModalities() {
        List<String> modalities = new ArrayList<>();
        modalities.add("TEXT");
        modalities.add("IMAGE");
        modalities.add("AUDIO");
        return modalities;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ServerTool> getAvailableServerTools() {
        List<ServerTool> tools = new ArrayList<>();
        tools.add(new ServerTool(GoogleSearch.class, "Google Search", "Search the web using Google."));
        //tools.add(new ServerTool(GoogleSearchRetrieval.class, "Google Search Retrieval", "Specialized retrieval tool powered by Google Search."));
        tools.add(new ServerTool(ToolCodeExecution.class, "Code Execution", "Enables the model to execute Python code as part of generation."));
        //tools.add(new ServerTool(GoogleMaps.class, "Google Maps", "Tool to support Google Maps in Model."));
        //tools.add(new ServerTool(EnterpriseWebSearch.class, "Enterprise Web Search", "Search the web using Enterprise Search."));
        //tools.add(new ServerTool(FileSearch.class, "File Search", "Search through uploaded files."));
        //tools.add(new ServerTool(ComputerUse.class, "Computer Use", "Enables the model to interact with a computer."));
        return tools;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ServerTool> getDefaultServerTools() {
        return getAvailableServerTools().stream()
                .filter(st -> st.getId().equals(GoogleSearch.class))
                .collect(Collectors.toList());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Float getDefaultTemperature() {
        return getGenaiModel().temperature().orElse(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Integer getDefaultTopK() {
        return getGenaiModel().topK().orElse(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Float getDefaultTopP() {
        return getGenaiModel().topP().orElse(null);
    }

    /**
     * Internal container for parameters used to invoke the Gemini API.
     *
     * @param history The list of content blocks to send.
     * @param historyJson The JSON representation of the history.
     * @param config The generation configuration.
     */
    private record GeminiGenerateContentParameters(List<Content> history, String historyJson, GenerateContentConfig config) {

    }

    /**
     * Synthesizes the history and configuration into SDK-ready parameters.
     *
     * @param request The source Anahata request.
     * @return The prepared parameters.
     */
    private GeminiGenerateContentParameters prepareGenerateContentParameters(GenerationRequest request) {
        RequestConfig config = request.config();
        List<AbstractMessage> history = request.history();
        boolean includePruned = config.isIncludePruned();
        String currentProviderUuid = getProvider().getUuid();

        // 1-to-N Mapping: A single turn-holding ModelMessage expands into multiple API contents.
        List<Content> googleHistory = history.stream()
                .map(msg -> new GeminiContentAdapter(msg, includePruned, currentProviderUuid).toGoogle())
                .flatMap(List::stream)
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(ArrayList::new));

        // Gemini API requirement: The first message in the history must be from the 'user' role.
        if (!googleHistory.isEmpty() && !"user".equals(googleHistory.get(0).role().orElse(""))) {
            googleHistory.add(0, Content.builder()
                    .role("user")
                    .parts(List.of(Part.fromText(" ")))
                    .build());
        }

        // Log the final history for debugging
        log.info("Final Google History ({} messages):", googleHistory.size());
        for (int i = 0; i < googleHistory.size(); i++) {
            Content c = googleHistory.get(i);
            log.debug("  [{}] Role: {}, Parts: {}", i, c.role().orElse("unknown"), c.parts().map(List::size).orElse(0));
        }

        String historyJson = googleHistory.stream()
                .map(Content::toJson)
                .collect(Collectors.joining(",\n", "[\n", "\n]"));

        GenerateContentConfig gcc = RequestConfigAdapter.toGoogle(config);
        return new GeminiGenerateContentParameters(googleHistory, historyJson, gcc);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Response generateContent(GenerationRequest request) {
        Client client = provider.getClient();
        GeminiGenerateContentParameters prepared = prepareGenerateContentParameters(request);

        log.info("Sending request to Gemini model: {} {} content elements", getModelId(), prepared.history().size());
        for (int i = 0; i < prepared.history().size(); i++) {
            Content c = prepared.history().get(i);
            log.debug("  Message {}: role={}, parts={}", i, c.role().orElse("N/A"), c.parts().map(List::size).orElse(0));
        }

        // 2. Make the API call
        try {
            GenerateContentResponse response = client.models.generateContent(
                    getModelId(),
                    prepared.history(),
                    prepared.config()
            );
            log.info("Got response from Gemini model: {}", response.toJson());

            // 3. Convert the Gemini response to the Anahata response using the new OO response class.
            Agi agi = request.config().getAgi();
            return new GeminiResponse(prepared.config().toJson(), prepared.historyJson(), agi, getModelId(), response);
        } catch (Exception e) {
            log.error("Exception in generateContent", e);
            if (isInterruption(e)) {
                throw new ApiCallInterruptedException(e);
            }
            if (isRetryable(e)) {
                provider.hokusPocus();
                throw new RetryableApiException(client.apiKey(), e.toString(), e);
            }
            throw e;
        }

    }

    /**
     * Serious intelligence to detect whether it is retriable or not.
     *
     * @param e the exception to check.
     * @return true if the error is considered transient/retryable.
     */
    private boolean isRetryable(Exception e) {
        return e.toString().contains("429") || e.toString().contains("503") || e.toString().contains("500") || e.toString().contains("499") || e.toString().contains("403");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        Client client = provider.getClient();
        GeminiGenerateContentParameters prepared = prepareGenerateContentParameters(request);
        Agi agi = request.config().getAgi();

        log.info("Streaming request to Gemini model: {} {} content elements", getModelId(), prepared.history().size());
        for (int i = 0; i < prepared.history().size(); i++) {
            Content c = prepared.history().get(i);
            log.debug("  Message {}: role={}, parts={}", i, c.role().orElse("N/A"), c.parts().map(List::size).orElse(0));
        }

        try {
            ResponseStream<GenerateContentResponse> stream = client.models.generateContentStream(
                    getModelId(), prepared.history(), prepared.config());

            List<GeminiModelMessage> targets = new ArrayList<>();
            boolean started = false;
            GeminiResponse lastGeminiResponse = null;

            int totalCandidatesTokens = 0;

            for (GenerateContentResponse chunk : stream) {
                String chunkJson = chunk.toJson();

                if (!started) {
                    List<Candidate> candidates = chunk.candidates().orElse(Collections.emptyList());
                    String modelVersion = chunk.modelVersion().orElse(getModelId());
                    for (int i = 0; i < candidates.size(); i++) {
                        targets.add(new GeminiModelMessage(agi, modelVersion));
                    }
                    observer.onStart((List) targets);
                    started = true;
                }

                for (GeminiModelMessage target : targets) {
                    target.appendRawJson(chunkJson);
                }

                handleChunk(chunk, targets);

                Optional<GenerateContentResponseUsageMetadata> usage = chunk.usageMetadata();
                if (usage.isPresent()) {
                    GenerateContentResponseUsageMetadata um = usage.get();
                    totalCandidatesTokens = Math.max(totalCandidatesTokens, um.candidatesTokenCount().orElse(0));
                    for (GeminiModelMessage target : targets) {
                        target.setBilledCompletionTokens(totalCandidatesTokens);
                        target.setBilledPromptTokens(um.promptTokenCount().orElse(0));
                    }
                }

                lastGeminiResponse = new GeminiResponse(prepared.config().toJson(), prepared.historyJson(), agi, getModelId(), chunk);
                observer.onNext(lastGeminiResponse);
            }

            if (lastGeminiResponse != null) {
                for (GeminiModelMessage target : targets) {
                    target.setResponse(lastGeminiResponse);
                    target.setModelId(lastGeminiResponse.getModelVersion());

                    if (target.getFinishReason() == null) {
                        target.setFinishReason(FinishReason.GOD_FUCKING_KNOWS);
                    }
                }
            }

            observer.onComplete();
        } catch (Exception e) {
            log.error("Exception in generateContentStream", e);
            if (isInterruption(e)) {
                observer.onError(new ApiCallInterruptedException(e));
            } else if (isRetryable(e)) {
                provider.hokusPocus();
                observer.onError(new RetryableApiException(client.apiKey(), e.toString(), e));
            } else {
                observer.onError(e);
            }
        }
    }

    /**
     * Checks if the given exception or any of its causes is an interruption.
     *
     * @param e The exception to check.
     * @return true if it's an interruption, false otherwise.
     */
    private boolean isInterruption(Throwable e) {
        Throwable t = e;
        while (t != null) {
            if (t instanceof InterruptedException || t instanceof InterruptedIOException) {
                return true;
            }
            t = t.getCause();
        }
        return false;
    }

    /**
     * Processes a single streaming chunk by appending its deltas to the
     * corresponding target messages.
     *
     * @param chunk The raw chunk from the Gemini API.
     * @param targets The persistent ModelMessage objects being updated.
     */
    private void handleChunk(GenerateContentResponse chunk, List<GeminiModelMessage> targets) {
        List<Candidate> candidates = chunk.candidates().orElse(Collections.emptyList());

        for (int i = 0; i < Math.min(candidates.size(), targets.size()); i++) {
            Candidate c = candidates.get(i);
            GeminiModelMessage target = targets.get(i);

            c.content().ifPresent(content -> content.parts().ifPresent(parts -> {
                for (Part p : parts) {
                    if (p.text().isPresent()) {
                        String text = p.text().get();
                        byte[] sig = p.thoughtSignature().orElse(null);
                        boolean isThought = p.thought().orElse(false);

                        List<AbstractPart> activeParts = target.getParts();
                        AbstractPart lastPart = activeParts.isEmpty() ? null : activeParts.get(activeParts.size() - 1);

                        boolean canAppend = false;
                        if (lastPart instanceof ModelTextPart mtp && mtp.isThought() == isThought) {
                            if (!text.isEmpty()) {
                                mtp.appendText(text);
                            }
                            if (sig != null) {
                                mtp.setThoughtSignature(sig);
                            }
                            canAppend = true;
                        }

                        if (!canAppend) {
                            if (!text.isEmpty() || sig != null) {
                                target.addTextPart(text, sig, isThought);
                            }
                        }
                    } else if (p.functionCall().isPresent()) {
                        // Guard against duplicate tool calls if the API repeats parts in chunks.
                        String callId = p.functionCall().get().id().orElse(null);
                        if (callId != null && target.getToolCalls().stream().anyMatch(tc -> callId.equals(tc.getId()))) {
                            log.warn("Duplicate tool call ID received in stream, skipping: {}", callId);
                            continue;
                        }
                        target.toAnahataPart(p);
                    } else {
                        // For other non-text parts, use the unified logic.
                        target.toAnahataPart(p);
                    }
                }
            }));

            handleResponseMetadata(c, chunk, target, targets.size());
        }
    }

    /**
     * Unified handler for response metadata (finish reason, grounding).
     *
     * @param c The candidate object.
     * @param response The full response or chunk.
     * @param target The target Anahata message.
     * @param candidateCount Total number of candidates in the response.
     */
    private void handleResponseMetadata(Candidate c, GenerateContentResponse response, GeminiModelMessage target, int candidateCount) {
        // 1. Finish Reason
        c.finishReason().ifPresent(fr -> target.setFinishReason(GeminiModelMessage.toAnahataFinishReason(fr)));
        c.finishMessage().ifPresent(target::setFinishMessage);

        // 2. Citations
        c.citationMetadata().ifPresent(cm -> {
            String citations = cm.citations().orElse(List.of()).stream()
                    .map(Citation::uri)
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(Collectors.joining(", "));
            target.setCitationMetadata(citations);
        });

        // 3. Grounding
        c.groundingMetadata().ifPresent(gm -> {
            target.setGroundingMetadata(GeminiModelMessage.toAnahataGroundingMetadata(gm));
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        FunctionDeclaration fd = new GeminiFunctionDeclarationAdapter(tool, config.isUseNativeSchemas()).toGoogle();
        return fd != null ? fd.toJson() : "{}";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return getDisplayName().isEmpty() ? modelId : getDisplayName();
    }

}
