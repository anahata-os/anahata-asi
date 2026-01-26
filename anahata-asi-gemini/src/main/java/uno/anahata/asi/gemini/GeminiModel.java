/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.Client;
import com.google.genai.ResponseStream;
import com.google.genai.errors.ClientException;
import com.google.genai.types.Candidate;
import com.google.genai.types.Citation;
import com.google.genai.types.ComputerUse;
import com.google.genai.types.Content;
import com.google.genai.types.EnterpriseWebSearch;
import com.google.genai.types.FileSearch;
import com.google.genai.types.FunctionDeclaration;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.GenerateContentResponseUsageMetadata;
import com.google.genai.types.GoogleMaps;
import com.google.genai.types.GoogleSearch;
import com.google.genai.types.GoogleSearchRetrieval;
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
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.gemini.adapter.GeminiContentAdapter;
import uno.anahata.asi.gemini.adapter.GeminiFunctionDeclarationAdapter;
import uno.anahata.asi.gemini.adapter.RequestConfigAdapter;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.GenerationRequest;
import uno.anahata.asi.model.core.ModelTextPart;
import uno.anahata.asi.model.core.RequestConfig;
import uno.anahata.asi.model.core.Response;
import uno.anahata.asi.model.core.StreamObserver;
import uno.anahata.asi.model.provider.AbstractAiProvider;
import uno.anahata.asi.model.provider.AbstractModel;
import uno.anahata.asi.model.provider.ApiCallInterruptedException;
import uno.anahata.asi.model.provider.ServerTool;
import uno.anahata.asi.model.tool.AbstractTool;
import uno.anahata.asi.tool.RetryableApiException;

/**
 * Gemini-specific implementation of the {@code AbstractModel}. It wraps the
 * native Google GenAI {@code Model} object and implements the abstract methods
 * from the superclass by delegating to the wrapped object.
 *
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
public class GeminiModel extends AbstractModel {

    private final GeminiAiProvider provider;
    private final String modelId;
    private transient Model genaiModel;

    public GeminiModel(GeminiAiProvider provider, Model genaiModel) {
        this.provider = provider;
        this.genaiModel = genaiModel;
        this.modelId = genaiModel.name().orElseThrow(() -> new IllegalArgumentException("Model name is required"));
    }

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

    @Override
    public AbstractAiProvider getProvider() {
        return provider;
    }

    @Override
    public String getModelId() {
        return modelId;
    }

    @Override
    public String getDisplayName() {
        return getGenaiModel().displayName().orElse("");
    }

    @Override
    public String getDescription() {
        String desc = getGenaiModel().description().orElse("");
        String displayName = getDisplayName();
        if (desc.isEmpty() || desc.equalsIgnoreCase(displayName)) {
            return "";
        }
        return desc;
    }

    @Override
    public String getVersion() {
        return getGenaiModel().version().orElse("");
    }

    @Override
    public int getMaxInputTokens() {
        return getGenaiModel().inputTokenLimit().orElse(0);
    }

    @Override
    public int getMaxOutputTokens() {
        return getGenaiModel().outputTokenLimit().orElse(0);
    }

    @Override
    public List<String> getSupportedActions() {
        return getGenaiModel().supportedActions().orElse(Collections.emptyList());
    }

    @Override
    public String getRawDescription() {
        Model m = getGenaiModel();
        String json = m.toJson();
        String toString = m.toString();

        // Return only the inner content. WrappingHtmlPane will add the <html><body> tags.
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

    private String escapeHtml(String text) {
        return text.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#x27;")
                .replace("/", "&#x2F;");
    }

    @Override
    public boolean isSupportsFunctionCalling() {
        // Currently we have no way of knowing if a model supports tool calling or not 
        // (because 'tool' is never listed as a supported action). Just always return true for now.
        return true;
    }

    @Override
    public boolean isSupportsContentGeneration() {
        return getSupportedActions().contains("generateContent");
    }

    @Override
    public boolean isSupportsBatchEmbeddings() {
        return getSupportedActions().contains("batchEmbedContents");
    }

    @Override
    public boolean isSupportsEmbeddings() {
        return getSupportedActions().contains("embedContent");
    }

    @Override
    public boolean isSupportsCachedContent() {
        return getSupportedActions().contains("createCachedContent");
    }

    @Override
    public List<String> getSupportedResponseModalities() {
        List<String> modalities = new ArrayList<>();
        modalities.add("TEXT");
        modalities.add("IMAGE");
        modalities.add("AUDIO");
        return modalities;
    }

    @Override
    public List<ServerTool> getAvailableServerTools() {
        List<ServerTool> tools = new ArrayList<>();
        tools.add(new ServerTool(GoogleSearch.class, "Google Search", "Search the web using Google."));
        tools.add(new ServerTool(GoogleSearchRetrieval.class, "Google Search Retrieval", "Specialized retrieval tool powered by Google Search."));
        tools.add(new ServerTool(ToolCodeExecution.class, "Code Execution", "Enables the model to execute Python code as part of generation."));
        tools.add(new ServerTool(GoogleMaps.class, "Google Maps", "Tool to support Google Maps in Model."));
        tools.add(new ServerTool(EnterpriseWebSearch.class, "Enterprise Web Search", "Search the web using Enterprise Search."));
        tools.add(new ServerTool(FileSearch.class, "File Search", "Search through uploaded files."));
        tools.add(new ServerTool(ComputerUse.class, "Computer Use", "Enables the model to interact with a computer."));
        return tools;
    }

    @Override
    public Float getDefaultTemperature() {
        return getGenaiModel().temperature().orElse(null);
    }

    @Override
    public Integer getDefaultTopK() {
        return getGenaiModel().topK().orElse(null);
    }

    @Override
    public Float getDefaultTopP() {
        return getGenaiModel().topP().orElse(null);
    }

    private record GeminiGenerateContentParameters(List<Content> history, String historyJson, GenerateContentConfig config) {}

    private GeminiGenerateContentParameters prepareGenerateContentParameters(GenerationRequest request) {
        RequestConfig config = request.config();
        List<AbstractMessage> history = request.history();
        boolean includePruned = config.isIncludePruned();

        List<Content> googleHistory = history.stream()
                .map(msg -> new GeminiContentAdapter(msg, includePruned).toGoogle())
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        String historyJson = googleHistory.stream()
                .map(Content::toJson)
                .collect(Collectors.joining(",\n", "[\n", "\n]"));

        GenerateContentConfig gcc = RequestConfigAdapter.toGoogle(config);
        return new GeminiGenerateContentParameters(googleHistory, historyJson, gcc);
    }

    @Override
    public Response generateContent(GenerationRequest request) {
        Client client = provider.getClient();
        GeminiGenerateContentParameters prepared = prepareGenerateContentParameters(request);
        
        log.info("Sending request to Gemini model: {} {} content elements", getModelId(), prepared.history().size());

        // 2. Make the API call
        try {
            GenerateContentResponse response = client.models.generateContent(
                    getModelId(),
                    prepared.history(),
                    prepared.config()
            );
            log.info("Got response from Gemini model: {}", response.toJson());

            // 3. Convert the Gemini response to the Anahata response using the new OO response class.
            Chat chat = request.config().getChat();
            return new GeminiResponse(prepared.config().toJson(), prepared.historyJson(), chat, getModelId(), response);
        } catch (ClientException e) {
            log.error("Exception in generateContent", e);
            if (isInterruption(e)) {
                throw new ApiCallInterruptedException(e);
            }
            if (e.toString().contains("429") || e.toString().contains("503") || e.toString().contains("500") || e.toString().contains("499")) {
                provider.resetClient();
                throw new RetryableApiException(client.apiKey(), e.toString(), e);
            }
            throw e;
        }

    }

    @Override
    public void generateContentStream(GenerationRequest request, StreamObserver<Response<? extends AbstractModelMessage>> observer) {
        Client client = provider.getClient();
        GeminiGenerateContentParameters prepared = prepareGenerateContentParameters(request);
        Chat chat = request.config().getChat();

        try {
            ResponseStream<GenerateContentResponse> stream = client.models.generateContentStream(
                    getModelId(), prepared.history(), prepared.config());

            List<GeminiModelMessage> targets = new ArrayList<>();
            boolean started = false;
            GeminiResponse lastGeminiResponse = null;
            
            // Accumulators for usage metadata
            int totalCandidatesTokens = 0;

            for (GenerateContentResponse chunk : stream) {
                String chunkJson = chunk.toJson();
                
                if (!started) {
                    List<Candidate> candidates = chunk.candidates().orElse(Collections.emptyList());
                    String modelVersion = chunk.modelVersion().orElse(getModelId());
                    for (int i = 0; i < candidates.size(); i++) {
                        targets.add(new GeminiModelMessage(chat, modelVersion));
                    }
                    observer.onStart((List)targets);
                    started = true;
                }

                for (GeminiModelMessage target : targets) {
                    target.appendRawJson(chunkJson);
                }

                handleChunk(chunk, targets);
                
                // Accumulate billed tokens if present in the chunk
                Optional<GenerateContentResponseUsageMetadata> usage = chunk.usageMetadata();
                if (usage.isPresent()) {
                    GenerateContentResponseUsageMetadata um = usage.get();
                    // Gemini usually sends the total accumulated so far in each chunk.
                    totalCandidatesTokens = Math.max(totalCandidatesTokens, um.candidatesTokenCount().orElse(0));
                    for (GeminiModelMessage target : targets) {
                        target.setBilledTokenCount(totalCandidatesTokens);
                    }
                }
                
                lastGeminiResponse = new GeminiResponse(prepared.config().toJson(), prepared.historyJson(), chat, getModelId(), chunk);
                observer.onNext(lastGeminiResponse);
            }
            
            if (lastGeminiResponse != null) {
                for (GeminiModelMessage target : targets) {
                    target.setResponse(lastGeminiResponse);
                    // Ensure the final model version is set
                    target.setModelId(lastGeminiResponse.getModelVersion());
                }
            }
            
            observer.onComplete();
        } catch (Exception e) {
            log.error("Exception in generateContentStream", e);
            if (isInterruption(e)) {
                observer.onError(new ApiCallInterruptedException(e));
            } else if (e.toString().contains("429") || e.toString().contains("503") || e.toString().contains("500")) {
                provider.resetClient();
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
     * Processes a single streaming chunk by appending its deltas to the corresponding target messages.
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
                                new ModelTextPart(target, text, sig, isThought);
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
                        // For other non-text parts, use the unified conversion logic.
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
        c.finishReason().ifPresent(fr -> target.setFinishReason(fr.knownEnum().name()));
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

    @Override
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        FunctionDeclaration fd = new GeminiFunctionDeclarationAdapter(tool, config.isUseNativeSchemas()).toGoogle();
        return fd != null ? fd.toJson() : "{}";
    }

    @Override
    public String toString() {
        return getDisplayName().isEmpty() ? modelId : getDisplayName();
    }

}
