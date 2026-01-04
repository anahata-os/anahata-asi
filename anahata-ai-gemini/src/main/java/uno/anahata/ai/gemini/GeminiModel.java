/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini;

import com.google.genai.Client;
import com.google.genai.ResponseStream;
import com.google.genai.errors.ClientException;
import com.google.genai.types.Candidate;
import com.google.genai.types.Citation;
import com.google.genai.types.ComputerUse;
import com.google.genai.types.Content;
import com.google.genai.types.EnterpriseWebSearch;
import com.google.genai.types.FileSearch;
import com.google.genai.types.GenerateContentConfig;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.GoogleMaps;
import com.google.genai.types.GoogleSearch;
import com.google.genai.types.GoogleSearchRetrieval;
import com.google.genai.types.Model;
import com.google.genai.types.Part;
import com.google.genai.types.ToolCodeExecution;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.gemini.adapter.GeminiContentAdapter;
import uno.anahata.ai.gemini.adapter.RequestConfigAdapter;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.ModelBlobPart;
import uno.anahata.ai.model.core.ModelTextPart;
import uno.anahata.ai.model.core.RequestConfig;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.StreamObserver;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.provider.AbstractAiProvider;
import uno.anahata.ai.model.provider.AbstractModel;
import uno.anahata.ai.model.provider.ServerTool;
import uno.anahata.ai.tool.RetryableApiException;

/**
 * Gemini-specific implementation of the {@code AbstractModel}. It wraps the
 * native Google GenAI {@code Model} object and implements the abstract methods
 * from the superclass by delegating to the wrapped object.
 *
 * @author anahata-gemini-pro-2.5
 */
@RequiredArgsConstructor
@Slf4j
public class GeminiModel extends AbstractModel {

    private final GeminiAiProvider provider;
    private final Model genaiModel;

    @Override
    public AbstractAiProvider getProvider() {
        return provider;
    }

    @Override
    public String getModelId() {
        return genaiModel.name().orElse("");
    }

    @Override
    public String getDisplayName() {
        return genaiModel.displayName().orElse("");
    }

    @Override
    public String getDescription() {
        String desc = genaiModel.description().orElse("");
        String displayName = getDisplayName();
        if (desc.isEmpty() || desc.equalsIgnoreCase(displayName)) {
            return "";
        }
        return desc;
    }

    @Override
    public String getVersion() {
        return genaiModel.version().orElse("");
    }

    @Override
    public int getMaxInputTokens() {
        return genaiModel.inputTokenLimit().orElse(0);
    }

    @Override
    public int getMaxOutputTokens() {
        return genaiModel.outputTokenLimit().orElse(0);
    }

    @Override
    public List<String> getSupportedActions() {
        return genaiModel.supportedActions().orElse(Collections.emptyList());
    }

    @Override
    public String getRawDescription() {
        String json = genaiModel.toJson();
        String toString = genaiModel.toString();

        // Return only the inner content. WrappingHtmlPane will add the <html><body> tags.
        return "<html><b>ID: </b>" + escapeHtml(getModelId()) + "<br>"
                + "<b>Display Name: </b>" + escapeHtml(getDisplayName()) + "<br>"
                + "<b>Version: </b>" + escapeHtml(getVersion()) + "<br>"
                + "<b>Description: </b>" + escapeHtml(getDescription()) + "<br>"
                + "<b>Supported Actions: </b>" + getSupportedActions() + "<br>"
                + "<b>Labels: </b>" + genaiModel.labels().orElse(Collections.EMPTY_MAP) + "<br>"
                + "<b>TunedModelInfo: </b>" + genaiModel.tunedModelInfo().orElse(null) + "<br>"
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
        return genaiModel.temperature().orElse(null);
    }

    @Override
    public Integer getDefaultTopK() {
        return genaiModel.topK().orElse(null);
    }

    @Override
    public Float getDefaultTopP() {
        return genaiModel.topP().orElse(null);
    }

    @Override
    public Response generateContent(Chat chat, RequestConfig config, List<AbstractMessage> history) {
        Client client = provider.getClient();

        // 1. Adapt the anahata-ai request to the Gemini-specific request using the new OO adapter
        boolean includePruned = config.isIncludePruned();
        List<Content> googleHistory = history.stream()
                .map(msg -> new GeminiContentAdapter(msg, includePruned).toGoogle())
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        log.info("Sending request to Gemini model: {} {} content elements", getModelId(), googleHistory.size());
        for (Content content : googleHistory) {
            log.info(content.toJson());
        }

        // 2. Make the API call
        log.info("Sending request to Gemini model: {}", getModelId());
        try {
            GenerateContentConfig gcc = RequestConfigAdapter.toGoogle(config);
            log.info("GenerateContentConfig: {}", gcc.toJson());
            
            GenerateContentResponse response = client.models.generateContent(
                    getModelId(),
                    googleHistory,
                    gcc
                    
            );
            log.info("Got response from Gemini model: {}", response.toJson());

            // 3. Convert the Gemini response to the Anahata response using the new OO response class.
            return new GeminiResponse(gcc.toJson(), chat, getModelId(), response);
        } catch (ClientException e) {
            log.error("Exception in generateContent", e);
            if (e.toString().contains("429") || e.toString().contains("503") || e.toString().contains("500")) {
                log.error("429, 503 or 500 exception, resetting client", e.getMessage());
                provider.resetClient();
                throw new RetryableApiException(client.apiKey(), e.toString(), e);
            }
            throw e;
        }

    }

    @Override
    public void generateContentStream(Chat chat, RequestConfig config, List<AbstractMessage> history, StreamObserver<Response<? extends AbstractModelMessage>, ? extends AbstractModelMessage> observer) {
        Client client = provider.getClient();
        boolean includePruned = config.isIncludePruned();
        List<Content> googleHistory = history.stream()
                .map(msg -> new GeminiContentAdapter(msg, includePruned).toGoogle())
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        try {
            GenerateContentConfig gcc = RequestConfigAdapter.toGoogle(config);
            ResponseStream<GenerateContentResponse> stream = client.models.generateContentStream(
                    getModelId(), googleHistory, gcc);

            // "Targets" are our persistent ModelMessage objects that will live in the Chat history.
            // We create one target for each candidate the model decides to generate.
            List<GeminiModelMessage> targets = new ArrayList<>();
            boolean started = false;
            GeminiResponse lastGeminiResponse = null;

            for (GenerateContentResponse chunk : stream) {
                // The first chunk is special: it tells us how many candidates (targets) we need to track.
                if (!started) {
                    List<Candidate> candidates = chunk.candidates().orElse(Collections.emptyList());
                    String modelVersion = chunk.modelVersion().orElse(getModelId());
                    for (int i = 0; i < candidates.size(); i++) {
                        // We use the lightweight constructor because the chunk's Candidate is partial.
                        targets.add(new GeminiModelMessage(chat, modelVersion));
                    }
                    log.info("calling observer onStart " + targets);
                    // Notify the orchestrator (Chat) so it can add these messages to the UI/history immediately.
                    observer.onStart((List)targets);
                    started = true;
                }

                // Pour the deltas from this chunk into our persistent target messages.
                handleChunk(chunk, targets);
                
                // Notify the observer that a new chunk has been processed.
                // The Response object here is ephemeral, used only to carry metadata for this chunk.
                lastGeminiResponse = new GeminiResponse(gcc.toJson(), chat, getModelId(), chunk);
                observer.onNext(lastGeminiResponse);
            }
            
            // FINALIZATION: When the stream is complete, we set the raw JSON of the final chunk
            // on each target message. This enables the "Json" view in the UI.
            // Note: This is the JSON of the last chunk, which for Gemini usually contains 
            // the full metadata for that candidate.
            if (lastGeminiResponse != null) {
                for (GeminiModelMessage target : targets) {
                    target.setResponse(lastGeminiResponse);
                    target.setRawJson(lastGeminiResponse.getRawJson());
                }
            }
            
            observer.onComplete();
        } catch (Exception e) {
            log.error("Exception in generateContentStream", e);
            if (e.toString().contains("429") || e.toString().contains("503") || e.toString().contains("500")) {
                provider.resetClient();
                observer.onError(new RetryableApiException(client.apiKey(), e.toString(), e));
            } else {
                observer.onError(e);
            }
        }
    }

    /**
     * Processes a single streaming chunk by appending its deltas to the corresponding target messages.
     * 
     * @param chunk The raw chunk from the Gemini API.
     * @param targets The persistent ModelMessage objects being updated.
     */
    private void handleChunk(GenerateContentResponse chunk, List<GeminiModelMessage> targets) {
        System.out.println("handling chunk " + chunk.text());
        try {
        Thread.sleep(500);    
        } catch (Exception e) {
        }
        
        List<Candidate> candidates = chunk.candidates().orElse(Collections.emptyList());
        
        // Iterate through each candidate delta in the chunk and apply it to the matching target.
        for (int i = 0; i < Math.min(candidates.size(), targets.size()); i++) {
            Candidate c = candidates.get(i);
            GeminiModelMessage target = targets.get(i);
            
            c.content().ifPresent(content -> content.parts().ifPresent(parts -> {
                for (Part p : parts) {
                    if (p.text().isPresent()) {
                        String text = p.text().get();
                        boolean isThought = p.thought().orElse(false);
                        
                        List<AbstractPart> activeParts = target.getParts();
                        AbstractPart lastPart = activeParts.isEmpty() ? null : activeParts.get(activeParts.size() - 1);
                        
                        // APPEND LOGIC:
                        // If the last part in the target is a TextPart of the same "type" (thought vs speech),
                        // we append the new text to it. This keeps the UI rendering smooth.
                        boolean canAppend = false;
                        if (lastPart instanceof ModelTextPart mtp && mtp.isThought() == isThought) {
                            mtp.appendText(text);
                            canAppend = true;
                        }
                        
                        // NEW PART LOGIC:
                        // If we can't append (e.g., the model switched from thinking to speaking),
                        // we create a brand new part. The constructor handles self-registration to the target.
                        if (!canAppend) {
                            log.info("Creating new model part for " + chunk);
                            new ModelTextPart(target, text, p.thoughtSignature().orElse(null), isThought);
                        }
                    } else if (p.inlineData().isPresent() || p.functionCall().isPresent()) {
                        // For non-text parts (blobs, tool calls), we use the standard conversion logic.
                        // These are usually sent in a single chunk, not streamed token-by-token.
                        target.toAnahataPart(p);
                    }
                }
            }));
            
            // Update metadata as it becomes available in the stream.
            c.finishReason().ifPresent(fr -> target.setFinishReason(fr.knownEnum().name()));
            c.finishMessage().ifPresent(target::setFinishMessage);
            c.tokenCount().ifPresent(target::setTokenCount);
            
            c.citationMetadata().ifPresent(cm -> {
                String citations = cm.citations().orElse(List.of()).stream()
                    .map(Citation::uri)
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(Collectors.joining(", "));
                target.setCitationMetadata(citations);
            });
        }
    }

    @Override
    public String toString() {
        return genaiModel.displayName().orElse(genaiModel.name().orElse("??"));

    }

}
