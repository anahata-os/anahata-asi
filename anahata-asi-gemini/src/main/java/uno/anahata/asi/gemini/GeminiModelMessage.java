/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini;

import com.google.genai.types.Candidate;
import com.google.genai.types.Citation;
import com.google.genai.types.Content;
import com.google.genai.types.FunctionCall;
import com.google.genai.types.Part;
import com.google.genai.types.SafetyRating;
import com.google.genai.types.HarmCategory;
import com.google.genai.types.HarmProbability;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.core.ModelBlobPart;
import uno.anahata.asi.model.core.ModelTextPart;
import uno.anahata.asi.model.core.ThoughtSignature;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.web.GroundingMetadata;
import uno.anahata.asi.model.web.GroundingSource;

/**
 * An object-oriented representation of a ModelMessage derived from the Gemini provider.
 * This class encapsulates the logic for constructing a valid ModelMessage from a
 * Gemini Content object, ensuring the parent message is created before its parts.
 *
 * @author anahata-ai
 */
@Slf4j
@Getter
public class GeminiModelMessage extends AbstractModelMessage<GeminiResponse, GeminiToolMessage> {

    /** The original, native Candidate object from the Google GenAI API. */
    private final transient Candidate geminiCandidate;
    
    /**
     * Constructs a GeminiModelMessage for streaming, without an initial candidate.
     * 
     * @param chat The parent chat session.
     * @param modelId The ID of the model.
     */
    public GeminiModelMessage(Chat chat, String modelId) {
        super(chat, modelId);
        this.geminiCandidate = null;
        setStreaming(true);
    }

    /**
     * Constructs a GeminiModelMessage, encapsulating the conversion logic.
     *
     * @param chat          The parent chat session.
     * @param modelId       The ID of the model that generated the content.
     * @param candidate The source Gemini Candidate object.
     * @param response The GeminiResponse that returned this message.
     */
    public GeminiModelMessage(Chat chat, String modelId, Candidate candidate, GeminiResponse response) {
        super(chat, modelId);
        this.geminiCandidate = candidate;
        setResponse(response);
        
        // Populate new fields from Candidate
        candidate.groundingMetadata().ifPresent(gm -> setGroundingMetadata(toAnahataGroundingMetadata(gm)));
        
        candidate.finishMessage().ifPresent(this::setFinishMessage);
        candidate.safetyRatings().ifPresent(sr -> setSafetyRatings(sr.stream()
            .map(s -> s.category().map(c -> c.knownEnum().name()).orElse("") + ":" + s.probability().map(p -> p.knownEnum().name()).orElse(""))
            .collect(Collectors.joining(", "))));
        setTokenCount(candidate.tokenCount().orElse(0));
        setRawJson(candidate.toJson());
        setCitationMetadata(candidate.citationMetadata()
            .map(cm -> cm.citations().orElse(List.of()).stream()
                .map(Citation::uri)
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.joining(", ")))
            .orElse(""));
        
        // All construction logic is now encapsulated here. The parent (this) exists
        // before any child parts are created and added. The AbstractPart constructor
        // adds the part to the message, so we just need a terminal operation to
        // trigger the stream.
        candidate.content().ifPresent(content -> content.parts().ifPresent(parts -> parts.stream()
            .map(this::toAnahataPart)
            .filter(Objects::nonNull)
            .collect(Collectors.toList()))); // Use a terminal operation that doesn't re-add the parts.
    }

    /**
     * Converts a Google GenAI Part to an Anahata AbstractPart within the context of this message.
     * This method encapsulates the logic previously in PartAdapter and FunctionCallAdapter.
     *
     * @param googlePart The Google part to convert.
     * @return The corresponding Anahata AbstractPart, or null if unsupported.
     */
    public AbstractPart toAnahataPart(Part googlePart) {
        byte[] thoughtSignature = googlePart.thoughtSignature().orElse(null);

        if (googlePart.text().isPresent()) {
            String text = googlePart.text().get();
            
            // As requested, all model text parts should be ModelTextPart.
            // Extract optional thought metadata.
            boolean thought = googlePart.thought().orElse(false);
            
            return new ModelTextPart(this, text, thoughtSignature, thought);
        }
        if (googlePart.functionCall().isPresent()) {
            AbstractToolCall toolCall = toAnahataToolCall(googlePart.functionCall().get());
            toolCall.setThoughtSignature(thoughtSignature); // Directly set, no cast needed
            return toolCall;
        }
        if (googlePart.inlineData().isPresent()) {
            com.google.genai.types.Blob googleBlob = googlePart.inlineData().get();
            return new ModelBlobPart(this, googleBlob.mimeType().orElse("application/octet-stream"), googleBlob.data().orElse(new byte[0]), thoughtSignature);
        }
        log.warn("Unsupported Gemini Part type for Anahata conversion, skipping: {}", googlePart);
        return null;
    }

    /**
     * Converts a Google GenAI FunctionCall to an Anahata AbstractToolCall.
     *
     * @param googleFc The FunctionCall received from the Google API.
     * @return A new AbstractToolCall.
     */
    private AbstractToolCall toAnahataToolCall(FunctionCall googleFc) {
        String name = googleFc.name().orElse("");
        Map<String, Object> args = googleFc.args().orElse(Map.of());
        String id = googleFc.id().orElse(null);

        // The ToolManager is accessible via the Chat reference in the message.
        return getChat().getToolManager().createToolCall(this, id, name, args);
    }

    @Override
    protected GeminiToolMessage createToolMessage() {
        return new GeminiToolMessage(this);
    }
    
    /**
     * Converts Google's GroundingMetadata to Anahata's domain model.
     * 
     * @param gm The Google GroundingMetadata object.
     * @return The Anahata GroundingMetadata object.
     */
    public static GroundingMetadata toAnahataGroundingMetadata(com.google.genai.types.GroundingMetadata gm) {
        List<String> webSearchQueries = gm.webSearchQueries().orElse(List.of());
        List<String> supportingTexts = gm.groundingSupports().orElse(List.of()).stream()
            .filter(gs -> gs.segment().isPresent() && gs.segment().get().text().isPresent())
            .map(gs -> gs.segment().get().text().get())
            .collect(Collectors.toList());

        List<GroundingSource> sources = gm.groundingChunks().orElse(List.of()).stream()
            .map(gc -> {
                if (gc.web().isPresent()) {
                    return GroundingSource.builder()
                        .title(gc.web().get().title().orElse("Unknown"))
                        .uri(gc.web().get().uri().orElse(""))
                        .build();
                }
                return null;
            })
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
        
        String searchEntryPointHtml = gm.searchEntryPoint()
                .flatMap(sep -> sep.renderedContent())
                .orElse(null);

        return new GroundingMetadata(
            webSearchQueries,
            supportingTexts,
            sources,
            searchEntryPointHtml,
            gm.toJson());
    }
}
