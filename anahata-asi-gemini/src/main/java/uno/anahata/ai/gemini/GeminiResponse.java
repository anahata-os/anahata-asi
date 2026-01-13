/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.gemini;

import com.google.genai.types.Candidate;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.GenerateContentResponsePromptFeedback;
import com.google.genai.types.GenerateContentResponseUsageMetadata;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.Getter;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.Response;
import uno.anahata.ai.model.core.ResponseUsageMetadata;
import uno.anahata.ai.model.web.GroundingMetadata;

/**
 * A specialized, object-oriented Response class for the Gemini provider.
 * It encapsulates all the logic for converting a native Google GenerateContentResponse
 * into the Anahata domain model, making it a self-contained and reusable component.
 *
 * @author anahata
 */
@Getter
public class GeminiResponse extends Response<GeminiModelMessage> {

    /** The original, native response object from the Google GenAI API. */
    private final transient GenerateContentResponse genaiResponse;

    // --- Final fields to hold the converted data ---
    private final List<GeminiModelMessage> candidates;
    private final ResponseUsageMetadata usageMetadata;
    private final Optional<String> promptFeedback;
    private final String rawRequestConfigJson;
    private final String rawHistoryJson;
    private final String rawJson;    
    private final String modelVersion;

    /**
     * Constructs a GeminiResponse, performing the full conversion from the native
     * Google GenAI response to the Anahata domain model.
     *
     * @param requestConfigJson The raw JSON of the request configuration.
     * @param historyJson   The raw JSON of the conversation history sent in the request.
     * @param chat          The parent chat session, required for constructing model messages.
     * @param modelId       The ID of the model that generated this response.
     * @param genaiResponse The native response object from the API.
     */
    public GeminiResponse(String requestConfigJson, String historyJson, Chat chat, String modelId, GenerateContentResponse genaiResponse) {
        this.rawRequestConfigJson = requestConfigJson;
        this.rawHistoryJson = historyJson;
        this.genaiResponse = genaiResponse;        
        this.rawJson = genaiResponse.toJson();
        this.modelVersion = genaiResponse.modelVersion().orElse(modelId);
        
        // --- 1. Convert Usage Metadata ---
        this.usageMetadata = genaiResponse.usageMetadata()
            .map(this::convertUsageMetadata)
            .orElse(ResponseUsageMetadata.builder().build()); // Default empty metadata

        // --- 2. Convert Candidates ---
        List<Candidate> googleCandidates = genaiResponse.candidates().orElse(Collections.emptyList());
        this.candidates = googleCandidates.stream()
            .map(candidate -> {
                GeminiModelMessage msg = new GeminiModelMessage(chat, modelVersion, candidate, this);
                // Fallback: If candidate doesn't have its own token count and there's only one candidate,
                // use the total candidatesTokenCount from usageMetadata.
                if (msg.getTokenCount() <= 0 && googleCandidates.size() == 1) {
                    msg.setTokenCount(usageMetadata.getCandidatesTokenCount());
                }
                return msg;
            })
            .collect(Collectors.toList());

        // --- 3. Convert Prompt Feedback ---
        this.promptFeedback = genaiResponse.promptFeedback()
            .flatMap(GenerateContentResponsePromptFeedback::blockReasonMessage);
        
    }

    /**
     * Converts the native GenAI usage metadata to the V2 core model.
     *
     * @param genaiUsage The native usage metadata.
     * @return The V2 {@code ResponseUsageMetadata}.
     */
    private ResponseUsageMetadata convertUsageMetadata(GenerateContentResponseUsageMetadata genaiUsage) {
        return ResponseUsageMetadata.builder()
            .promptTokenCount(genaiUsage.promptTokenCount().orElse(0))
            .candidatesTokenCount(genaiUsage.candidatesTokenCount().orElse(0))
            .cachedContentTokenCount(genaiUsage.cachedContentTokenCount().orElse(0))
            .thoughtsTokenCount(genaiUsage.thoughtsTokenCount().orElse(0))
            .toolUsePromptTokenCount(genaiUsage.toolUsePromptTokenCount().orElse(0))
            .totalTokenCount(genaiUsage.totalTokenCount().orElse(0))
            .rawJson(genaiUsage.toJson())
            .build();
    }

    @Override
    public int getTotalTokenCount() {
        return usageMetadata.getTotalTokenCount();
    }
}
