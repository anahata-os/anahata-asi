/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.gemini;

import com.google.genai.types.Candidate;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.GenerateContentResponsePromptFeedback;
import com.google.genai.types.GenerateContentResponseUsageMetadata;
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
    private final String rawJson;    
    private final String modelVersion;

    /**
     * Constructs a GeminiResponse, performing the full conversion from the native
     * Google GenAI response to the Anahata domain model.
     *
     * @param requestConfigJson
     * @param chat          The parent chat session, required for constructing model messages.
     * @param modelId       The ID of the model that generated this response.
     * @param genaiResponse The native response object from the API.
     */
    public GeminiResponse(String requestConfigJson, Chat chat, String modelId, GenerateContentResponse genaiResponse) {
        // The superclass is abstract, so no super() call is needed here.
        this.rawRequestConfigJson = requestConfigJson;
        this.genaiResponse = genaiResponse;        
        this.rawJson = genaiResponse.toJson();
        this.modelVersion = genaiResponse.modelVersion().orElse(modelId);
        
        // --- 1. Convert Candidates ---
        this.candidates = genaiResponse.candidates().get().stream()
            .map(candidate -> new GeminiModelMessage(chat, modelVersion, candidate, this))
            .collect(Collectors.toList());

        // --- 2. Convert Usage Metadata ---
        this.usageMetadata = genaiResponse.usageMetadata()
            .map(this::convertUsageMetadata)
            .orElse(ResponseUsageMetadata.builder().build()); // Default empty metadata

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
            .rawJson(genaiUsage.toJson()) // Added rawJson population
            .build();
    }

    // --- Implementation of Abstract Methods ---

    @Override
    public int getTotalTokenCount() {
        return usageMetadata.getTotalTokenCount(); // Delegate to usageMetadata
    }
}
