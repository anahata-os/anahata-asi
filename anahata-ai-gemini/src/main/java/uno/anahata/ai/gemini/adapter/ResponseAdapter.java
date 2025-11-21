package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.Candidate;
import com.google.genai.types.GenerateContentResponse;
import com.google.genai.types.GenerateContentResponseUsageMetadata;
import java.util.List;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.ModelMessage;
import uno.anahata.ai.tool.ToolManager;

/**
 * A focused adapter responsible for converting a Google GenAI GenerateContentResponse
 * into our model-agnostic Response object.
 *
 * @author pablo
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ResponseAdapter {

    /**
     * Converts a Google GenAI GenerateContentResponse to an Anahata Response.
     *
     * @param genaiResponse The response from the Google API.
     * @param toolManager The ToolManager, required for creating tool calls from the response.
     * @param modelId The ID of the model that generated this response.
     * @return The corresponding Anahata Response object.
     */
    public static uno.anahata.ai.model.core.Response toAnahata(
            GenerateContentResponse genaiResponse,
            ToolManager toolManager,
            String modelId) {

        List<AbstractMessage> candidates = genaiResponse.candidates().get().stream()
            .map(candidate -> toAnahataMessage(candidate, toolManager, modelId))
            .collect(Collectors.toList());

        int promptTokenCount = genaiResponse.usageMetadata().flatMap(GenerateContentResponseUsageMetadata::promptTokenCount).orElse(0);
        int totalTokenCount = genaiResponse.usageMetadata().flatMap(GenerateContentResponseUsageMetadata::totalTokenCount).orElse(0);
        
        String finishReason = genaiResponse.candidates().get().stream().findFirst()
            .flatMap(Candidate::finishReason)
            .map(Object::toString)
            .orElse("UNKNOWN");

        return uno.anahata.ai.model.core.Response.builder()
            .candidates(candidates)
            .finishReason(finishReason)
            .promptTokenCount(promptTokenCount)
            .totalTokenCount(totalTokenCount)
            .build();
    }

    private static ModelMessage toAnahataMessage(Candidate candidate, ToolManager toolManager, String modelId) {
        // The ContentAdapter handles the conversion of the message content.
        ModelMessage message = ContentAdapter.toAnahata(candidate.content().get(), toolManager);
        // We then populate the metadata from the higher-level Candidate object.
        message.setTokenCount(candidate.tokenCount().orElse(0));
        message.setModelId(modelId);
        return message;
    }
}