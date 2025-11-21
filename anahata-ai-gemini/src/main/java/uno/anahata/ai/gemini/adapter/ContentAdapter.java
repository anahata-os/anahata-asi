package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.Content;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import uno.anahata.ai.model.core.AbstractMessage;
import uno.anahata.ai.model.core.ModelMessage;
import uno.anahata.ai.tool.ToolManager;

/**
 * A focused adapter for bidirectional conversion between Anahata's AbstractMessage
 * and Google GenAI's Content object.
 * @author pablo
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ContentAdapter {

    /**
     * Converts an Anahata AbstractMessage to a Google GenAI Content object.
     * @param anahataMessage The message to convert.
     * @return The corresponding Content object.
     */
    public static Content toGoogle(AbstractMessage anahataMessage) {
        Content.Builder builder = Content.builder()
            .role(anahataMessage.getRole().name().toLowerCase());

        List<com.google.genai.types.Part> googleParts = anahataMessage.getParts().stream()
            .map(PartAdapter::toGoogle)
            .filter(Objects::nonNull)
            .collect(Collectors.toList());

        builder.parts(googleParts);
        return builder.build();
    }

    /**
     * Converts a Google GenAI Content object to an Anahata ModelMessage.
     * Note: This method does not populate metadata like tokenCount or modelId,
     * as that information is only available at a higher level (e.g., the Candidate).
     * @param googleContent The content to convert.
     * @param toolManager The ToolManager, required for creating tool calls.
     * @return The corresponding ModelMessage.
     */
    public static ModelMessage toAnahata(com.google.genai.types.Content googleContent, ToolManager toolManager) {
        List<uno.anahata.ai.model.core.AbstractPart> anahataParts = googleContent.parts().get().stream()
            .map(part -> PartAdapter.toAnahata(part, toolManager))
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
        
        ModelMessage modelMessage = new ModelMessage();
        modelMessage.getParts().addAll(anahataParts);
        return modelMessage;
    }
}