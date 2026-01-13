/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.Content;
import com.google.genai.types.Part;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import uno.anahata.ai.model.core.AbstractMessage;

/**
 * An object-oriented adapter that converts a single Anahata AbstractMessage into a
 * native Google GenAI Content object.
 *
 * @author anahata-ai
 */
@RequiredArgsConstructor
public class GeminiContentAdapter {

    private final AbstractMessage anahataMessage;
    private final boolean includePruned;

    /**
     * Performs the conversion from the Anahata message to a Google GenAI Content object.
     * @return The corresponding Content object, or null if the message has no visible parts.
     */
    public Content toGoogle() {
        Content.Builder builder = Content.builder()
            .role(anahataMessage.getRole().name().toLowerCase());

        // The GeminiPartAdapter now handles all part types, including the complex
        // AbstractToolResponse (which includes attachments).
        List<Part> googleParts = anahataMessage.getParts(includePruned).stream()
            .map(part -> new GeminiPartAdapter(part).toGoogle())
            .filter(Objects::nonNull)
            .collect(Collectors.toList());

        if (googleParts.isEmpty()) {
            return null; // Don't create a Content object if there are no visible parts
        }

        builder.parts(googleParts);
        return builder.build();
    }
}