/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.Content;
import com.google.genai.types.Part;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.Role;

/**
 * An object-oriented adapter that converts a single Anahata AbstractMessage into a
 * native Google GenAI Content object, injecting in-band metadata headers for
 * improved model self-awareness.
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

        List<Part> googleParts = new ArrayList<>();

        // 1. Inject message-level metadata header if enabled for this message type.
        boolean shouldCreateMetadata = anahataMessage.shouldCreateMetadata();
        if (shouldCreateMetadata) {
            googleParts.add(Part.fromText(anahataMessage.createMetadataHeader()));
        }

        // 2. Process and convert each part, injecting part-level headers where appropriate.
        // We iterate over ALL parts (true) to handle pruned placeholders.
        for (AbstractPart part : anahataMessage.getParts(true)) {
            boolean isEffectivelyPruned = part.isEffectivelyPruned();
            
            if (shouldCreateMetadata) {
                googleParts.add(Part.fromText(part.createMetadataHeader()));
            }

            if (isEffectivelyPruned && !includePruned) {
                // If pruned and not including, the header above serves as the placeholder.
                continue;
            }

            Part googlePart = new GeminiPartAdapter(part).toGoogle();
            if (googlePart != null) {
                // Update the part's token count using the actual JSON that will be sent.
                part.setTokenCount(TokenizerUtils.countTokens(googlePart.toJson()));
                googleParts.add(googlePart);
            }
        }

        if (googleParts.isEmpty()) {
            return null; // Don't create a Content object if there are no visible parts
        }

        builder.parts(googleParts);
        return builder.build();
    }
}
