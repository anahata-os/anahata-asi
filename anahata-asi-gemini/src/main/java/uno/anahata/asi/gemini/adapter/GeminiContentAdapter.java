/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.Content;
import com.google.genai.types.Part;
import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.ThoughtSignature;

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

        // 1. Inject message-level metadata header if enabled.
        boolean shouldCreateMetadata = anahataMessage.shouldCreateMetadata();
        if (shouldCreateMetadata) {
            googleParts.add(Part.fromText(anahataMessage.createMetadataHeader()));
        }

        // 2. Process and convert each part.
        // We iterate over ALL parts (true) to handle pruned placeholders.
        for (AbstractPart part : anahataMessage.getParts(true)) {
            boolean isEffectivelyPruned = part.isEffectivelyPruned();
            
            if (isEffectivelyPruned && !includePruned) {
                // ROLE-PRESERVING PLACEHOLDER:
                // We send the metadata header as text, and if it's a model part, 
                // we attach the preserved thought signature to the SAME part.
                Part.Builder placeholderBuilder = Part.builder()
                    .text(part.createMetadataHeader());
                
                if (part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
                    placeholderBuilder.thoughtSignature(ts.getThoughtSignature());
                }
                
                googleParts.add(placeholderBuilder.build());
                continue;
            }

            // If not pruned, inject the part-level header before the actual content.
            if (shouldCreateMetadata) {
                googleParts.add(Part.fromText(part.createMetadataHeader()));
            }

            Part googlePart = new GeminiPartAdapter(part).toGoogle();
            if (googlePart != null) {
                // Update the part's token count using the actual JSON that will be sent.
                part.setTokenCount(TokenizerUtils.countTokens(googlePart.toJson()));
                googleParts.add(googlePart);
            }
        }

        if (googleParts.isEmpty()) {
            return null;
        }

        builder.parts(googleParts);
        return builder.build();
    }
}
