/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.Content;
import com.google.genai.types.Part;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import uno.anahata.asi.internal.TokenizerUtils;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.Role;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.ThoughtSignature;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.AbstractToolResponse;

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
        Role role = anahataMessage.getRole();
        
        if (role == Role.USER) {
            return toGoogleUser();
        } else if (role == Role.MODEL) {
            return toGoogleModel();
        } else if (role == Role.TOOL) {
            return toGoogleTool();
        }
        
        return null;
    }

    private Content toGoogleUser() {
        Content.Builder builder = Content.builder().role("user");
        List<Part> googleParts = new ArrayList<>();

        boolean shouldCreateMetadata = anahataMessage.shouldCreateMetadata();
        if (shouldCreateMetadata) {
            googleParts.add(Part.fromText(anahataMessage.createMetadataHeader()));
        }

        for (AbstractPart part : anahataMessage.getParts(true)) {
            addPartWithMetadata(googleParts, part, shouldCreateMetadata);
        }

        if (googleParts.isEmpty()) {
            return null;
        }

        builder.parts(googleParts);
        return builder.build();
    }

    private Content toGoogleModel() {
        Content.Builder builder = Content.builder().role("model");
        List<Part> googleParts = new ArrayList<>();

        boolean shouldCreateMetadata = anahataMessage.shouldCreateMetadata();
        if (shouldCreateMetadata) {
            googleParts.add(Part.fromText(anahataMessage.createMetadataHeader()));
        }

        List<AbstractPart> allParts = anahataMessage.getParts(true);
        
        // 1. Process Text Parts (with individual metadata)
        for (AbstractPart part : allParts) {
            if (part instanceof TextPart) {
                addPartWithMetadata(googleParts, part, shouldCreateMetadata);
            }
        }

        // 2. Aggregated Tool Metadata
        List<AbstractToolCall> toolCalls = allParts.stream()
                .filter(p -> p instanceof AbstractToolCall)
                .map(p -> (AbstractToolCall) p)
                .collect(Collectors.toList());

        if (!toolCalls.isEmpty() && shouldCreateMetadata) {
            StringBuilder sb = new StringBuilder("--- Aggregated Tool Metadata ---\n");
            // Include Tool Message metadata if available
            AbstractMessage toolMsg = toolCalls.get(0).getResponse().getMessage();
            for (AbstractToolCall tc : toolCalls) {
                sb.append(tc.createMetadataHeader()).append("\n");
            }
            if (toolMsg != null) {
                //sb.append(toolMsg.createMetadataHeader()).append("\n");
            }
            googleParts.add(Part.fromText(sb.toString().trim()));
        }

        // 3. Native Tool Calls (only if not effectively pruned)
        for (AbstractToolCall tc : toolCalls) {
            if (!tc.isEffectivelyPruned() || includePruned) {
                Part googlePart = new GeminiPartAdapter(tc).toGoogle();
                if (googlePart != null) {
                    tc.setTokenCount(TokenizerUtils.countTokens(googlePart.toJson()));
                    googleParts.add(googlePart);
                }
            }
        }

        if (googleParts.isEmpty()) {
            return null;
        }

        builder.parts(googleParts);
        return builder.build();
    }

    private Content toGoogleTool() {
        Content.Builder builder = Content.builder().role("tool");
        List<Part> googleParts = new ArrayList<>();

        // Tool messages are "Pristine Clean": No metadata, only visible native responses.
        for (AbstractPart part : anahataMessage.getParts(includePruned)) {
            if (part instanceof AbstractToolResponse) {
                Part googlePart = new GeminiPartAdapter(part).toGoogle();
                if (googlePart != null) {
                    part.setTokenCount(TokenizerUtils.countTokens(googlePart.toJson()));
                    googleParts.add(googlePart);
                }
            }
        }

        if (googleParts.isEmpty()) {
            return null;
        }

        builder.parts(googleParts);
        return builder.build();
    }

    private void addPartWithMetadata(List<Part> googleParts, AbstractPart part, boolean shouldCreateMetadata) {
        boolean isEffectivelyPruned = part.isEffectivelyPruned();
        
        if (isEffectivelyPruned && !includePruned) {
            // ROLE-PRESERVING PLACEHOLDER:
            // We send the metadata header as text, and if it's a model part, 
            // we attach the preserved thought signature to the SAME part.
            Part.Builder placeholderBuilder = Part.builder()
                .text(part.createMetadataHeader() + "\n[PRUNED PLACEHOLDER: Metadata preserved for context awareness]");
            
            if (part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
                placeholderBuilder.thoughtSignature(ts.getThoughtSignature());
            }
            
            googleParts.add(placeholderBuilder.build());
        } else {
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
    }
}
