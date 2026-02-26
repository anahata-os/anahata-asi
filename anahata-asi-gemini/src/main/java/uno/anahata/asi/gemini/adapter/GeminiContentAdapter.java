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
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.Role;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.ThoughtSignature;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.AbstractToolResponse;
import uno.anahata.asi.model.tool.ToolExecutionStatus;

/**
 * An object-oriented adapter that converts a single Anahata AbstractMessage into one
 * or more native Google GenAI Content objects, injecting in-band metadata headers 
 * for improved model self-awareness.
 * <p>
 * In the V2 simplified architecture, this adapter performs a 1-to-N mapping for 
 * ModelMessages: it synthesizes the required 'model' (calls) and 'tool' (responses) 
 * API messages from a single turn-holding ModelMessage.
 * </p>
 *
 * @author anahata
 */
@RequiredArgsConstructor
public class GeminiContentAdapter {

    private final AbstractMessage anahataMessage;
    private final boolean includePruned;

    /**
     * Performs the conversion from the Anahata message to a list of Google GenAI Content objects.
     * @return A list of Content objects, or an empty list if no content is visible.
     */
    public List<Content> toGoogle() {
        Role role = anahataMessage.getRole();
        List<Content> results = new ArrayList<>();
        
        if (role == Role.USER) {
            Content userContent = toGoogleUser();
            if (userContent != null) results.add(userContent);
        } else if (role == Role.MODEL) {
            results.addAll(toGoogleModel());
        }
        
        return results;
    }

    private Content toGoogleUser() {
        Content.Builder builder = Content.builder().role("user");
        List<Part> googleParts = new ArrayList<>();

        boolean shouldCreateMetadata = anahataMessage.shouldCreateMetadata();
        if (shouldCreateMetadata) {
            googleParts.add(createMetadataPart(anahataMessage.createMetadataHeader()));
        }

        for (AbstractPart part : anahataMessage.getParts(includePruned)) {
            addPartWithMetadata(googleParts, part, shouldCreateMetadata);
        }

        if (googleParts.isEmpty()) {
            return null;
        }

        builder.parts(googleParts);
        return builder.build();
    }

    /**
     * Synthesizes the model message into potentially two API messages: 
     * 1. A 'model' role content containing text and tool calls.
     * 2. A 'tool' role content containing tool responses (if executed).
     */
    private List<Content> toGoogleModel() {
        List<Content> synthesized = new ArrayList<>();
        AbstractModelMessage<?> modelMsg = (AbstractModelMessage<?>) anahataMessage;

        // --- 1. Synthesize the MODEL role content (Calls) ---
        Content.Builder modelContentBuilder = Content.builder().role("model");
        List<Part> modelParts = new ArrayList<>();

        boolean shouldCreateMetadata = anahataMessage.shouldCreateMetadata();
        if (shouldCreateMetadata) {
            modelParts.add(createMetadataPart(anahataMessage.createMetadataHeader()));
        }

        List<AbstractPart> allParts = anahataMessage.getParts(includePruned);
        
        // Process ALL parts (Text, Blob, ToolCalls) with interleaved metadata.
        // This ensures the model has immediate context for each part's identity and status.
        for (AbstractPart part : allParts) {
            addPartWithMetadata(modelParts, part, shouldCreateMetadata);
        }

        if (!modelParts.isEmpty()) {
            synthesized.add(modelContentBuilder.parts(modelParts).build());
        }

        // --- 2. Synthesize the TOOL role content (Responses) ---
        List<AbstractToolResponse<?>> executedResponses = modelMsg.getToolCalls().stream()
                .map(AbstractToolCall::getResponse)
                .collect(Collectors.toList());

        if (!executedResponses.isEmpty()) {
            Content.Builder toolContentBuilder = Content.builder().role("tool");
            List<Part> toolParts = new ArrayList<>();

            for (AbstractToolResponse<?> response : executedResponses) {
                // Tool responses are "Pristine Clean" at the API level: 
                // No in-band metadata, only native parts.
                Part googlePart = GeminiPartAdapter.toGoogleFunctionResponsePart(response);
                if (googlePart != null) {
                    toolParts.add(googlePart);
                }
            }

            if (!toolParts.isEmpty()) {
                synthesized.add(toolContentBuilder.parts(toolParts).build());
            }
        }

        return synthesized;
    }

    /**
     * Adds an Anahata part to the Google GenAI list, automatically injecting 
     * metadata headers and handling pruned placeholder hints.
     */
    private void addPartWithMetadata(List<Part> googleParts, AbstractPart part, boolean shouldCreateMetadata) {
        boolean isEffectivelyPruned = part.isEffectivelyPruned();
        
        if (isEffectivelyPruned && !includePruned) {
            // METADATA INTERLEAVING: Even if pruned, we provide the metadata header 
            // as a "Hint" (via createMetadataHeader) to maintain semantic context 
            // of the conversation flow.
            Part.Builder placeholderBuilder = createMetadataPartBuilder(part.createMetadataHeader() + "\n[PRUNED: Content removed to save context window tokens]");
            
            if (part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
                placeholderBuilder.thoughtSignature(ts.getThoughtSignature());
            }
            
            googleParts.add(placeholderBuilder.build());
        } else {
            if (shouldCreateMetadata) {
                googleParts.add(createMetadataPart(part.createMetadataHeader()));
            }

            Part googlePart = new GeminiPartAdapter(part).toGoogle();
            if (googlePart != null) {
                part.setTokenCount(TokenizerUtils.countTokens(googlePart.toJson()));
                googleParts.add(googlePart);
            }
        }
    }

    private Part.Builder createMetadataPartBuilder(String text) {
        return Part.builder()
                .text(text)
                .thought(true);
    }

    private Part createMetadataPart(String text) {
        return createMetadataPartBuilder(text).build();
    }
}
