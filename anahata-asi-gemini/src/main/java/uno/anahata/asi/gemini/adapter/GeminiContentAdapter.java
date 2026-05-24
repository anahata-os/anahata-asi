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
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.Role;
import uno.anahata.asi.agi.message.ThoughtSignature;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;

/**
 * An object-oriented adapter that converts a single Anahata AbstractMessage
 * into one or more native Google GenAI Content objects, injecting in-band
 * metadata headers for improved model self-awareness.
 * <p>
 * In the V2 simplified architecture, this adapter performs a 1-to-N mapping for
 * ModelMessages: it synthesizes the required 'model' (calls) and 'tool'
 * (responses) API messages from a single turn-holding ModelMessage.
 * </p>
 *
 * @author anahata
 */
@RequiredArgsConstructor
public class GeminiContentAdapter {

    /**
     * The Anahata message instance to be converted.
     */
    private final AbstractMessage anahataMessage;
    
    /**
     * Whether to include parts marked as effectively pruned in the output payload.
     */
    private final boolean includePruned;
    
    /**
     * The UUID of the provider for which the payload is being prepared. 
     * Used to ensure thought signatures are only replayed to the originating provider.
     */
    private final String targetProviderUuid;

    /**
     * Performs the conversion from the Anahata message to a list of Google
     * GenAI Content objects.
     *
     * @return A list of Content objects, or an empty list if no content is
     * visible.
     */
    public List<Content> toGoogle() {
        Role role = anahataMessage.getRole();
        List<Content> results = new ArrayList<>();

        if (role == Role.USER) {
            Content userContent = toGoogleUser();
            if (userContent != null) {
                results.add(userContent);
            }
        } else if (role == Role.MODEL) {
            results.addAll(toGoogleModel());
        }

        return results;
    }

    /**
     * Determines if in-band metadata (headers) should be injected into the 
     * parts based on session configuration and message state.
     * @return True if injection is enabled and allowed for this message.
     */
    private boolean shouldInjectInbandMetadata() {
        return anahataMessage.getAgi().getRequestConfig().isInjectInbandMetadata() && anahataMessage.shouldCreateMetadata();
    }

    /**
     * Converts a user-role message into a native Google Content object.
     * <p>Implementation details: Injects a turn-level metadata header and then 
     * processes individual parts with interleaved part-level metadata.</p>
     * @return The user content or null if empty.
     */
    private Content toGoogleUser() {
        Content.Builder builder = Content.builder().role("user");
        List<Part> googleParts = new ArrayList<>();

        if (shouldInjectInbandMetadata()) {
            googleParts.add(createMetadataPart(anahataMessage.createMetadataHeader()));
        }

        // We iterate over ALL parts (including pruned) because addPartWithMetadata 
        // handles the intelligent hint generation.
        for (AbstractPart part : anahataMessage.getParts(true)) {
            addPartWithMetadata(googleParts, part);
        }

        if (googleParts.isEmpty()) {
            return null;
        }

        builder.parts(googleParts);
        return builder.build();
    }

    /**
     * Synthesizes a model-role message into multiple API messages.
     * <p>Implementation details: Produces a 'model' role message containing the 
     * model's generated content (text/calls) and an optional 'tool' role message 
     * containing the execution results of any triggered tools.</p>
     * @return A list of synthesized Content objects.
     */
    private List<Content> toGoogleModel() {
        List<Content> synthesized = new ArrayList<>();
        AbstractModelMessage<?> modelMsg = (AbstractModelMessage<?>) anahataMessage;

        // --- 1. Synthesize the MODEL role content (Calls) ---
        Content.Builder modelContentBuilder = Content.builder().role("model");
        List<Part> modelParts = new ArrayList<>();

        if (shouldInjectInbandMetadata()) {
            modelParts.add(createMetadataPart(anahataMessage.createMetadataHeader()));
        }

        // Process ALL parts (Text, Blob, ToolCalls) with interleaved metadata.
        // This ensures the model has immediate context for each part's identity and status.
        for (AbstractPart part : anahataMessage.getParts(true)) {
            addPartWithMetadata(modelParts, part);
        }

        if (!modelParts.isEmpty()) {
            synthesized.add(modelContentBuilder.parts(modelParts).build());
        }

        // --- 2. Synthesize the TOOL role content (Responses) ---
        // CRITICAL: We only include tool responses if the corresponding tool call 
        // is visible (not effectively pruned) or if includePruned is explicitly set.
        List<AbstractToolResponse<?>> executedResponses = modelMsg.getToolCalls().stream()
                .filter(tc -> includePruned || !tc.isEffectivelyPruned())
                .map(AbstractToolCall::getResponse)
                .filter(Objects::nonNull)
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
     * Adds a single Anahata part to the Google parts list, handling 
     * metadata injection and pruned content replacement.
     * @param googleParts The target list for native parts.
     * @param part        The Anahata part to process.
     */
    private void addPartWithMetadata(List<Part> googleParts, AbstractPart part) {
        boolean isEffectivelyPruned = part.isEffectivelyPruned();
        boolean shouldIncludeContent = !isEffectivelyPruned || includePruned;

        if (shouldInjectInbandMetadata()) {
            // METADATA INTERLEAVING: The metadata header is always created if the
            // message allows it. The AbstractPart itself now handles the
            // rich "Ghost" hint when effectively pruned.
            Part.Builder headerBuilder = createMetadataPartBuilder(part.createMetadataHeader());

            // If we are NOT going to include the actual part (because it's pruned),
            // the metadata header must take responsibility for carrying the
            // thought signature if one exists.
            if (!shouldIncludeContent && part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
                //if it has been prunned already, its probably many turns old?
                //does it really make sense?
                //headerBuilder.thoughtSignature(ts.getThoughtSignature());
            }

            googleParts.add(headerBuilder.build());
        }

        if (shouldIncludeContent) {
            boolean includeThoughtSignature = true;
            if (anahataMessage instanceof AbstractModelMessage<?> amm) {
                // If we know the source provider and it's different from the current one,
                // we don't replay the signature as it might be invalid/incompatible.
                if (amm.getProviderUuid() != null && targetProviderUuid != null) {
                    includeThoughtSignature = Objects.equals(amm.getProviderUuid(), targetProviderUuid);
                }
            }

            Part googlePart = new GeminiPartAdapter(part, includeThoughtSignature).toGoogle();
            if (googlePart != null) {
                //part.setTokenCount(TokenizerUtils.countTokens(googlePart.toJson(), anahataMessage.getActiveTokenizer()));
                googleParts.add(googlePart);
            }
        }
    }

    /**
     * Creates a builder for a metadata part containing the specified text.
     * @param text The metadata header text.
     * @return A Part.Builder.
     */
    private Part.Builder createMetadataPartBuilder(String text) {
        return Part.builder()
                .text(text);
        //.thought(true);
    }

    /**
     * Creates a native metadata part.
     * @param text The metadata header text.
     * @return A native Part.
     */
    private Part createMetadataPart(String text) {
        return createMetadataPartBuilder(text).build();
    }
}
