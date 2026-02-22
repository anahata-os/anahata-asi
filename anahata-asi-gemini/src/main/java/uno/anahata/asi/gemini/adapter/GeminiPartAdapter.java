/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.FunctionCall;
import com.google.genai.types.FunctionResponse;
import com.google.genai.types.FunctionResponsePart;
import com.google.genai.types.Part;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.BlobPart;
import uno.anahata.asi.model.core.ModelBlobPart;
import uno.anahata.asi.model.core.ModelTextPart;
import uno.anahata.asi.model.core.TextPart;
import uno.anahata.asi.model.core.ThoughtSignature;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.AbstractToolResponse;
import uno.anahata.asi.model.tool.ToolResponseAttachment;

/**
 * An object-oriented adapter that converts a single Anahata AbstractPart into a
 * native Google GenAI Part. This class handles simple, one-to-one conversions
 * and the complex conversion of {@link AbstractToolResponse} into a single,
 * rich {@code FunctionResponse} part.
 *
 * @author anahata-ai
 */
@Slf4j
@RequiredArgsConstructor
public class GeminiPartAdapter {
    private final AbstractPart anahataPart;

    /**
     * Performs the conversion for simple, one-to-one Anahata parts.
     * @return The corresponding Google GenAI Part, or null if unsupported.
     */
    public Part toGoogle() {
        Part.Builder partBuilder = Part.builder();
        byte[] thoughtSignature = null;

        if (anahataPart instanceof ThoughtSignature) {
            thoughtSignature = ((ThoughtSignature) anahataPart).getThoughtSignature();
        }
        if (thoughtSignature != null) {
            partBuilder.thoughtSignature(thoughtSignature);
        }

        if (anahataPart instanceof ModelTextPart) {
            ModelTextPart modelText = (ModelTextPart) anahataPart;
            partBuilder.text(modelText.getText());
            partBuilder.thought(modelText.isThought());
            return partBuilder.build();
        }
        if (anahataPart instanceof TextPart) {
            return Part.fromText(((TextPart) anahataPart).getText());
        }
        if (anahataPart instanceof ModelBlobPart) {
            ModelBlobPart modelBlob = (ModelBlobPart) anahataPart;
            partBuilder.inlineData(com.google.genai.types.Blob.builder()
                .data(modelBlob.getData())
                .mimeType(modelBlob.getMimeType())
                .build());
            return partBuilder.build();
        }
        if (anahataPart instanceof BlobPart) {
            BlobPart blob = (BlobPart) anahataPart;
            return Part.builder()
                .inlineData(com.google.genai.types.Blob.builder()
                    .data(blob.getData())
                    .mimeType(blob.getMimeType())
                    .build())
                .build();
        }
        if (anahataPart instanceof AbstractToolCall) {
            AbstractToolCall toolCall = (AbstractToolCall) anahataPart;
            
            Map<String, Object> safeArgs = (Map<String, Object>) JacksonUtils.toJsonPrimitives(toolCall.getEffectiveArgs());
            
            FunctionCall.Builder fcBuilder = FunctionCall.builder()
                .name(toolCall.getToolName())
                .args(safeArgs)
                .id(toolCall.getId());
            
            return partBuilder.functionCall(fcBuilder.build()).build();
        }
        
        log.warn("Unsupported Anahata Part type for Google conversion, skipping: {}", anahataPart.getClass().getSimpleName());
        return null;
    }

    /**
     * Converts an AbstractToolResponse into the main Google FunctionResponse Part,
     * including attachments.
     * 
     * @param anahataResponse The rich response POJO.
     * @return The corresponding Google FunctionResponse Part.
     */
    public static Part toGoogleFunctionResponsePart(AbstractToolResponse<?> anahataResponse) {
        // FINAL GATE: We send the ENTIRE rich response object (status, result, errors, etc.) 
        // as the JSON response. This gives the model full visibility into the execution.
        Map<String, Object> responseMap = (Map<String, Object>) JacksonUtils.toJsonPrimitives(anahataResponse);
        
        // 2. Convert attachments to FunctionResponsePart
        List<FunctionResponsePart> attachmentParts = new ArrayList<>();
        for (ToolResponseAttachment attachment : anahataResponse.getAttachments()) {
            attachmentParts.add(toGoogleAttachmentPart(attachment));
        }

        // 3. Build the FunctionResponse
        FunctionResponse fr = FunctionResponse.builder()
            .name(anahataResponse.getCall().getToolName())
            .id(anahataResponse.getCall().getId())
            .response(responseMap)
            .parts(attachmentParts)
            .build();
        
        return Part.builder().functionResponse(fr).build();
    }
    
    private static FunctionResponsePart toGoogleAttachmentPart(ToolResponseAttachment attachment) {
        return FunctionResponsePart.fromBytes(attachment.getData(), attachment.getMimeType());
    }
}
