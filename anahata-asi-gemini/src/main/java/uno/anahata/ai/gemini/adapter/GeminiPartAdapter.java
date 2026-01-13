/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.gemini.adapter;

import com.google.genai.types.FunctionCall;
import com.google.genai.types.FunctionResponse;
import com.google.genai.types.FunctionResponsePart;
import com.google.genai.types.Part;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import uno.anahata.ai.internal.JacksonUtils;
import uno.anahata.ai.model.core.AbstractPart;
import uno.anahata.ai.model.core.BlobPart;
import uno.anahata.ai.model.core.ModelBlobPart;
import uno.anahata.ai.model.core.ModelTextPart;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.core.ThoughtSignature;
import uno.anahata.ai.model.tool.AbstractToolCall;
import uno.anahata.ai.model.tool.AbstractToolResponse;
import uno.anahata.ai.model.tool.ToolResponseAttachment;

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
            // thoughtSignature is already handled above
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
            // thoughtSignature is already handled above
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
        if (anahataPart instanceof AbstractToolResponse) {
            return toGoogleFunctionResponsePart();
        }
        if (anahataPart instanceof AbstractToolCall) {
            AbstractToolCall toolCall = (AbstractToolCall) anahataPart;
            FunctionCall.Builder fcBuilder = FunctionCall.builder()
                .name(toolCall.getToolName())
                .args(toolCall.getArgs())
                .id(toolCall.getId());
            
            
            // thoughtSignature is already handled above
            return partBuilder.functionCall(fcBuilder.build()).build();
        }
        
        log.warn("Unsupported Anahata Part type for Google conversion, skipping: {}", anahataPart.getClass().getSimpleName());
        return null;
    }

    /**
     * Converts an AbstractToolResponse into the main Google FunctionResponse Part,
     * including attachments.
     * @return The corresponding Google FunctionResponse Part.
     */
    private Part toGoogleFunctionResponsePart() {
        AbstractToolResponse<?> anahataResponse = (AbstractToolResponse) anahataPart;
        Map<String, Object> responseMap;
        
        /*
        // 1. Determine the JSON payload (output or error)
        if (StringUtils.isNotBlank(anahataResponse.getError())) {
            // If there's an error, the response map must contain the "error" key.
            responseMap = JacksonUtils.convertObjectToMap("error", anahataResponse.getError());
        } else {
            // Otherwise, it was successful, and the map must contain the "output" key.
            responseMap = JacksonUtils.convertObjectToMap("output", anahataResponse.getResult());
        }
        */
        responseMap = JacksonUtils.convertObjectToMap(null, anahataResponse);
        //log.info("responseMap " + responseMap);
        Map resultMap = JacksonUtils.convertObjectToMap("result", anahataResponse.getResult());
        //log.info("resultMap" + resultMap);
        
        responseMap.putAll(resultMap);
        
        //responseMap.put("result", );
        
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
            .parts(attachmentParts) // Attachments are nested here
            .build();
        
        // thoughtSignature for AbstractToolResponse is handled by the outer toGoogle() method
        return Part.builder().functionResponse(fr).build();
    }
    
    /**
     * Converts a ToolResponseAttachment into a Google FunctionResponsePart.
     * @param attachment The attachment to convert.
     * @return The corresponding Google FunctionResponsePart.
     */
    private static FunctionResponsePart toGoogleAttachmentPart(ToolResponseAttachment attachment) {
        return FunctionResponsePart.fromBytes(attachment.getData(), attachment.getMimeType());
    }
}