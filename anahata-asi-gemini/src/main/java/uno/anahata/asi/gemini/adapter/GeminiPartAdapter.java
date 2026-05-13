/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.gemini.adapter;

import com.google.genai.types.CodeExecutionResult;
import com.google.genai.types.ExecutableCode;
import com.google.genai.types.FunctionCall;
import com.google.genai.types.FunctionResponse;
import com.google.genai.types.FunctionResponsePart;
import com.google.genai.types.Language;
import com.google.genai.types.Outcome;
import com.google.genai.types.Part;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.internal.JacksonUtils;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.BlobPart;
import uno.anahata.asi.agi.message.ModelBlobPart;
import uno.anahata.asi.agi.message.code.HostedCodeExecutionCallPart;
import uno.anahata.asi.agi.message.code.HostedCodeExecutionResultPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.TextPart;
import uno.anahata.asi.agi.message.ThoughtSignature;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;
import uno.anahata.asi.agi.tool.ToolResponseAttachment;

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
    private final boolean includeThoughtSignature;

    public GeminiPartAdapter(AbstractPart anahataPart) {
        this(anahataPart, true);
    }

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
        if (includeThoughtSignature && thoughtSignature != null) {
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
        if (anahataPart instanceof HostedCodeExecutionCallPart mccp) {
            Language.Known l = mccp.getLanguage().toLowerCase().equals("python") ? Language.Known.PYTHON : Language.Known.LANGUAGE_UNSPECIFIED;
            ExecutableCode.Builder ecb = ExecutableCode.builder()
                .code(mccp.getText())
                .language(l); 
            return partBuilder.executableCode(ecb.build()).build();
        }
        if (anahataPart instanceof HostedCodeExecutionResultPart mcop) {
            //map anahata outcomes to genai outcomes
            Outcome.Known o = Outcome.Known.OUTCOME_UNSPECIFIED;
            
            if (null != mcop.getOutcome()) switch (mcop.getOutcome()) {
                case FAILED -> o = Outcome.Known.OUTCOME_FAILED;
                case TIMEOUT -> o = Outcome.Known.OUTCOME_DEADLINE_EXCEEDED;
                case OK -> o = Outcome.Known.OUTCOME_OK;
                default -> {
                }
            }
            CodeExecutionResult.Builder cerb = CodeExecutionResult.builder()
                .outcome(o)
                .output(mcop.getText());
            return partBuilder.codeExecutionResult(cerb.build()).build();
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
                .args(safeArgs)/*
                .id(toolCall.getId())*/;
            
            if (toolCall.getId() != null) {
                fcBuilder.id(toolCall.getId());
            }
            
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
        FunctionResponse.Builder frb = FunctionResponse.builder()
            .name(anahataResponse.getCall().getToolName())
            .response(responseMap)
            .parts(attachmentParts);
        
        if (anahataResponse.getCall().getId() != null) {
            frb.id(anahataResponse.getCall().getId());
        }
        
        return Part.builder().functionResponse(frb.build()).build();
    }
    
    private static FunctionResponsePart toGoogleAttachmentPart(ToolResponseAttachment attachment) {
        return FunctionResponsePart.fromBytes(attachment.getData(), attachment.getMimeType());
    }
}
