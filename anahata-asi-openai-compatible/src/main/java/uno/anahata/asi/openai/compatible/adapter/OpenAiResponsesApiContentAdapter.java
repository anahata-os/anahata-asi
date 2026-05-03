/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible.adapter;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.BlobPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.Role;
import uno.anahata.asi.agi.message.TextPart;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;
import uno.anahata.asi.openai.compatible.OpenAiResponsesModelMessage;

/**
 * A specialized content adapter for the OpenAI Responses API (/v1/responses).
 * Translates Anahata's domain model into the "Items" architecture.
 * 
 * @author anahata
 */
@Slf4j
@RequiredArgsConstructor
public class OpenAiResponsesApiContentAdapter {

    private final AbstractMessage anahataMessage;
    private final boolean includePruned;
    private final TokenizerType tokenizerType;
    
    /** Reasoning style of the target model. */
    private final uno.anahata.asi.openai.compatible.ReasoningStyle reasoningStyle;
    /** Tags used for reasoning (e.g., ["<think>", "</think>"]). */
    private final List<String> reasoningTags;

    /**
     * Translates the message into a list of Responses API "Items".
     * Supports Persistent Reasoning by injecting original items if available.
     * 
     * @return A list of ObjectNodes representing the items.
     */
    public List<ObjectNode> toItems() {
        List<ObjectNode> results = new ArrayList<>();
        
        // 1. Model turn (either persistent items or synthesized)
        if (anahataMessage instanceof OpenAiResponsesModelMessage omm && !omm.getPersistentItems().isEmpty()) {
            log.info("Injecting {} original Responses API items for persistent reasoning", omm.getPersistentItems().size());
            // Deep copy to prevent mutation during payload construction
            for (com.fasterxml.jackson.databind.JsonNode item : omm.getPersistentItems()) {
                results.add((ObjectNode) item.deepCopy());
            }
        } else if (anahataMessage.getRole() == Role.MODEL && anahataMessage instanceof AbstractModelMessage<?> modelMsg) {
            results.addAll(toModelTurnItems(modelMsg));
        } else {
            ObjectNode item = toUserOrSystemItem();
            if (item != null) results.add(item);
        }

        // 2. Tool Responses (ALWAYS append these to the turn if they exist)
        if (anahataMessage instanceof AbstractModelMessage<?> modelMsg) {
            results.addAll(toToolResponseItems(modelMsg));
        }
        
        return results;
    }

    private List<ObjectNode> toModelTurnItems(AbstractModelMessage<?> modelMsg) {
        List<ObjectNode> items = new ArrayList<>();
        // 1. Assistant Message Item (Text only)
        ObjectNode assistantItem = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        assistantItem.put("type", "message");
        assistantItem.put("role", "assistant");
        ArrayNode contentArray = assistantItem.putArray("content");
        // Handle metadata header for the message
        if (shouldInjectInbandMetadata()) {
            contentArray.addObject()
                    .put("type", "input_text")
                    .put("text", anahataMessage.createMetadataHeader() + "\n");
        }
        for (AbstractPart part : anahataMessage.getParts(true)) {
            if (part instanceof AbstractToolCall<?, ?> tc) {
                // V2 Architecture: Tool calls are separate items in the output array!
                ObjectNode callItem = SchemaProvider.OBJECT_MAPPER.createObjectNode();
                callItem.put("type", "function_call");
                callItem.put("id", tc.getId()); // Item ID
                callItem.put("call_id", tc.getId()); // Action ID
                // Split qualified tool name back into namespace and name for history synthesis
                String fullName = tc.getToolName();
                int dotIdx = fullName.lastIndexOf(".");
                if (dotIdx > 0) {
                    callItem.put("namespace", fullName.substring(0, dotIdx));
                    callItem.put("name", fullName.substring(dotIdx + 1));
                } else {
                    callItem.put("name", fullName);
                }
                try {
                    String argsJson = SchemaProvider.OBJECT_MAPPER.writeValueAsString(tc.getResponse().getExecutedArgs());
                    callItem.put("arguments", argsJson);
                } catch (Exception e) {
                    log.error("Failed to serialize executed args for tool call {}", tc.getId(), e);
                    callItem.put("arguments", "{}");
                }
                items.add(callItem);
            } else {
                addPartToContent(contentArray, null, part);
            }
        }
        if (!contentArray.isEmpty()) {
            items.add(0, assistantItem); // Text message goes first
        }
        return items;
    }
    // 1. Assistant Message Item (Text only)
    // Handle metadata header for the message
    // V2 Architecture: Tool calls are separate items in the output array!
    // Item ID
    // Action ID
    // Text message goes first
    
    private List<ObjectNode> toToolResponseItems(AbstractModelMessage<?> modelMsg) {
        List<AbstractToolResponse<?>> executedResponses = modelMsg.getToolResponses().stream().filter(tr-> includePruned || !tr.getCall().isEffectivelyPruned()).filter(Objects::nonNull).collect(Collectors.toList());
        List<ObjectNode> items = new ArrayList<>();
        for (AbstractToolResponse<?> tr : executedResponses) {
            ObjectNode responseItem = SchemaProvider.OBJECT_MAPPER.createObjectNode();
            // Fix: The Responses API requires 'function_call_output', not 'tool_call_output'.
            responseItem.put("type", "function_call_output");
            responseItem.put("call_id", tr.getCall().getId());
            String fullResponseJson = SchemaProvider.OBJECT_MAPPER.valueToTree(tr).toString();
            responseItem.put("output", fullResponseJson);
            items.add(responseItem);
        }
        return items;
    }
    // Fix: The Responses API requires 'function_call_output', not 'tool_call_output'.
    
    private ObjectNode toUserOrSystemItem() {
        ObjectNode item = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        item.put("type", "message");
        item.put("role", anahataMessage.getRole() == Role.SYSTEM ? "system" : "user");
        
        ArrayNode contentArray = item.putArray("content");
        
        if (shouldInjectInbandMetadata()) {
            contentArray.addObject()
                    .put("type", "input_text")
                    .put("text", anahataMessage.createMetadataHeader() + "\n");
        }
        
        for (AbstractPart part : anahataMessage.getParts(true)) {
            addPartToContent(contentArray, null, part);
        }
        
        return contentArray.isEmpty() ? null : item;
    }

    private void addPartToContent(ArrayNode contentArray, ArrayNode toolCalls, AbstractPart part) {
        boolean isEffectivelyPruned = part.isEffectivelyPruned();
        boolean shouldIncludeContent = !isEffectivelyPruned || includePruned;

        if (shouldInjectInbandMetadata()) {
            contentArray.addObject()
                    .put("type", "input_text")
                    .put("text", part.createMetadataHeader() + "\n");
        }

        if (shouldIncludeContent) {
            if (part instanceof TextPart tp) {
                contentArray.addObject()
                        .put("type", "input_text")
                        .put("text", tp.getText());
            } else if (part instanceof ModelTextPart mtp) {
                String text = mtp.getText();
                
                // Thought Tagging: If this is a thought part and the model uses TAGS style,
                // we wrap the text in the appropriate tags to preserve the model's "flow".
                if (mtp.isThought() && reasoningStyle == uno.anahata.asi.openai.compatible.ReasoningStyle.TAGS 
                        && reasoningTags != null && reasoningTags.size() >= 2) {
                    text = reasoningTags.get(0) + text + reasoningTags.get(1);
                }
                
                contentArray.addObject()
                        .put("type", "input_text")
                        .put("text", text);
            } else if (part instanceof AbstractToolCall<?, ?> tc && toolCalls != null) {
                ObjectNode callNode = toolCalls.addObject();
                callNode.put("id", tc.getId());
                callNode.put("type", "function");
                ObjectNode funcNode = callNode.putObject("function");
                funcNode.put("name", tc.getToolName());
                try {
                    String argsJson = SchemaProvider.OBJECT_MAPPER.writeValueAsString(tc.getResponse().getExecutedArgs());
                    funcNode.put("arguments", argsJson);
                } catch (Exception e) {
                    log.error("Failed to serialize executed args for tool call {}", tc.getId(), e);
                    funcNode.put("arguments", "{}");
                }
            } else if (part instanceof BlobPart bp) {
                if (bp.getMimeType().startsWith("image/")) {
                    ObjectNode imageNode = contentArray.addObject();
                    imageNode.put("type", "input_image");
                    ObjectNode dataNode = imageNode.putObject("input_image");
                    dataNode.put("data", Base64.getEncoder().encodeToString(bp.getData()));
                    dataNode.put("format", bp.getMimeType().substring(6));
                } else if (bp.getMimeType().startsWith("audio/")) {
                    // Responses API supports input_audio
                    ObjectNode audioNode = contentArray.addObject();
                    audioNode.put("type", "input_audio");
                    ObjectNode dataNode = audioNode.putObject("audio");
                    dataNode.put("data", Base64.getEncoder().encodeToString(bp.getData()));
                    dataNode.put("format", bp.getMimeType().substring(6)); // e.g., "wav", "mp3"
                } else {
                    // Generic input_file for all other blobs (PDF, XML, JSON, etc.)
                    String fileName = bp.getSourcePath() != null ? bp.getSourcePath().getFileName().toString() : "unnamed-file";
                    ObjectNode fileNode = contentArray.addObject();
                    fileNode.put("type", "input_file");
                    fileNode.put("filename", fileName);
                    fileNode.put("file_data", Base64.getEncoder().encodeToString(bp.getData()));
                    fileNode.put("mime_type", bp.getMimeType());
                }
            }
        }
    }

    private boolean shouldInjectInbandMetadata() {
        return anahataMessage.getAgi().getRequestConfig().isInjectInbandMetadata() && anahataMessage.shouldCreateMetadata();
    }
}
