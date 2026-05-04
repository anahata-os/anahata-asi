/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.ObjectMapper;
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
import uno.anahata.asi.agi.message.ThoughtSignature;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;

/**
 * Pure content adapter for the OpenAI Responses API, strictly translating 
 * Anahata's domain model into the "Items" architecture.
 * 
 * <p>Supports text, images, audio, and generic files.</p>
 */
@Slf4j
@RequiredArgsConstructor
public class OpenAiItemAdapter {

    private static final ObjectMapper API_MAPPER = new ObjectMapper();
    private final AbstractMessage anahataMessage;
    private final boolean includePruned;
    private final String targetModelId;

    /**
     * Translates the message into a list of Responses API "Items".
     * 
     * @return A list of ObjectNodes representing the items.
     * @throws Exception on serialization errors.
     */
    public List<ObjectNode> toItems() throws Exception {
        if (anahataMessage.getRole() == Role.MODEL && anahataMessage instanceof AbstractModelMessage<?> modelMsg) {
            List<ObjectNode> items = toModelTurnItems(modelMsg);
            items.addAll(toToolResponseItems(modelMsg));
            return items;
        } else {
            ObjectNode item = toUserOrSystemItem();
            return item != null ? List.of(item) : List.of();
        }
    }

    private List<ObjectNode> toModelTurnItems(AbstractModelMessage<?> modelMsg) throws Exception {
        List<ObjectNode> items = new ArrayList<>();
        boolean sameModel = Objects.equals(modelMsg.getModelId(), targetModelId);

        ObjectNode currentMessageItem = null;
        ArrayNode currentContentArray = null;
        String currentProviderId = null;

        for (AbstractPart part : anahataMessage.getParts(true)) {
            if (part.isEffectivelyPruned() && !includePruned) {
                continue;
            }

            // High Fidelity: Reuse captured ID if same model, otherwise generate synthetic
            String partProviderId = sameModel ? part.getProviderId() : null;
            
            if (part instanceof AbstractToolCall<?, ?> tc) {
                flushMessageItem(items, currentMessageItem);
                currentMessageItem = null;
                
                ObjectNode callItem = API_MAPPER.createObjectNode();
                callItem.put("type", "function_call");
                callItem.put("id", partProviderId != null ? partProviderId : "fc_" + tc.getSequentialId());
                callItem.put("call_id", tc.getId());
                String fullName = tc.getToolName();
                int dotIdx = fullName.lastIndexOf(".");
                if (dotIdx > 0) {
                    callItem.put("namespace", fullName.substring(0, dotIdx));
                    callItem.put("name", fullName.substring(dotIdx + 1));
                } else {
                    callItem.put("name", fullName);
                }
                String argsJson = SchemaProvider.OBJECT_MAPPER.writeValueAsString(tc.getResponse().getExecutedArgs());
                callItem.put("arguments", argsJson);
                items.add(callItem);
                
            } else if (sameModel && part instanceof ThoughtSignature ts && ts.getThoughtSignature() != null) {
                flushMessageItem(items, currentMessageItem);
                currentMessageItem = null;

                ObjectNode reasoningItem = API_MAPPER.createObjectNode();
                reasoningItem.put("type", "reasoning");
                reasoningItem.put("id", partProviderId != null ? partProviderId : "rs_" + part.getSequentialId());
                reasoningItem.put("encrypted_content", new String(ts.getThoughtSignature()));
                items.add(reasoningItem);
                
                // Ghosting: If this part's text is just our placeholder, don't add it to a following message item
                if (part instanceof TextPart tp && OpenAiModelMessage.ENCRYPTED_REASONING_PLACEHOLDER.equals(tp.getText())) {
                    continue;
                }
            } else {
                // Assistant speech (batched into message items based on original providerId grouping)
                if (currentMessageItem != null && Objects.equals(partProviderId, currentProviderId)) {
                    addPartToContent(currentContentArray, part, "assistant");
                } else {
                    flushMessageItem(items, currentMessageItem);
                    currentMessageItem = API_MAPPER.createObjectNode();
                    currentMessageItem.put("type", "message");
                    currentMessageItem.put("role", "assistant");
                    
                    // Identification Logic: use part's providerId or synthetic from its unique session ID
                    currentMessageItem.put("id", partProviderId != null ? partProviderId : "msg_" + part.getSequentialId());
                    
                    if (modelMsg instanceof OpenAiModelMessage oam && oam.getPhase() != null) {
                        currentMessageItem.put("phase", oam.getPhase());
                    }
                    currentContentArray = currentMessageItem.putArray("content");
                    currentProviderId = partProviderId;
                    addPartToContent(currentContentArray, part, "assistant");
                }
            }
        }
        flushMessageItem(items, currentMessageItem);
        return items;
    }

    private void flushMessageItem(List<ObjectNode> items, ObjectNode item) {
        if (item != null) {
            items.add(item);
        }
    }
    
    private List<ObjectNode> toToolResponseItems(AbstractModelMessage<?> modelMsg) {
        List<AbstractToolResponse<?>> executedResponses = modelMsg.getToolResponses().stream()
                .filter(tr -> includePruned || !tr.getCall().isEffectivelyPruned())
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        
        List<ObjectNode> items = new ArrayList<>();
        for (AbstractToolResponse<?> tr : executedResponses) {
            ObjectNode responseItem = API_MAPPER.createObjectNode();
            responseItem.put("type", "function_call_output");
            // Identification Logic: Use fco_ prefix + the tool call's unique session ID
            responseItem.put("id", "fco_" + tr.getCall().getSequentialId());
            responseItem.put("call_id", tr.getCall().getId());
            
            String fullResponseJson = SchemaProvider.OBJECT_MAPPER.valueToTree(tr).toString();
            responseItem.put("output", fullResponseJson);
            items.add(responseItem);
        }
        return items;
    }

    private ObjectNode toUserOrSystemItem() {
        ObjectNode item = API_MAPPER.createObjectNode();
        item.put("type", "message");
        String role = anahataMessage.getRole() == Role.SYSTEM ? "system" : "user";
        item.put("role", role);
        // Turn-level ID is sufficient for User/System messages as they map 1:1 to items
        item.put("id", "msg_" + anahataMessage.getSequentialId());
        ArrayNode contentArray = item.putArray("content");
        
        for (AbstractPart part : anahataMessage.getParts(true)) {
            addPartToContent(contentArray, part, role);
        }
        
        return contentArray.isEmpty() ? null : item;
    }

    private void addPartToContent(ArrayNode contentArray, AbstractPart part, String role) {
        if (part.isEffectivelyPruned() && !includePruned) {
            return;
        }

        // Assistant items in history must use 'output_text'. 
        // User and System items must use 'input_text'.
        String typePrefix = "assistant".equals(role) ? "output_text" : "input_text";

        if (part instanceof TextPart tp) {
            contentArray.addObject().put("type", typePrefix).put("text", tp.getText());
        } else if (part instanceof ModelTextPart mtp) {
            contentArray.addObject().put("type", typePrefix).put("text", mtp.getText());
        } else if (part instanceof BlobPart bp) {
            String mime = bp.getMimeType();
            String b64 = Base64.getEncoder().encodeToString(bp.getData());
            
            if (mime.startsWith("image/")) {
                ObjectNode node = contentArray.addObject();
                node.put("type", "input_image");
                node.putObject("input_image").put("data", b64).put("format", mime.substring(mime.indexOf("/") + 1));
            } else if (mime.startsWith("audio/")) {
                ObjectNode node = contentArray.addObject();
                node.put("type", "input_audio");
                node.putObject("audio").put("data", b64).put("format", mime.substring(mime.indexOf("/") + 1));
            } else {
                ObjectNode node = contentArray.addObject();
                node.put("type", "input_file");
                node.put("file_data", b64);
                node.put("mime_type", mime);
                if (bp.getSourcePath() != null) {
                    node.put("filename", bp.getSourcePath().getFileName().toString());
                }
            }
        }
    }
}
