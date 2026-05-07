/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.provider.AiProviderException;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.BlobPart;
import uno.anahata.asi.agi.message.ModelBlobPart;
import uno.anahata.asi.agi.message.ModelCodeExecutionCallPart;
import uno.anahata.asi.agi.message.ModelCodeExecutionResultPart;
import uno.anahata.asi.agi.message.ModelSearchCallPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.RagMessage;
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
 * <p>
 * Supports text, images, and generic files (including audio fallback).</p>
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
        if (anahataMessage instanceof RagMessage rag) {
            ObjectNode item = toRagItem();
            return item != null ? List.of(item) : List.of();
        } else if (anahataMessage.getRole() == Role.MODEL && anahataMessage instanceof AbstractModelMessage<?> modelMsg) {
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

        // Map to track and reuse Code Interpreter items (Call -> JSON Node)
        Map<ModelCodeExecutionCallPart, ObjectNode> ciNodes = new java.util.LinkedHashMap<>();

        for (AbstractPart part : modelMsg.getParts(includePruned)) {
            String partProviderId = sameModel ? part.getProviderId() : null;

            // 1. OpenAI-Specific Encrypted Reasoning Item
            if (sameModel && part instanceof ModelTextPart mtp && mtp.isThought() 
                    && OpenAiModelMessage.ENCRYPTED_REASONING_PLACEHOLDER.equals(mtp.getText())
                    && mtp.getThoughtSignature() != null) {
                
                flushMessageItem(items, currentMessageItem);
                currentMessageItem = null;
                items.add(createReasoningNode(part, (ThoughtSignature)part, sameModel));
                continue; // Skip: already handled as a top-level reasoning item
            }

            // 2. Code Interpreter Components (Call, Result, or Blob with parentCall)
            ModelCodeExecutionCallPart ciRoot = getCiRoot(part);
            if (ciRoot != null) {
                flushMessageItem(items, currentMessageItem);
                currentMessageItem = null;
                ObjectNode ciNode = getOrCreateCiNode(ciNodes, ciRoot, items, sameModel);
                updateCiNode(ciNode, part);
                continue;
            }

            // 3. Independent Items (Functions, Search)
            if (part instanceof AbstractToolCall<?, ?> tc) {
                flushMessageItem(items, currentMessageItem);
                currentMessageItem = null;
                items.add(createFunctionCallNode(tc, sameModel));
            } else if (part instanceof ModelSearchCallPart mscp) {
                flushMessageItem(items, currentMessageItem);
                currentMessageItem = null;
                items.add(createWebSearchCallNode(mscp, sameModel));
            } else {
                // 4. Message Content (Assistant Speech, Visible Thoughts, Blobs without parentCall)
                if (currentMessageItem != null && Objects.equals(partProviderId, currentProviderId)) {
                    addPartToContentArray(currentContentArray, part, "assistant");
                } else {
                    flushMessageItem(items, currentMessageItem);
                    currentMessageItem = API_MAPPER.createObjectNode();
                    currentMessageItem.put("type", "message");
                    currentMessageItem.put("role", "assistant");
                    currentMessageItem.put("id", partProviderId != null ? partProviderId : "msg_" + part.getSequentialId());

                    if (modelMsg instanceof OpenAiModelMessage oam && oam.getPhase() != null) {
                        currentMessageItem.put("phase", oam.getPhase());
                    }
                    currentContentArray = currentMessageItem.putArray("content");
                    currentProviderId = partProviderId;
                    addPartToContentArray(currentContentArray, part, "assistant");
                }
            }
        }
        flushMessageItem(items, currentMessageItem);
        return items;
    }

    private ModelCodeExecutionCallPart getCiRoot(AbstractPart part) {
        if (part instanceof ModelCodeExecutionCallPart mccp) {
            return mccp;
        }
        if (part instanceof ModelCodeExecutionResultPart res) {
            return res.getParentCall();
        }
        if (part instanceof ModelBlobPart blob) {
            return blob.getParentCall();
        }
        return null;
    }

    private ObjectNode getOrCreateCiNode(Map<ModelCodeExecutionCallPart, ObjectNode> ciNodes, ModelCodeExecutionCallPart root, List<ObjectNode> items, boolean sameModel) {
        return ciNodes.computeIfAbsent(root, k -> {
            ObjectNode node = API_MAPPER.createObjectNode();
            node.put("type", "code_interpreter_call");
            node.put("status", "completed");
            String id = (sameModel && k.getProviderId() != null) ? k.getProviderId() : "ci_" + k.getSequentialId();
            node.put("id", id);
            node.put("code", "# Source code removed or pruned to save tokens");
            node.putArray("outputs");
            items.add(node);
            return node;
        });
    }

    private void updateCiNode(ObjectNode ciNode, AbstractPart part) {
        if (part instanceof ModelCodeExecutionCallPart mccp) {
            ciNode.put("code", mccp.getText());
        } else {
            ArrayNode outputs = (ArrayNode) ciNode.get("outputs");
            if (part instanceof ModelCodeExecutionResultPart res) {
                outputs.addObject().put("type", "logs").put("logs", res.getText());
            } else if (part instanceof ModelBlobPart mbp && mbp.getMimeType().startsWith("image/")) {
                ObjectNode imgNode = outputs.addObject();
                imgNode.put("type", "image");
                imgNode.put("url", "data:" + mbp.getMimeType() + ";base64," + Base64.getEncoder().encodeToString(mbp.getData()));
            }
        }
    }

    private ObjectNode createFunctionCallNode(AbstractToolCall<?, ?> tc, boolean sameModel) throws Exception {
        ObjectNode callItem = API_MAPPER.createObjectNode();
        callItem.put("type", "function_call");
        String id = (sameModel && tc.getProviderId() != null) ? tc.getProviderId() : "fc_" + tc.getSequentialId();
        callItem.put("id", id);
        // Robustness: if no call_id exists (e.g. legacy or cross-provider), use the synthetic ID
        callItem.put("call_id", tc.getId() != null ? tc.getId() : id);
        String fullName = tc.getToolName();
        int dotIdx = fullName.lastIndexOf(".");
        if (dotIdx > 0) {
            callItem.put("namespace", fullName.substring(0, dotIdx));
            callItem.put("name", fullName.substring(dotIdx + 1));
        } else {
            callItem.put("name", fullName);
        }
        String argsJson = SchemaProvider.OBJECT_MAPPER.writeValueAsString(tc.getRawArgs());
        callItem.put("arguments", argsJson);
        return callItem;
    }

    private ObjectNode createWebSearchCallNode(ModelSearchCallPart mscp, boolean sameModel) {
        ObjectNode searchCall = API_MAPPER.createObjectNode();
        searchCall.put("type", "web_search_call");
        String id = (sameModel && mscp.getProviderId() != null) ? mscp.getProviderId() : "ws_" + mscp.getSequentialId();
        searchCall.put("id", id);
        searchCall.putObject("action").put("query", mscp.getQueries().isEmpty() ? "" : mscp.getQueries().get(0));
        return searchCall;
    }

    private ObjectNode createReasoningNode(AbstractPart part, ThoughtSignature ts, boolean sameModel) {
        ObjectNode reasoningItem = API_MAPPER.createObjectNode();
        reasoningItem.put("type", "reasoning");
        String id = (sameModel && part.getProviderId() != null) ? part.getProviderId() : "rs_" + part.getSequentialId();
        reasoningItem.put("id", id);
        reasoningItem.put("encrypted_content", new String(ts.getThoughtSignature()));
        return reasoningItem;
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
            // Always use fco_ prefix + the tool call's unique session ID
            responseItem.put("id", "fco_" + tr.getCall().getSequentialId());
            // Robustness fallback: use the synthetic ID if call_id is null
            String callId = tr.getCall().getId() != null ? tr.getCall().getId() : "fc_" + tr.getCall().getSequentialId();
            responseItem.put("call_id", callId);

            String fullResponseJson = SchemaProvider.OBJECT_MAPPER.valueToTree(tr).toString();
            responseItem.put("output", fullResponseJson);
            items.add(responseItem);

            // Multimodal Tool Support
            if (!tr.getAttachments().isEmpty()) {
                ObjectNode attachmentItem = API_MAPPER.createObjectNode();
                attachmentItem.put("type", "message");
                attachmentItem.put("role", "developer");
                attachmentItem.put("id", "msg_fco_att_" + tr.getCall().getSequentialId());
                ArrayNode contentArray = attachmentItem.putArray("content");

                contentArray.addObject()
                        .put("type", "input_text")
                        .put("text", "The following are multimodal attachments generated by the tool '" + tr.getToolName() + "':");

                for (var att : tr.getAttachments()) {
                    addBlobToContentArray(contentArray, att.getMimeType(), att.getData());
                }
                items.add(attachmentItem);
            }
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
            addPartToContentArray(contentArray, part, role);
        }

        return contentArray.isEmpty() ? null : item;
    }

    private ObjectNode toRagItem() {
        ObjectNode item = API_MAPPER.createObjectNode();
        item.put("type", "message");
        item.put("role", "user");
        item.put("id", "msg_rag");
        ArrayNode contentArray = item.putArray("content");

        for (AbstractPart part : anahataMessage.getParts(true)) {
            addPartToContentArray(contentArray, part, "user");
        }

        return contentArray.isEmpty() ? null : item;
    }

    private void addPartToContentArray(ArrayNode contentArray, AbstractPart part, String role) {
        if (part.isEffectivelyPruned() && !includePruned) {
            return;
        }

        if (part instanceof TextPart tp) {
            // High Fidelity: Differentiate assistant and user/system text.
            // Note: 'reasoning_content' is not supported in 'input' history items for Responses API, 
            // so assistant thoughts are sent as standard 'output_text'.
            String type = "assistant".equals(role) ? "output_text" : "input_text";
            contentArray.addObject().put("type", type).put("text", tp.getText());
        } else if (part instanceof BlobPart bp) {
            addBlobToContentArray(contentArray, bp.getMimeType(), bp.getData());
        }
    }

    private void addBlobToContentArray(ArrayNode contentArray, String mimeType, byte[] data) {
        String b64 = Base64.getEncoder().encodeToString(data);
        String format = mimeType.contains("/") ? mimeType.substring(mimeType.indexOf("/") + 1) : mimeType;

        if (mimeType.startsWith("image/")) {
            // OpenAI Responses API: Images in message content use 'image_url' with data URI format
            contentArray.addObject()
                    .put("type", "input_image")
                    .put("image_url", "data:" + mimeType + ";base64," + b64);
        }
        if (mimeType.startsWith("audio/")) {
            // OpenAI Responses API: Audio data is sent base64 encoded with a specified format
            // Note: 'input_audio' is currently not supported in the Responses API, 
            contentArray.addObject()
                    .put("type", "input_audio")
                    .putObject("input_audio")
                    .put("audio", b64)
                    .put("format", format);
        } else {
            throw new AiProviderException("Image and audio only today. Will not add this " + mimeType + " blob " + data.length + " to: " + contentArray);

             // OpenAI Responses API: Generic files use an 'input_file' nested object.
            // so we fall back to 'input_file' for all non-image blobs (including audio).
            //with input_file 
            //This doesn't work, to do this, you have to upload the file first using the files api, and then reference the file id.
            //leave it commented out, don't delete
            /*
             ObjectNode filePart = contentArray.addObject();
             filePart.put("type", "input_file");
             filePart.putObject("input_file")
                    .put("data", b64)
                    .put("format", format);
             */
        }
    }
}
