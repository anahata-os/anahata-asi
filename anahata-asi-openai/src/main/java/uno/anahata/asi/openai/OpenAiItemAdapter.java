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
import uno.anahata.asi.agi.provider.AiProviderException;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.BlobPart;
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
                // Fidelity: Replay original rawArgs to the model
                String argsJson = SchemaProvider.OBJECT_MAPPER.writeValueAsString(tc.getRawArgs());
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
                    addPartToContentArray(currentContentArray, part, "assistant");
                } else {
                    flushMessageItem(items, currentMessageItem);
                    currentMessageItem = API_MAPPER.createObjectNode();
                    currentMessageItem.put("type", "message");
                    currentMessageItem.put("role", "assistant");

                    // Identification Logic: use part's providerId OR part's sequential ID if model changed
                    if (partProviderId != null) {
                        currentMessageItem.put("id", partProviderId);
                    } else {
                        // If model changed, we use the part ID to ensure uniqueness in interleaved turns
                        currentMessageItem.put("id", "msg_" + part.getSequentialId());
                    }

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
            // Identification Logic: Always use fco_ prefix + the tool call's unique session ID
            responseItem.put("id", "fco_" + tr.getCall().getSequentialId());
            responseItem.put("call_id", tr.getCall().getId());

            String fullResponseJson = SchemaProvider.OBJECT_MAPPER.valueToTree(tr).toString();
            responseItem.put("output", fullResponseJson);
            items.add(responseItem);

            // Multimodal Tool Support: OpenAI Responses API doesn't support attachments in fco items.
            // We follow up with a dedicated message item containing the attachments.
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
            // Assistant items in history must use 'output_text'. 
            // User and System items must use 'input_text'.
            String typePrefix = "assistant".equals(role) ? "output_text" : "input_text";
            contentArray.addObject().put("type", typePrefix).put("text", tp.getText());
        } else if (part instanceof ModelTextPart mtp) {
            String typePrefix = "assistant".equals(role) ? "output_text" : "input_text";
            contentArray.addObject().put("type", typePrefix).put("text", mtp.getText());
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
            // OpenAI Responses API: Images in message content use 'image_url' with data URI format
            contentArray.addObject()
                    .put("type", "input_audio")
                    .putObject("input_audio")
                    .put("audio", b64)
                    .put("format", format);
        } else {
            throw new AiProviderException("Image and audio only today");

            // OpenAI Responses API: Generic files use an 'input_file' nested object.
            // Note: 'input_audio' is currently not supported in the Responses API, 
            // so we fall back to 'input_file' for all non-image blobs (including audio).
            //with input_file 
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
