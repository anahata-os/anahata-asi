/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.anthropic.adapter;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import uno.anahata.asi.agi.message.*;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;

/**
 * A translation adapter that converts Anahata's unified message model 
 * into the structured JSON required by the Anthropic Messages API.
 * <p>
 * This adapter handles:
 * </p>
 * <ul>
 *   <li>Standard text and multimodal parts (images).</li>
 *   <li>Outbound tool calls (<code>tool_use</code>).</li>
 *   <li>Inbound tool results (<code>tool_result</code>).</li>
 *   <li>Pruning logic based on the session configuration.</li>
 * </ul>
 * @author anahata
 */
public class AnthropicContentAdapter {
    /**
     * The source Anahata message to translate.
     */
    private final AbstractMessage anahataMessage;
    /**
     * Whether to include parts that have been marked as effectively pruned 
     * in the generated payload.
     */
    private final boolean includePruned;

    /**
     * Constructs a new adapter for a specific message.
     * @param anahataMessage The message to translate.
     * @param includePruned Whether to include pruned content.
     */
    public AnthropicContentAdapter(AbstractMessage anahataMessage, boolean includePruned) {
        this.anahataMessage = anahataMessage;
        this.includePruned = includePruned;
    }

    /**
     * Converts the message into a list of Anthropic-compatible JSON objects.
     * <p>
     * Note: A single Anahata message may expand into multiple Anthropic 
     * messages if it contains both tool calls (assistant role) and tool 
     * responses (user role).
     * </p>
     * @return A list of ObjectNodes ready for inclusion in a 'messages' array.
     */
    public List<ObjectNode> toAnthropic() {
        List<ObjectNode> results = new ArrayList<>();
        Role role = anahataMessage.getRole();

        if (role == Role.SYSTEM) {
            return results; // System instructions are handled at top-level
        }

        if (role == Role.MODEL && anahataMessage instanceof AbstractModelMessage<?> modelMsg) {
            // 1. Assistant message (Text + Tool Calls)
            ObjectNode assistantMsg = SchemaProvider.OBJECT_MAPPER.createObjectNode();
            assistantMsg.put("role", "assistant");
            ArrayNode assistantContent = assistantMsg.putArray("content");

            boolean hasAssistantContent = false;
            for (AbstractPart part : anahataMessage.getParts(true)) {
                if (part.isEffectivelyPruned() && !includePruned) continue;

                if (part instanceof TextPart tp) {
                    assistantContent.addObject().put("type", "text").put("text", tp.getText());
                    hasAssistantContent = true;
                } else if (part instanceof AbstractToolCall<?,?> tc) {
                    ObjectNode toolUse = assistantContent.addObject();
                    toolUse.put("type", "tool_use");
                    toolUse.put("id", tc.getId() != null ? tc.getId() : "call_" + tc.getSequentialId());
                    toolUse.put("name", tc.getToolName().replace(".", "__"));
                    try {
                        toolUse.set("input", SchemaProvider.OBJECT_MAPPER.readTree(
                            SchemaProvider.OBJECT_MAPPER.writeValueAsString(tc.getEffectiveArgs())
                        ));
                    } catch (Exception e) {
                        toolUse.putObject("input");
                    }
                    hasAssistantContent = true;
                }
            }
            
            if (hasAssistantContent) {
                results.add(assistantMsg);
            }

            // 2. User message (Tool Results)
            List<AbstractToolResponse<?>> executedResponses = modelMsg.getToolResponses().stream()
                    .filter(tr -> includePruned || !tr.getCall().isEffectivelyPruned())
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            if (!executedResponses.isEmpty()) {
                ObjectNode userMsg = SchemaProvider.OBJECT_MAPPER.createObjectNode();
                userMsg.put("role", "user");
                ArrayNode userContent = userMsg.putArray("content");

                for (AbstractToolResponse<?> tr : executedResponses) {
                    ObjectNode toolResult = userContent.addObject();
                    toolResult.put("type", "tool_result");
                    toolResult.put("tool_use_id", tr.getCall().getId() != null ? tr.getCall().getId() : "call_" + tr.getCall().getSequentialId());
                    
                    String contentStr = SchemaProvider.OBJECT_MAPPER.valueToTree(tr).toString();
                    if (tr.getStatus() == uno.anahata.asi.agi.tool.ToolExecutionStatus.FAILED || tr.getStatus() == uno.anahata.asi.agi.tool.ToolExecutionStatus.DECLINED) {
                        toolResult.put("is_error", true);
                    }
                    toolResult.put("content", contentStr);
                }
                results.add(userMsg);
            }
        } else {
            // USER or RAG message
            ObjectNode userMsg = SchemaProvider.OBJECT_MAPPER.createObjectNode();
            userMsg.put("role", "user");
            ArrayNode userContent = userMsg.putArray("content");
            boolean hasContent = false;

            for (AbstractPart part : anahataMessage.getParts(true)) {
                if (part.isEffectivelyPruned() && !includePruned) continue;

                if (part instanceof TextPart tp) {
                    userContent.addObject().put("type", "text").put("text", tp.getText());
                    hasContent = true;
                } else if (part instanceof BlobPart bp) {
                    String mimeType = bp.getMimeType();
                    if (mimeType.startsWith("image/")) {
                        ObjectNode imageNode = userContent.addObject();
                        imageNode.put("type", "image");
                        ObjectNode sourceNode = imageNode.putObject("source");
                        sourceNode.put("type", "base64");
                        sourceNode.put("media_type", mimeType);
                        sourceNode.put("data", Base64.getEncoder().encodeToString(bp.getData()));
                        hasContent = true;
                    } else {
                        userContent.addObject().put("type", "text")
                            .put("text", "[Attached File: " + mimeType + "]");
                        hasContent = true;
                    }
                }
            }
            
            if (hasContent) {
                results.add(userMsg);
            }
        }

        return results;
    }
}