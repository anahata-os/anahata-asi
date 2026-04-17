/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.adapter;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import uno.anahata.asi.agi.provider.TokenizerType;
import uno.anahata.asi.internal.TokenizerUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.BlobPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.Role;
import uno.anahata.asi.agi.message.TextPart;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.spi.AbstractToolResponse;

/**
 * A specialized content adapter that translates Anahata's domain model into 
 * standard OpenAI-compatible JSON objects for the Chat Completion API.
 * <p>
 * This adapter follows the Anahata V2 pruning and metadata strategies, 
 * ensuring that models remain context-aware by injecting metadata headers 
 * even for effectively pruned parts.
 * </p>
 * <p>
 * <b>Role Mapping:</b>
 * <ul>
 *   <li>{@link Role#USER} &rarr; "user"</li>
 *   <li>{@link Role#MODEL} &rarr; "assistant"</li>
 *   <li>{@link Role#SYSTEM} &rarr; "system"</li>
 * </ul>
 * </p>
 * <p>
 * <b>Turn Synthesis:</b> For Model turns containing tool responses, it generates 
 * an 'assistant' message (calls) followed by one 'tool' message per response.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@RequiredArgsConstructor
public class OpenAiContentAdapter {

    /** The Anahata message to be translated. */
    private final AbstractMessage anahataMessage;
    
    /** Whether to include the full content of parts that have been effectively pruned. */
    private final boolean includePruned;
    
    /** The tokenizer used to count parts accurately for the CwGC. */
    private final TokenizerType tokenizerType;

    /**
     * Translates the Anahata message into a list of OpenAI-style JSON message nodes.
     * 
     * @return A list of ObjectNodes representing the messages.
     */
    public List<ObjectNode> toOpenAi() {
        Role role = anahataMessage.getRole();
        List<ObjectNode> results = new ArrayList<>();
        
        if (role == Role.MODEL && anahataMessage instanceof AbstractModelMessage<?> modelMsg) {
            results.addAll(toOpenAiModel(modelMsg));
        } else {
            ObjectNode userNode = toOpenAiUser();
            if (userNode != null) {
                results.add(userNode);
            }
        }
        
        return results;
    }

    /**
     * Synthesizes a Model turn into 'assistant' and 'tool' messages.
     */
    private List<ObjectNode> toOpenAiModel(AbstractModelMessage<?> modelMsg) {
        List<ObjectNode> synthesized = new ArrayList<>();
        
        ObjectNode assistantNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        assistantNode.put("role", "assistant");
        
        StringBuilder textContent = new StringBuilder();
        
        // 1. Message Metadata Header
        if (shouldInjectInbandMetadata()) {
            textContent.append(anahataMessage.createMetadataHeader()).append("\n");
        }
        
        ArrayNode toolCalls = SchemaProvider.OBJECT_MAPPER.createArrayNode();
        
        // 2. Process parts with metadata interleaving
        for (AbstractPart part : anahataMessage.getParts(true)) {
            addPartWithMetadata(textContent, toolCalls, part);
        }
        
        if (textContent.length() > 0) {
            assistantNode.put("content", textContent.toString());
        } else {
            assistantNode.putNull("content");
        }
        
        if (toolCalls.size() > 0) {
            assistantNode.set("tool_calls", toolCalls);
        }
        
        if (textContent.length() > 0 || toolCalls.size() > 0) {
            synthesized.add(assistantNode);
        }
        
        // 3. Responses synthesis
        List<AbstractToolResponse<?>> executedResponses = modelMsg.getToolResponses().stream()
                .filter(tr -> includePruned || !tr.getCall().isEffectivelyPruned())
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        for (AbstractToolResponse<?> tr : executedResponses) {
            ObjectNode responseNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
            responseNode.put("role", "tool");
            responseNode.put("tool_call_id", tr.getCall().getId());
            
            // Send the entire rich response (Result, Status, Logs, Errors)
            String fullResponseJson = SchemaProvider.OBJECT_MAPPER.valueToTree(tr).toString();
            responseNode.put("content", fullResponseJson);
            synthesized.add(responseNode);
        }
        
        return synthesized;
    }

    /**
     * Translates USER or SYSTEM messages, supporting multimodal content arrays.
     */
    private ObjectNode toOpenAiUser() {
        ObjectNode msgNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        msgNode.put("role", anahataMessage.getRole() == Role.SYSTEM ? "system" : "user");
        
        ArrayNode contentArray = SchemaProvider.OBJECT_MAPPER.createArrayNode();
        
        if (shouldInjectInbandMetadata()) {
            contentArray.addObject()
                    .put("type", "text")
                    .put("text", anahataMessage.createMetadataHeader() + "\n");
        }
        
        for (AbstractPart part : anahataMessage.getParts(true)) {
            boolean isEffectivelyPruned = part.isEffectivelyPruned();
            boolean shouldIncludeContent = !isEffectivelyPruned || includePruned;

            if (shouldInjectInbandMetadata()) {
                contentArray.addObject()
                        .put("type", "text")
                        .put("text", part.createMetadataHeader() + "\n");
            }

            if (shouldIncludeContent) {
                if (part instanceof TextPart tp) {
                    contentArray.addObject()
                            .put("type", "text")
                            .put("text", tp.getText());
                } else if (part instanceof BlobPart bp) {
                    if (bp.getMimeType().startsWith("image/")) {
                        ObjectNode imageNode = contentArray.addObject();
                        imageNode.put("type", "image_url");
                        ObjectNode urlNode = imageNode.putObject("image_url");
                        urlNode.put("url", "data:" + bp.getMimeType() + ";base64," + 
                                    Base64.getEncoder().encodeToString(bp.getData()));
                    } else {
                        String fileName = bp.getSourcePath() != null ? bp.getSourcePath().getFileName().toString() : "unnamed-file";
                        contentArray.addObject()
                                .put("type", "text")
                                .put("text", String.format("[File Attached: %s (%s)]", fileName, bp.getMimeType()));
                    }
                }
            }
        }
        
        if (contentArray.isEmpty()) {
            return null;
        }

        if (contentArray.size() == 1 && "text".equals(contentArray.get(0).get("type").asText())) {
            msgNode.put("content", contentArray.get(0).get("text").asText());
        } else {
            msgNode.set("content", contentArray);
        }
        
        return msgNode;
    }

    /**
     * Internal helper for metadata interleaving in the assistant buffer.
     */
    private void addPartWithMetadata(StringBuilder textContent, ArrayNode toolCalls, AbstractPart part) {
        boolean isEffectivelyPruned = part.isEffectivelyPruned();
        boolean shouldIncludeContent = !isEffectivelyPruned || includePruned;

        if (shouldInjectInbandMetadata()) {
            textContent.append(part.createMetadataHeader()).append("\n");
        }

        if (shouldIncludeContent) {
            if (part instanceof ModelTextPart mtp) {
                String text = mtp.getText();
                part.setTokenCount(TokenizerUtils.countTokens(text, tokenizerType));
                textContent.append(text);
            } else if (part instanceof AbstractToolCall<?, ?> tc) {
                ObjectNode callNode = toolCalls.addObject();
                callNode.put("id", tc.getId());
                callNode.put("type", "function");
                ObjectNode funcNode = callNode.putObject("function");
                funcNode.put("name", tc.getToolName());
                try {
                    String argsJson = SchemaProvider.OBJECT_MAPPER.writeValueAsString(tc.getResponse().getExecutedArgs());
                    funcNode.put("arguments", argsJson);
                    part.setTokenCount(TokenizerUtils.countTokens(argsJson, tokenizerType));
                } catch (Exception e) {
                    log.error("Failed to serialize executed args for tool call {}", tc.getId(), e);
                    funcNode.put("arguments", "{}");
                }
            } else if (part instanceof BlobPart bp) {
                 textContent.append(String.format("\n[Output Blob: %s]\n", bp.getMimeType()));
            }
        }
    }

    /**
     * Determines if in-band metadata headers should be injected into the payload.
     * <p>
     * This check combines the global request configuration policy with the
     * message's individual metadata capability.
     * </p>
     * @return {@code true} if metadata injection is enabled and applicable.
     */
    private boolean shouldInjectInbandMetadata() {
        return anahataMessage.getAgi().getRequestConfig().isInjectInbandMetadata() && anahataMessage.shouldCreateMetadata();
    }
}
