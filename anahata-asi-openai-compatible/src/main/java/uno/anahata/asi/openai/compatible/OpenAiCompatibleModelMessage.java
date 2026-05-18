/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai.compatible;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import java.util.Map;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.provider.FinishReason;

/**
 * Base implementation for OpenAI-specific model messages.
 * Provides common logic for content accumulation, finish reason mapping,
 * and tool call handling across different OpenAI API versions.
 * 
 * @author anahata
 */
@Slf4j
public abstract class OpenAiCompatibleModelMessage extends AbstractModelMessage<OpenAiCompatibleResponse> {

    /**
     * State flag indicating if the parser is currently inside a reasoning tag.
     */
    private boolean insideReasoningTags = false;

    /**
     * Constructs a new base model message.
     * @param agi The parent session.
     * @param modelId The model ID.
     */
    public OpenAiCompatibleModelMessage(Agi agi, String modelId) {
        super(agi, modelId);
    }

    /**
     * Updates the message content and state from a JSON node (choice, item, or event).
     * 
     * @param node The JSON node to parse.
     * @param reasoningStyle The strategy for extracting thoughts.
     * @param reasoningFieldName The field name for reasoning content (if using FIELD style).
     * @param reasoningTags The tags for reasoning content (if using TAGS style).
     */
    public abstract void updateFromNode(JsonNode node, OpenAiCompatibleReasoningStyle reasoningStyle, String reasoningFieldName, List<String> reasoningTags);
    
    /**
     * Updates a single tool call from a JSON node.
     * 
     * @param callNode The JSON node containing the tool call (or delta).
     */
    public abstract void updateToolCall(JsonNode callNode);

    /**
     * Flushes any buffered tool calls (used during streaming).
     * This ensures that partial tool call arguments are fully assembled and
     * registered in the tool manager.
     */
    public abstract void flushToolCalls();

    /**
     * Sets the finish reason from a raw OpenAI string.
     * 
     * @param fr The raw finish reason string (e.g., "stop", "length").
     */
    public void setFinishReasonFromOpenAi(String fr) {
        setFinishReason(mapFinishReason(fr));
        if ("stop".equals(fr) || "tool_calls".equals(fr)) {
            flushToolCalls();
        }
    }

    /**
     * Maps a standard OpenAI finish reason string to the Anahata enum.
     * @param reason The raw string from the API.
     * @return The corresponding FinishReason.
     */
    private FinishReason mapFinishReason(String reason) {
        if (reason == null) return FinishReason.OTHER;
        return switch (reason) {
            case "stop" -> FinishReason.STOP;
            case "length" -> FinishReason.MAX_TOKENS;
            case "tool_calls" -> FinishReason.STOP;
            case "content_filter" -> FinishReason.SAFETY;
            default -> FinishReason.OTHER;
        };
    }

    /**
     * Appends text to the main content part or creates a new one.
     * 
     * @param text The text to append.
     */
    public void appendContent(String text) {
        List<AbstractPart> parts = getParts();
        if (!parts.isEmpty() && parts.get(parts.size() - 1) instanceof ModelTextPart mtp && !mtp.isThought()) {
            mtp.appendText(text);
        } else {
            addTextPart(text);
        }
    }

    /**
     * Appends text to the reasoning/thought part or creates a new one.
     * 
     * @param text The thought text to append.
     */
    public void appendThoughts(String text) {
        List<AbstractPart> parts = getParts();
        if (!parts.isEmpty() && parts.get(parts.size() - 1) instanceof ModelTextPart mtp && mtp.isThought()) {
            mtp.appendText(text);
        } else {
            addTextPart(text, null, true);
        }
    }

    /**
     * Appends text while detecting and extracting reasoning content wrapped in tags.
     * 
     * @param text The text containing potential tags.
     * @param startTag The opening tag (e.g., "<think>").
     * @param endTag The closing tag (e.g., "</think>").
     */
    public void appendTaggedContent(String text, String startTag, String endTag) {
        if (!insideReasoningTags && text.contains(startTag)) {
            int idx = text.indexOf(startTag);
            String before = text.substring(0, idx);
            if (!before.isEmpty()) {
                appendContent(before);
            }
            insideReasoningTags = true;
            appendTaggedContent(text.substring(idx + startTag.length()), startTag, endTag);
        } else if (insideReasoningTags && text.contains(endTag)) {
            int idx = text.indexOf(endTag);
            String thoughts = text.substring(0, idx);
            if (!thoughts.isEmpty()) {
                appendThoughts(thoughts);
            }
            insideReasoningTags = false;
            appendTaggedContent(text.substring(idx + endTag.length()), startTag, endTag);
        } else {
            if (insideReasoningTags) {
                appendThoughts(text);
            } else {
                appendContent(text);
            }
        }
    }
}
