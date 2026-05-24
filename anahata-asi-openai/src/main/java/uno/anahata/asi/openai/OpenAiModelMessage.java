/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import uno.anahata.asi.agi.message.web.GroundingMetadata;
import uno.anahata.asi.agi.message.web.GroundingSource;
import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.ModelBlobPart;
import uno.anahata.asi.agi.message.ModelTextPart;
import uno.anahata.asi.agi.message.code.HostedCodeExecutionCallPart;
import uno.anahata.asi.agi.message.code.HostedCodeExecutionResultPart;
import uno.anahata.asi.agi.message.web.WebSearchCallPart;
import uno.anahata.asi.agi.message.TextPart;
import uno.anahata.asi.agi.provider.FinishReason;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;

/**
 * Specialized ModelMessage for the OpenAI Responses API.
<p>Aggregates multiple items (messages, reasoning, function calls, web searches, and code execution) 
from a single Responses API turn into a unified Anahata message.
 * It handles complex multi-item
merging and multimodal data harvesting.</p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class OpenAiModelMessage extends AbstractModelMessage<OpenAiResponse> {

    /**
     * Internal Jackson mapper for stream event processing.
     */
    private static final ObjectMapper API_MAPPER = new ObjectMapper();

    /**
     * Placeholder text used for reasoning items in stateless mode when no 
     * summary is available.
     */
    public static final String ENCRYPTED_REASONING_PLACEHOLDER = "(Encrypted Reasoning Chain)";

    /** The generation phase reported by the model (e.g., 'commentary', 'final_answer'). */
    private String phase;

    /**
     * Tracks the association between tool calls and their parent reasoning chains.
     * This is required for OpenAI's strict referential integrity.
     */
    private final Map<AbstractToolCall<?, ?>, ModelTextPart> toolThoughts = new LinkedHashMap<>();

    /**
     * Transient tracker for the most recently parsed reasoning item.
     */
    private transient ModelTextPart lastParsedThought;

    /**
     * Constructs a new OpenAiModelMessage bound to a specific session and model.
     * @param agi The parent AGI session.
     * @param modelId The ID of the model that generated this message.
     */
    public OpenAiModelMessage(Agi agi, String modelId) {
        super(agi, modelId);
    }

    /**
     * Processes a single item from the OpenAI 'output' array and maps it 
     * to the appropriate Anahata parts.
     * <p>This includes handling messages, reasoning chains, function calls, 
     * web searches, and code interpreter executions.</p>
     * @param item The JSON node representing an OpenAI item.
     */
    @SneakyThrows
    public void processItem(JsonNode item) {
        String type = item.path("type").asText();
        String id = item.path("id").asText(null);
        
        // 1. Map Status to FinishReason (updates as we process items)
        String status = item.path("status").asText();
        if ("completed".equals(status)) {
            setFinishReason(FinishReason.STOP);
        } else if ("incomplete".equals(status)) {
            setFinishReason(FinishReason.MAX_TOKENS);
        }

        if ("message".equals(type)) {
            setProviderId(id);
            this.phase = item.path("phase").asText(null);
            JsonNode content = item.get("content");
            if (content != null && content.isArray()) {
                for (JsonNode partNode : content) {
                    String partType = partNode.path("type").asText();
                    String text = partNode.path("text").asText();
                    TextPart tp = null;
                    if ("output_text".equals(partType)) {
                        tp = addTextPart(text, null, false);
                        // Harvesting Citations: Extract annotations and map to GroundingMetadata
                        JsonNode annotations = partNode.get("annotations");
                        if (annotations != null && annotations.isArray()) {
                            processCitations(annotations);
                        }
                    } else if ("reasoning_content".equals(partType)) {
                        tp = addTextPart(text, null, true);
                    }
                    if (tp != null) {
                        tp.setProviderId(id);
                    }
                }
            }
        } else if ("reasoning".equals(type)) {
            String encrypted = item.path("encrypted_content").asText(null);
            StringBuilder thoughtsText = new StringBuilder();
            byte[] signature = null;
            
            JsonNode content = item.get("content");
            if (content != null && content.isArray()) {
                for (JsonNode block : content) {
                    if ("thinking".equals(block.path("type").asText())) {
                        thoughtsText.append(block.path("thinking").asText(""));
                        String sig = block.path("signature").asText(null);
                        if (sig != null) signature = sig.getBytes();
                    } else if ("redacted_thinking".equals(block.path("type").asText())) {
                        String data = block.path("data").asText(null);
                        if (data != null) signature = data.getBytes();
                    }
                }
            }
            
            StringBuilder summaryText = new StringBuilder();
            JsonNode summary = item.get("summary");
            if (summary != null && summary.isArray()) {
                for (JsonNode s : summary) {
                    if ("summary_text".equals(s.path("type").asText())) {
                        summaryText.append(s.path("text").asText()).append("\n");
                    }
                }
            }
            
            ModelTextPart existingThought = null;
            if (isStreaming()) {
                 List<AbstractPart> parts = getParts();
                 for (int i = parts.size() - 1; i >= 0; i--) {
                     if (parts.get(i) instanceof ModelTextPart mtp && mtp.isThought()) {
                         if (mtp.getProviderId() == null || mtp.getProviderId().equals(id)) {
                             existingThought = mtp;
                             break;
                         }
                     }
                 }
            }

            if (existingThought != null) {
                existingThought.setProviderId(id);
                if (signature != null) {
                    existingThought.setThoughtSignature(signature);
                } else if (encrypted != null) {
                    existingThought.setThoughtSignature(encrypted.getBytes());
                }
                if (encrypted != null) {
                    existingThought.setText(ENCRYPTED_REASONING_PLACEHOLDER);
                } else if (summaryText.length() > 0) {
                    existingThought.setText(summaryText.toString().trim());
                }
                lastParsedThought = existingThought;
            } else {
                String label = "";
                if (encrypted != null) {
                    label = ENCRYPTED_REASONING_PLACEHOLDER;
                } else if (summaryText.length() > 0) {
                    label = summaryText.toString().trim();
                } else if (thoughtsText.length() > 0) {
                    label = thoughtsText.toString().trim();
                }
                byte[] sigToUse = encrypted != null ? encrypted.getBytes() : signature;
                TextPart tp = addTextPart(label, sigToUse, true);
                tp.setProviderId(id);
                lastParsedThought = (ModelTextPart) tp;
            }
        } else if ("function_call".equals(type)) {
            String callId = item.path("call_id").asText();
            String ns = item.path("namespace").asText(null);
            String name = item.path("name").asText();
            String argsJson = item.path("arguments").asText("{}");
            // Reconstruct the Canonical FQN for Anahata tool lookup
            String fullToolName = (ns != null && !ns.isEmpty()) ? ns + "." + name : name;
            Map<String, Object> args = API_MAPPER.readValue(argsJson, Map.class);
            AbstractToolCall<?, ?> tc = getAgi().getToolManager().createToolCall(this, callId, fullToolName, args);
            if (tc != null) {
                tc.setProviderId(id);
                if (lastParsedThought != null) {
                    toolThoughts.put(tc, lastParsedThought);
                }
            }
        } else if ("web_search_call".equals(type)) {
             JsonNode action = item.get("action");
             List<String> queries = new ArrayList<>();
             if (action.has("query")) {
                 queries.add(action.get("query").asText());
             }
             if (action.has("queries") && action.get("queries").isArray()) {
                 for (JsonNode q : action.get("queries")) {
                     queries.add(q.asText());
                 }
             }
             
             // Responses API Harvesting: Map action sources and results to GroundingSources
             List<GroundingSource> sources = new ArrayList<>();
             JsonNode sourcesNode = action.path("sources");
             if (sourcesNode.isArray()) {
                 for (JsonNode s : sourcesNode) {
                     String url = s.path("url").asText(null);
                     if (url != null) {
                         sources.add(GroundingSource.builder()
                                 .uri(url)
                                 .title(s.path("title").asText(url))
                                 .build());
                     }
                 }
             }
             
             JsonNode results = item.get("results");
             if (results != null && results.isArray()) {
                 for (JsonNode r : results) {
                     String url = r.path("url").asText(null);
                     if (url != null) {
                         sources.add(GroundingSource.builder()
                                 .uri(url)
                                 .title(r.path("title").asText(url))
                                 .build());
                     }
                 }
             }
             
             if (!queries.isEmpty() || !sources.isEmpty()) {
                 updateGroundingMetadata(queries, List.of(), sources, null, item.toString());
             }

             String displayText = String.format("Searching the web for: %s", queries);
             WebSearchCallPart part = new WebSearchCallPart(this, displayText, queries, null);
             part.setProviderId(id);
        } else if ("code_interpreter_call".equals(type)) {
             String code = item.path("code").asText("");
             HostedCodeExecutionCallPart callPart = new HostedCodeExecutionCallPart(this, code, "python", null);
             callPart.setProviderId(id);
             
             // Responses API: Outputs (logs, images) are nested in the call item
             JsonNode outputs = item.get("outputs");
             if (outputs != null && outputs.isArray()) {
                 for (JsonNode out : outputs) {
                     String outType = out.path("type").asText();
                     if ("logs".equals(outType)) {
                         HostedCodeExecutionResultPart resultPart = new HostedCodeExecutionResultPart(this, out.path("logs").asText(""), null);
                         resultPart.setProviderId(id);
                         resultPart.setParentCall(callPart);
                     } else if ("image".equals(outType)) {
                         String b64 = out.path("image").path("data").asText(null);
                         if (b64 != null) {
                             ModelBlobPart mbp = addBlobPart("image/png", Base64.getDecoder().decode(b64), null);
                             mbp.setProviderId(id);
                             mbp.setParentCall(callPart);
                         }
                     }
                 }
             }
        }
    }

    /**
     * Extracts citations from OpenAI annotations and updates the grounding metadata.
     * @param annotations The JSON array of annotations from the API.
     */
    private void processCitations(JsonNode annotations) {
        List<GroundingSource> sources = new ArrayList<>();
        for (JsonNode ann : annotations) {
            String type = ann.path("type").asText();
            if ("url_citation".equals(type)) {
                sources.add(GroundingSource.builder()
                        .title(ann.path("title").asText("Web Source"))
                        .uri(ann.path("url").asText(""))
                        .build());
            } else if ("container_file_citation".equals(type)) {
                sources.add(GroundingSource.builder()
                        .title(ann.path("filename").asText("Generated File"))
                        .uri("file-id://" + ann.path("file_id").asText(""))
                        .build());
            }
        }
        
        if (!sources.isEmpty()) {
            updateGroundingMetadata(List.of(), List.of(), sources, null, annotations.toString());
        }
    }

    /**
     * Aggregates new metadata components into the message's GroundingMetadata.
     * <p>Ensures that citations, supporting texts, and search queries from multiple 
     * items are correctly merged and deduped.</p>
     * 
     * @param queries Suggested search queries.
     * @param texts Supporting text segments.
     * @param sources Grounding sources (citations).
     * @param html The search entry point HTML.
     * @param rawJson The raw JSON from the provider.
     */
    private void updateGroundingMetadata(List<String> queries, List<String> texts, List<GroundingSource> sources, String html, String rawJson) {
        GroundingMetadata existing = getGroundingMetadata();
        
        List<String> mergedQueries = new ArrayList<>(queries);
        List<String> mergedTexts = new ArrayList<>(texts);
        List<GroundingSource> mergedSources = new ArrayList<>(sources);
        String mergedHtml = html;
        String mergedRawJson = rawJson;

        if (existing != null) {
            // High Fidelity Merge: Preserve all existing metadata components
            if (existing.getWebSearchQueries() != null) mergedQueries.addAll(existing.getWebSearchQueries());
            if (existing.getSupportingTexts() != null) mergedTexts.addAll(existing.getSupportingTexts());
            if (existing.getSources() != null) mergedSources.addAll(existing.getSources());
            if (mergedHtml == null) mergedHtml = existing.getSearchEntryPointHtml();
            if (mergedRawJson == null) mergedRawJson = existing.getRawJson();
        }

        // Distinct check to avoid duplicates in aggregate processing
        mergedQueries = mergedQueries.stream().distinct().collect(Collectors.toList());
        mergedTexts = mergedTexts.stream().distinct().collect(Collectors.toList());

        setGroundingMetadata(new GroundingMetadata(mergedQueries, mergedTexts, mergedSources, mergedHtml, mergedRawJson));
    }


    /**
     * Handles a Server-Sent Event (SSE) from the OpenAI Responses API stream.
     * <p>Routes deltas for real-time text and reasoning generation, and defers
     * complex items (function calls, web searches) to the completion of the item
     * where the full JSON structure is guaranteed to be intact.</p>
     * @param eventNode The parsed JSON node of the stream event.
     */
    public void handleStreamEvent(JsonNode eventNode) {
        String type = eventNode.path("type").asText();

        switch (type) {
            case "response.output_text.delta":
                appendContent(eventNode.path("delta").asText());
                break;
            case "response.reasoning_text.delta":
            case "response.reasoning_summary_text.delta":
                appendThoughts(eventNode.path("delta").asText());
                break;
            case "response.output_item.done":
                JsonNode item = eventNode.path("item");
                String itemType = item.path("type").asText();
                if ("message".equals(itemType)) {
                    JsonNode content = item.get("content");
                    if (content != null && content.isArray()) {
                        for (JsonNode partNode : content) {
                            if ("output_text".equals(partNode.path("type").asText())) {
                                JsonNode annotations = partNode.get("annotations");
                                if (annotations != null && annotations.isArray()) {
                                    processCitations(annotations);
                                }
                            }
                        }
                    }
                } else {
                    processItem(item);
                }
                break;
            case "response.done":
                JsonNode response = eventNode.path("response");
                if (response != null && response.has("usage")) {
                    JsonNode usage = response.get("usage");
                    if (usage != null && !usage.isNull()) {
                        setBilledCompletionTokens(usage.path("output_tokens").asInt(0));
                        setBilledPromptTokens(usage.path("input_tokens").asInt(0));
                    }
                }
                if (response != null && response.has("status")) {
                    String status = response.path("status").asText();
                    if ("completed".equals(status)) {
                        setFinishReason(FinishReason.STOP);
                    } else if ("incomplete".equals(status)) {
                        setFinishReason(FinishReason.MAX_TOKENS);
                    }
                }
                break;
        }
    }

    /**
     * Appends text to the current active text part or creates a new one if needed.
     * @param text The text delta to append.
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
     * Appends text to the current active reasoning part or creates a new 
     * thought part if needed.
     * @param text The reasoning delta to append.
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
     * {@inheritDoc}
     */
    @Override
    public String getFrom() {
        return getModelId();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDevice() {
        return "OpenAI-Cloud";
    }
}
