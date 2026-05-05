/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
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
import uno.anahata.asi.agi.message.ModelCodeCallPart;
import uno.anahata.asi.agi.message.ModelCodeOutputPart;
import uno.anahata.asi.agi.message.ModelSearchCallPart;
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

    private static final ObjectMapper API_MAPPER = new ObjectMapper();

    /** Placeholder text used for reasoning items in stateless mode when no summary is available. */
    public static final String ENCRYPTED_REASONING_PLACEHOLDER = "(Encrypted Reasoning Chain)";

    /** The generation phase reported by the model (e.g., 'commentary', 'final_answer'). */
    private String phase;

    public OpenAiModelMessage(Agi agi, String modelId) {
        super(agi, modelId);
    }

    /**
     * Processes a single item from the OpenAI 'output' array and maps it 
     * to the appropriate Anahata parts.
     * 
     * @param item The JSON node representing an OpenAI item.
     */
    /**
     * Processes a single item from the OpenAI 'output' array and maps it 
     * to the appropriate Anahata parts.
     * <p>This includes handling messages, reasoning chains, function calls, 
     * web searches, and code interpreter executions.</p>
     * 
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
            // Stateless Reasoning: Capture the signature even if text is hidden
            String encrypted = item.path("encrypted_content").asText(null);
            // Capture optional summary text
            StringBuilder summaryText = new StringBuilder();
            JsonNode summary = item.get("summary");
            if (summary != null && summary.isArray()) {
                for (JsonNode s : summary) {
                    if ("summary_text".equals(s.path("type").asText())) {
                        summaryText.append(s.path("text").asText()).append("\n");
                    }
                }
            }
            if (encrypted != null) {
                String label = summaryText.length() > 0 ? summaryText.toString().trim() : ENCRYPTED_REASONING_PLACEHOLDER;
                // Store signature in a thought part.
                TextPart tp = addTextPart(label, encrypted.getBytes(), true);
                tp.setProviderId(id);
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
             String displayText = String.format("Searching the web for: %s", queries);
             
             // Update GroundingMetadata with search queries (suggestions)
             updateGroundingMetadata(queries, List.of(), List.of(), null, null);
             
             ModelSearchCallPart part = new ModelSearchCallPart(this, displayText, queries, null);
             part.setProviderId(id);
        } else if ("web_search_output".equals(type)) {
             // We'll capture it to maintain the narrative flow.
             JsonNode results = item.get("results");
             if (results != null) {
                  TextPart tp = addTextPart("[Hosted Search Results]\n" + results.toString(), null, false);
                  tp.setProviderId(id);
             }
        } else if ("code_interpreter_call".equals(type)) {
             String code = item.path("action").path("code").asText("");
             ModelCodeCallPart part = new ModelCodeCallPart(this, code, "python", null);
             part.setProviderId(id);
        } else if ("code_interpreter_output".equals(type)) {
             String logs = item.path("logs").asText("");
             ModelCodeOutputPart part = new ModelCodeOutputPart(this, logs, null);
             part.setProviderId(id);
             
             // Capture generated files (visual outputs) as ModelBlobParts
             JsonNode files = item.get("files");
             if (files != null && files.isArray()) {
                 for (JsonNode f : files) {
                     String b64 = f.path("data").asText(null);
                     if (b64 != null) {
                         String mime = "image/png"; // Standard for code interpreter plots
                         addBlobPart(mime, java.util.Base64.getDecoder().decode(b64), null).setProviderId(id);
                     }
                 }
             }
        }
    }

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


    @Override
    public String getFrom() {
        return getModelId();
    }

    @Override
    public String getDevice() {
        return "OpenAI-Cloud";
    }
}
