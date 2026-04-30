/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.openai;

import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolkit;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.provider.GenerationRequest;
import uno.anahata.asi.agi.provider.RequestConfig;
import uno.anahata.asi.agi.provider.ThinkingLevel;
import uno.anahata.asi.agi.tool.schema.SchemaProvider;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.openai.adapter.OpenAiChatCompletionsResponseAdapter;
import uno.anahata.asi.openai.adapter.OpenAiResponsesApiContentAdapter;

/**
 * Specialized model implementation for official OpenAI services. Handles legacy
 * completion endpoints, stream options, and specific usage parsing.
 *
 * @author anahata
 */
@Slf4j
public class OpenAiModel extends OpenAiCompatibleModel {

    public OpenAiModel(OpenAiCompatibleProvider provider, String modelId, String displayName) {
        super(provider, modelId, displayName);
    }

    public OpenAiModel(OpenAiCompatibleProvider provider, com.fasterxml.jackson.databind.JsonNode node) {
        super(provider, node);
    }

    @Override
    public OpenAiModelMessage createModelMessage(uno.anahata.asi.agi.Agi agi) {
        if (prefersResponsesApi()) {
            return new OpenAiResponsesModelMessage(agi, getModelId());
        }
        return super.createModelMessage(agi);
    }

    @Override
    public List<String> getSupportedResponseModalities() {
        String lowerId = getModelId().toLowerCase();
        if (lowerId.contains("gpt-5.4") || lowerId.contains("o4")) {
            return List.of("TEXT", "IMAGE", "AUDIO");
        }
        return super.getSupportedResponseModalities();
    }

    @Override
    public List<ServerTool> getAvailableServerTools() {
        if (prefersResponsesApi()) {
            List<ServerTool> tools = new java.util.ArrayList<>();
            for (OpenAiHostedTool ht : OpenAiHostedTool.values()) {
                tools.add(new ServerTool(ht, ht.getId(), ht.getDescription()));
            }
            return tools;
        }
        return super.getAvailableServerTools();
    }

    @Override
    public String getToolDeclarationJson(AbstractTool<?, ?> tool, RequestConfig config) {
        if (prefersResponsesApi()) {
            String fullName = tool.getName();
            String shortName = fullName.contains(".") ? fullName.substring(fullName.lastIndexOf(".") + 1) : fullName;
            return getResponsesToolDeclaration(tool, config, shortName);
        }
        return super.getToolDeclarationJson(tool, config);
    }

    private String getResponsesToolDeclaration(AbstractTool<?, ?> tool, RequestConfig config, String name) {
        ObjectNode toolNode = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        toolNode.put("type", "function");
        toolNode.put("name", name.replaceAll("[^a-zA-Z0-9_-]", "_"));
        toolNode.put("description", tool.getDescription());

        // We disable 'strict' mode by default to support optional parameters 
        // and dynamic Maps (which require additionalProperties: true).
        boolean strict = false;
        toolNode.put("strict", strict);

        ObjectNode paramsNode = buildParametersNode(tool, strict);
        toolNode.set("parameters", paramsNode);

        return toolNode.toPrettyString();
    }

    @Override
    protected String getEndpoint() {
        if (isLegacyModel()) {
            return "completions";
        }
        if (prefersResponsesApi()) {
            return "responses";
        }
        return "chat/completions";
    }

    @Override
    protected ObjectNode preparePayload(GenerationRequest request, boolean stream) {
        if (prefersResponsesApi()) {
            return prepareResponsesPayload(request, stream);
        }

        ObjectNode payload = super.preparePayload(request, stream);

        // 1. Handle Stream Options (OpenAI specific for chat/completions)
        if (stream) {
            payload.putObject("stream_options").put("include_usage", true);
        }

        // 2. Handle Legacy Model Payload Transformation
        if (isLegacyModel()) {
            log.info("Transforming payload for legacy completion model: {}", getModelId());
            payload.remove("messages");

            // Flatten history to a single prompt string
            StringBuilder prompt = new StringBuilder();
            if (!request.config().getSystemInstructions().isEmpty()) {
                prompt.append("System Instructions:\n");
                request.config().getSystemInstructions().forEach(si -> {
                    prompt.append(si).append("\n");
                });
                prompt.append("\n");
            }

            for (AbstractMessage msg : request.history()) {
                prompt.append(msg.getRole().name()).append(": ");
                List<ObjectNode> parts = new OpenAiChatCompletionsResponseAdapter(msg, request.config().isIncludePruned(), getTokenizerType(),
                        getReasoningStyle(), getReasoningTags()).toOpenAi();
                for (ObjectNode part : parts) {
                    if (part.has("content")) {
                        prompt.append(part.get("content").asText());
                    }
                }
                prompt.append("\n");
            }
            prompt.append("MODEL: ");
            payload.put("prompt", prompt.toString());
        }

        return payload;
    }

    @Override
    protected void enrichPayload(ObjectNode payload, GenerationRequest request) {
        if (supportsReasoningEffort()) {
            ThinkingLevel level = request.config().getThinkingLevel();
            // Only send reasoning effort if explicitly specified and not 'unspecified'
            if (level != null && level != ThinkingLevel.THINKING_LEVEL_UNSPECIFIED) {
                String effort = switch (level) {
                    case NONE ->
                        "none";
                    case MINIMAL ->
                        "minimal";
                    case LOW ->
                        "low";
                    case MEDIUM ->
                        "medium";
                    case HIGH ->
                        "high";
                    case XHIGH ->
                        "xhigh";
                    default ->
                        null;
                };

                if (effort != null) {
                    ObjectNode reasoning = payload.putObject("reasoning");
                    reasoning.put("effort", effort);
                    log.info("Setting OpenAI reasoning.effort to '{}' for model {}", effort, getModelId());
                }
            }
        }
    }

    /**
     * Prepares the payload for the newer /v1/responses endpoint. Uses 'input'
     * items instead of 'messages'.
     */
    private ObjectNode prepareResponsesPayload(GenerationRequest request, boolean stream) {
        ObjectNode payload = SchemaProvider.OBJECT_MAPPER.createObjectNode();
        payload.put("model", getModelId());
        payload.put("stream", stream);
        payload.put("store", true);

        // 1. Stateful Chaining: Search backwards for the last valid model response ID
        List<AbstractMessage> history = request.history();
        for (int i = history.size() - 1; i >= 0; i--) {
            if (history.get(i) instanceof OpenAiModelMessage omm && omm.getResponse() != null) {
                String responseId = omm.getResponse().getId();
                if (responseId != null && !responseId.startsWith("estimated-")) {
                    payload.put("previous_response_id", responseId);
                    log.info("Chaining OpenAI response via previous_response_id: {}", responseId);
                    break;
                }
            }
        }

        // 2. Output Modalities & Audio Config
        List<String> requested = request.config().getResponseModalities();
        boolean audioRequested = false;
        boolean nonDefaultRequested = false;

        if (requested != null && !requested.isEmpty()) {
            for (String mod : requested) {
                String lowerMod = mod.toLowerCase();
                if ("audio".equals(lowerMod)) {
                    audioRequested = true;
                    nonDefaultRequested = true;
                } else if (!"text".equals(lowerMod)) {
                    nonDefaultRequested = true;
                }
            }
        }

        if (nonDefaultRequested) {
            ArrayNode modalities = payload.putArray("modalities");
            for (String mod : requested) {
                String lowerMod = mod.toLowerCase();
                if ("text".equals(lowerMod) || "audio".equals(lowerMod)) {
                    modalities.add(lowerMod);
                }
            }

            if (audioRequested) {
                ObjectNode audioConfig = payload.putObject("audio");
                audioConfig.put("voice", "alloy");
                audioConfig.put("format", "wav");
            }
        }

        // 3. Tools (Local & Server)
        ArrayNode toolsArray = null;

        List<? extends AbstractTool> localTools = request.config().getLocalTools();
        if (localTools != null && !localTools.isEmpty()) {
            toolsArray = payload.putArray("tools");

            // Map tools to their toolkits for Namespacing
            Map<AbstractToolkit, List<AbstractTool>> grouped = localTools.stream()
                    .collect(Collectors.groupingBy(AbstractTool::getToolkit));

            for (Map.Entry<AbstractToolkit, List<AbstractTool>> entry : grouped.entrySet()) {
                AbstractToolkit toolkit = entry.getKey();
                List<AbstractTool> tools = entry.getValue();

                ObjectNode namespaceNode = toolsArray.addObject();
                namespaceNode.put("type", "namespace");
                namespaceNode.put("name", toolkit.getName());
                namespaceNode.put("description", toolkit.getDescription());

                ArrayNode nsTools = namespaceNode.putArray("tools");
                for (AbstractTool<?, ?> tool : tools) {
                    try {
                        String fullName = tool.getName();
                        String shortName = fullName.contains(".") ? fullName.substring(fullName.lastIndexOf(".") + 1) : fullName;
                        nsTools.add(SchemaProvider.OBJECT_MAPPER.readTree(getResponsesToolDeclaration(tool, request.config(), shortName)));
                    } catch (Exception e) {
                        log.error("Failed to parse tool declaration for {}", tool.getName(), e);
                    }
                }
            }
        }

        if (request.config().isServerToolsEnabled()) {
            List<ServerTool> enabled = request.config().getEnabledServerTools();
            if (enabled != null && !enabled.isEmpty()) {
                if (toolsArray == null) {
                    toolsArray = payload.putArray("tools");
                }
                for (ServerTool st : enabled) {
                    if (st.getId() instanceof OpenAiHostedTool ht) {
                        toolsArray.addObject().put("type", ht.getId());
                    }
                }
            }
        }

        ArrayNode input = payload.putArray("input");

        // 1. System Instructions (if not already in history)
        if (!request.config().getSystemInstructions().isEmpty()) {
            for (String si : request.config().getSystemInstructions()) {
                ObjectNode item = input.addObject();
                item.put("type", "message");
                item.put("role", "system");
                ArrayNode content = item.putArray("content");
                content.addObject().put("type", "input_text").put("text", si);
            }
        }

        // 2. History (Messages and Tool Calls)
        boolean includePruned = request.config().isIncludePruned();
        for (AbstractMessage msg : request.history()) {
            input.addAll(new OpenAiResponsesApiContentAdapter(msg, includePruned, getTokenizerType(),
                    getReasoningStyle(), getReasoningTags()).toItems());
        }

        // 3. Reasoning & Effort
        enrichPayload(payload, request);
        if (supportsReasoningEffort() && payload.has("reasoning")) {
            ((ObjectNode) payload.get("reasoning")).put("summary", "auto");
        }

        return payload;
    }

    private boolean isLegacyModel() {
        String id = getModelId().toLowerCase();
        return id.contains("-instruct") || id.contains("davinci") || id.contains("babbage") || id.contains("curie") || id.contains("ada");
    }

    private boolean supportsReasoningEffort() {
        String id = getModelId().toLowerCase();
        return id.startsWith("gpt-5") || id.startsWith("gpt-6") || id.startsWith("o1") || id.startsWith("o3") || id.startsWith("o4");
    }

    private boolean prefersResponsesApi() {
        String id = getModelId().toLowerCase();
        // Route GPT-5+, o-series, and modern GPT-4 models to the Responses API
        return id.startsWith("gpt-5") || id.startsWith("gpt-6") || id.startsWith("o4") || id.startsWith("o3") || id.contains("gpt-4o") || id.contains("gpt-4.1");
    }
}
