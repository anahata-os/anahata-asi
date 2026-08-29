/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractModelMessage;
import uno.anahata.asi.agi.message.AgiUserMessage;
import uno.anahata.asi.agi.message.BlobPart;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.message.TextPart;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ResponseModality;
import uno.anahata.asi.agi.provider.ThinkingLevel;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.internal.TextUtils;

/**
 * The definitive toolkit for managing and inspecting the ASI container and its
 * active sessions. This toolkit provides deep visibility into the 'Working
 * Memory' and 'Long-Term Context' of the ASI across all sessions.
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("Toolkit for managing and inspecting the ASI container and its active sessions.")
public class AsiContainer extends AnahataToolkit {

    /**
     * {@inheritDoc}
     * <p>
     * Provides core instructions on how to programmatically query the
     * container's AI providers and API keys from within NbJava scripts.</p>
     *
     * @throws Exception if an error occurs during instruction generation.
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        List<String> inst = new ArrayList<>(super.getSystemInstructions());
        inst.add("### The **AsiContainer** toolkit is a proxy toolkit for " + getAsiContainer().getClass().getName() + ". It provides some convenience, on-shot tools to query and manage sub agents.\n"
                + "Programmatic Container Access (from the java toolkit, if available:)\n"
                + "When scripting custom automation via the java toolkit, "
                + "you can programmatically query the ASI container's configurations, providers, and secure API keys:\n"
                + "1. Retrieve the Container: `AbstractAsiContainer container = getAsiContainer();`\n"
                + "2. Get a provider by id: `AbstractAiProvider provider= getProvider(\"Gemini\");`\n"
                + "3. Retrieve Active API Keys:\n"
                + "   - Get currently selected/rotated key: `String apiKey = provider.getCurrentKey();`\n"
                + "   - Trigger key rotation: `provider.hokusPocus();`"
        );
        return inst;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Populates the RAG message with container-level overview metadata,
     * including host application ID, working directory, default template
     * configuration, configured AI providers (summarized model counts), and
     * active AGI sessions.
     * </p>
     *
     * @param ragMessage The target RAG message to populate.
     * @throws java.lang.Exception If an error occurs during message population.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        AbstractAsiContainer container = getAsiContainer();

        StringBuilder sb = new StringBuilder();
        sb.append("## ASI Container Overview\n");
        sb.append("- **Host Application**: ").append(container.getHostApplicationId()).append("\n");
        sb.append("- **App Directory**: ").append(container.getAppDir()).append("\n");

        AgiConfig template = container.getPreferences() != null ? container.getPreferences().getAgiTemplate() : null;
        if (template != null) {
            sb.append("- **Default Provider UUID**: ").append(template.getSelectedProviderUuid() != null ? template.getSelectedProviderUuid() : "None").append("\n");
            sb.append("- **Default Model ID**: ").append(template.getSelectedModelId() != null ? template.getSelectedModelId() : "None").append("\n");
        }

        sb.append("\n### Configured AI Providers\n");
        sb.append(listAiProviders(false));

        List<Agi> activeAgis = container.getActiveAgis();
        if (activeAgis != null && !activeAgis.isEmpty()) {
            sb.append("\n### Active AGI Sessions\n");
            sb.append(listActiveAgis());
        }

        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Returns a Markdown table of all active AGI sessions in the container.
     *
     * @return A Markdown table listing sessions.
     */
    @AgiTool("Lists all active AGI sessions in the container.")
    public String listActiveAgis() {
        List<Agi> agis = getAsiContainer().getActiveAgis();
        if (agis.isEmpty()) {
            return "No active AGI sessions found in the container.";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("| Nickname | Session ID | Status | Open | History | Res | Context % |\n");
        sb.append("|---|---|---|---|---|---|---|\n");

        for (Agi agi : agis) {
            sb.append("| ").append(agi.getNickname() != null ? agi.getNickname() : "N/A")
                    .append(" | ").append(agi.getConfig().getSessionId())
                    .append(" | ").append(agi.getStatusManager().getCurrentStatus())
                    .append(" | ").append(agi.isOpen())
                    .append(" | ").append(agi.getContextManager().getHistory().size()).append(" msgs")
                    .append(" | ").append(agi.getResourceManager().getResourcesList().size())
                    .append(" | ").append(String.format("%.1f%%", agi.getContextWindowUsage() * 100))
                    .append(" |\n");
        }
        return sb.toString();
    }

    /**
     * Converts a list of models into a formatted Markdown table.
     *
     * @param models The list of models to format.
     * @return A Markdown formatted table.
     */
    public static String toMarkupTable(List<AbstractModel> models) {
        if (models == null || models.isEmpty()) {
            return "No models found.";
        }
        StringBuilder sb = new StringBuilder();
        sb.append(AbstractModel.MARKUP_TABLE_HEADER);
        for (AbstractModel m : models) {
            sb.append(m.toMarkupRow());
        }
        return sb.toString();
    }

    /**
     * Returns a Markdown table of all configured AI providers, including their
     * UUIDs, enabled status, class FQNs, endpoints, API key statuses, and model
     * counts or model IDs.
     *
     * @param includeModelIds Whether to include the full comma-separated list
     * of model IDs (true) or just the total count (false).
     * @return A Markdown formatted table summarizing the container's AI
     * providers.
     */
    @AgiTool("Lists all configured AI providers and their current status.")
    public String listAiProviders(
            @AgiToolParam(value = "Whether to include the full comma-separated list of model IDs or just the total count.", required = false) boolean includeModelIds) {
        List<AbstractAiProvider> providers = getAsiContainer().getAllProviders();
        if (providers.isEmpty()) {
            return "No registered AI providers found in the container.";
        }

        StringBuilder sb = new StringBuilder();
        sb.append(AbstractAiProvider.MARKUP_TABLE_HEADER);
        for (AbstractAiProvider p : providers) {
            sb.append(p.toMarkupRow(includeModelIds));
        }
        return sb.toString();
    }

    /**
     * Returns a Markdown table of available models for a specific AI provider
     * or all providers if providerUuid is null.
     *
     * @param providerUuid Optional unique UUID of the AI provider. If null or
     * empty, lists models across all providers.
     * @return A Markdown table of models.
     */
    @AgiTool("Lists all available models for a specific AI provider, or all models if providerUuid is null.")
    public String listAiModels(@AgiToolParam(value = "The unique UUID of the AI provider. If null, lists models for all providers.", required = false) String providerUuid) {
        if (providerUuid != null && !providerUuid.isBlank()) {
            AbstractAiProvider p = getAsiContainer().getProvider(providerUuid);
            if (p == null) {
                return "AI provider not found with UUID: " + providerUuid;
            }
            List<? extends AbstractModel> models = p.getModels();
            return toMarkupTable((List<AbstractModel>) models);
        }
        return toMarkupTable(getAsiContainer().getAllModels(false));
    }

    /**
     * Finds and filters AI models across all effectively enabled providers
     * using a regex or text query AND required response modalities.
     * <p>
     * The search evaluates an <b>AND condition</b>:
     * <ul>
     * <li><b>Query Regex</b>: If provided, matches case-insensitively against model ID,
     * display name, description, supported actions, or response modalities.</li>
     * <li><b>Response Modalities</b>: If provided (e.g.
     * {@code [IMAGE, TEXT]}), filters to models that support <b>ALL</b>
     * requested response modalities.</li>
     * </ul>
     * </p>
     *
     * @param query Optional regex or keyword query to match against model ID,
     * display name, description, actions, or modalities.
     * @param responseModalities Optional list of required response modalities (e.g.
     * [IMAGE], [AUDIO], [VIDEO]). The model must support all listed modalities.
     * @return A Markdown formatted table of matching models.
     */
    @AgiTool("Finds and filters AI models across all effectively enabled providers using a regex or text query and required response modalities.")
    public String findModels(
            @AgiToolParam(value = "Optional regex or keyword query to match against model ID, display name, description, supported actions, or modalities (e.g. 'image', 'video', 'claude', 'kling', 'glm', 'deepseek'). Evaluated using AND logic with responseModalities.", required = false) String query,
            @AgiToolParam(value = "Optional list of required response modalities (e.g. [IMAGE, AUDIO, VIDEO]). The model must support ALL specified modalities.", required = false) List<ResponseModality> responseModalities
    ) {
        List<AbstractModel> matches = getAsiContainer().findModels(query, responseModalities, true);
        if (matches.isEmpty()) {
            return "No models found matching query: '" + (query != null ? query : "") + "'"
                    + (responseModalities != null && !responseModalities.isEmpty() ? " with modalities: " + responseModalities : "");
        }
        return "### Found " + matches.size() + " Matching Model(s)\n\n" + toMarkupTable(matches);
    }

    /**
     * Returns detailed metadata for a specific AGI session, including its
     * enabled toolkits, context providers, and managed resources.
     *
     * @param sessionId The unique ID of the session.
     * @return A Markdown summary of the session details.
     */
    @AgiTool("Returns detailed metadata for a specific AGI session by its UUID.")
    public String getAgiDetails(@AgiToolParam("The unique ID of the session to inspect.") String sessionId) {
        Agi agi = getAsiContainer().getAgi(sessionId);
        StringBuilder sb = new StringBuilder();
        sb.append("### AGI Session Details: ").append(agi.getDisplayName()).append("\n\n");
        sb.append("## Current Session Metadata:\n");
        sb.append("- **AI Provider Class**: ").append(agi.getSelectedModel() != null && agi.getSelectedModel().getProvider() != null ? agi.getSelectedModel().getProvider().getClass().getName() : "None").append("\n");
        sb.append("- **AI Provider uuid**: ").append(agi.getSelectedModel() != null && agi.getSelectedModel().getProvider() != null ? agi.getSelectedModel().getProvider().getUuid() : "None").append("\n");
        sb.append("- **Model Class**: ").append(agi.getSelectedModel() != null ? agi.getSelectedModel().getClass().getName() : "None").append("\n");
        sb.append("- **Model Id**: ").append(agi.getSelectedModel() != null ? agi.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- **Thinking Level**: ").append(agi.getRequestConfig().getThinkingLevel()).append("\n");

        sb.append("- **Session ID**: ").append(agi.getConfig().getSessionId()).append("\n");
        sb.append("- **Nickname**: ").append(agi.getNickname()).append("\n");
        sb.append("- **Current Status**: ").append(agi.getStatusManager().getCurrentStatus()).append("\n");
        sb.append("- **Active Model**: ").append(agi.getSelectedModel() != null ? agi.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- **History Length**: ").append(agi.getContextManager().getHistory().size()).append(" messages\n");
        sb.append("- **Summary**: ").append(agi.getConversationSummary() != null ? agi.getConversationSummary() : "No summary available.").append("\n");

        // Enabled Toolkits (Single Line)
        String toolkits = agi.getToolManager().getEnabledToolkits().stream()
                .map(tk -> tk.getName())
                .collect(Collectors.joining(", "));
        sb.append("- **Enabled Toolkits**: ").append(toolkits.isEmpty() ? "None" : toolkits).append("\n");

        // Context Providers (Single Line)
        String providers = agi.getContextManager().getProviders().stream()
                .flatMap(root -> root.getFlattenedHierarchy(true).stream())
                .map(cp -> cp.getName() + " (EP: " + cp.isEffectivelyProviding() + ")")
                .collect(Collectors.joining(", "));
        sb.append("- **Context Providers**: ").append(providers.isEmpty() ? "None" : providers).append("\n");

        // Resources Table
        List<Resource> resources = agi.getResourceManager().getResourcesList();
        if (!resources.isEmpty()) {
            sb.append("\n#### Managed Resources\n\n");
            sb.append("| Name | UUID | Position | Policy | Mime |\n");
            sb.append("|---|---|---|---|---|\n");
            for (Resource r : resources) {
                sb.append("| ").append(r.getName())
                        .append(" | ").append(r.getId())
                        .append(" | ").append(r.getContextPosition())
                        .append(" | ").append(r.getRefreshPolicy())
                        .append(" | ").append(r.getMimeType())
                        .append(" |\n");
            }
        } else {
            sb.append("- **Resources**: None registered.\n");
        }

        return sb.toString();
    }

    /**
     * Creates a new AGI session with comprehensive configuration options.
     *
     * @param resourceURIs Optional list of resource URIs to register in the new
     * session.
     * @param aiProviderUUID Optional UUID of the AI provider to use. Will use
     * container default if null.
     * @param nickName the nickname for the new AGI
     * @param open Whether to open the new AGI session in the host UI.
     * @param autoReplyTools Whether to automatically execute tool calls for the
     * new session without waiting for manual user intervention.
     * @param toolPermissions Optional map of tool permission overrides for this
     * session (e.g. tool name -> PROMPT, APPROVE_ALWAYS, DENY).
     * @param initialMessage Optional message to send to the new AGI immediately
     * after creation.
     * @param modelID Optional ID of the AI model to select. Will use container
     * default if null.
     * @param toolkitFqns Optional list of fully qualified toolkit class names
     * to enable.
     * @param thinkingLevel the startup thinking level for the new AGI
     * @param responseModalities Optional list of response modalities for this
     * session (e.g. ['TEXT', 'IMAGE'], ['IMAGE'], ['AUDIO']).
     * @return A confirmation message with the new session ID.
     */
    @AgiTool("Creates a brand new AGI session with comprehensive configuration options.")
    public String createNewAgi(
            @AgiToolParam("Whether to open the new AGI session in the UI.") boolean open,
            @AgiToolParam("Whether to automatically execute tool calls for the new session without waiting for manual user intervention.") boolean autoReplyTools,
            @AgiToolParam(value = "Optional nickname for the new AGI session.", required = false) String nickName,
            @AgiToolParam(value = "The UUID of the AI provider to use. Will use the Asi Container default if not provided.", required = false) String aiProviderUUID,
            @AgiToolParam(value = "The ID of the AI model to use. Leave emtpy for default. Will use the Asi Container default if not provided", required = false) String modelID,
            @AgiToolParam(value = "List of toolkit fully qualified class names to enable. If not provided, will use all toolkits in the Asi Container preferences.", required = false) List<String> toolkitFqns,
            @AgiToolParam(value = "Optional List of resource URIs to register.", required = false) List<String> resourceURIs,
            @AgiToolParam(value = "An optional initial message to send to the new AGI.", required = false) String initialMessage,
            @AgiToolParam(value = "Optional map of tool permission overrides for this session. The key must be the exact tool name using '.' as separator between the toolkit name and the method name: e.g. 'NbJava.compileAndExecute' or 'Session.updateSessionNickname'. Do not include backticks or markdown quotes in the map key.", required = false) Map<String, ToolPermission> toolPermissions,
            @AgiToolParam(value = "Optional thinking level/mode for the new session.", required = false) ThinkingLevel thinkingLevel,
            @AgiToolParam(value = "Optional list of response modalities (e.g. [TEXT, IMAGE], [IMAGE], [AUDIO]).", required = false) List<ResponseModality> responseModalities
    ) {
        Agi newAgi = createNewAgiInternal(open, autoReplyTools, nickName, aiProviderUUID, modelID, toolkitFqns, resourceURIs, initialMessage, toolPermissions, thinkingLevel, responseModalities);
        return "Successfully created and registered new AGI session: " + newAgi.getConfig().getSessionId();
    }

    /**
     * Internal helper to construct, register, and bootstrap a new AGI session.
     *
     * @param open Whether to open the new AGI session in the host UI.
     * @param autoReplyTools Whether to automatically execute tool calls.
     * @param nickName Optional nickname.
     * @param aiProviderUUID Optional provider UUID.
     * @param modelID Optional model ID.
     * @param toolkitFqns Optional list of toolkit class FQNs.
     * @param resourceURIs Optional list of resource URIs.
     * @param initialMessage Optional initial message to send.
     * @param toolPermissions Optional tool permission overrides.
     * @param thinkingLevel Optional thinking level.
     * @param responseModalities Optional response modalities.
     * @return The newly created Agi instance.
     */
    public Agi createNewAgiInternal(
            boolean open,
            boolean autoReplyTools,
            String nickName,
            String aiProviderUUID,
            String modelID,
            List<String> toolkitFqns,
            List<String> resourceURIs,
            String initialMessage,
            Map<String, ToolPermission> toolPermissions,
            ThinkingLevel thinkingLevel,
            List<ResponseModality> responseModalities
    ) {
        AbstractAsiContainer container = getAsiContainer();
        AgiConfig config = container.createNewAgiConfig();

        // 1. Ancestry & Loop Configuration
        config.setParentUuid(getAgi().getConfig().getSessionId());
        config.setAutoReplyTools(autoReplyTools);

        // 2. Model & Provider Overrides
        if (aiProviderUUID != null) {
            config.setSelectedProviderUuid(aiProviderUUID);
        }
        if (modelID != null) {
            config.setSelectedModelId(modelID);
        }

        // 3. Toolkit Customization
        if (toolkitFqns != null) {
            config.getToolClasses().clear();
            for (String fqn : toolkitFqns) {
                try {
                    config.getToolClasses().add(Class.forName(fqn));
                } catch (ClassNotFoundException e) {
                    error("Failed to load toolkit class: " + fqn + " (" + e.getMessage() + ")");
                    error(e);
                }
            }
        }

        // 4. Atomic Creation & Registration
        Agi newAgi = container.createNewAgi(config);
        if (nickName != null && !nickName.isBlank()) {
            newAgi.setNickname(nickName);
        }
        if (thinkingLevel != null) {
            newAgi.getRequestConfig().setThinkingLevel(thinkingLevel);
        }
        if (responseModalities != null && !responseModalities.isEmpty()) {
            newAgi.getRequestConfig().setResponseModalities(new ArrayList<>(responseModalities));
        }

        // 5. Session-Level Tool Permission Overrides
        if (toolPermissions != null && !toolPermissions.isEmpty()) {
            for (Map.Entry<String, ToolPermission> entry : toolPermissions.entrySet()) {
                String toolName = entry.getKey();
                ToolPermission permission = entry.getValue();
                AbstractTool<?, ?> tool = newAgi.getToolManager().findToolByName(toolName).orElse(null);
                if (tool == null) {
                    error("disposing agi " + newAgi.getConfig().getSessionId() + " due to invalid tool permission key: " + toolName);
                    container.dispose(newAgi);
                    throw new AgiToolException("Invalid tool permission override: No tool found with name '" + toolName + "'. Available tools: " + newAgi.getToolManager().getAllToolNames());
                }
                tool.setPermission(permission);
            }
        }

        // 6. Resource Bootstrapping
        if (resourceURIs != null) {
            for (String uriStr : resourceURIs) {
                try {
                    URI uri = URI.create(uriStr);
                    newAgi.getResourceManager().registerHandle(config.createResourceHandle(uri),
                            "Spawned by session: " + getAgi().getDisplayName());
                } catch (Exception e) {
                    error("Failed to register resource URI '" + uriStr + "' in new session: " + e.getMessage());
                }
            }
        }

        // 7. Initial Prompting
        if (initialMessage != null && !initialMessage.isBlank()) {
            AgiUserMessage msg = new AgiUserMessage(newAgi, getAgi().getConfig().getSessionId());
            msg.addTextPart(initialMessage);
            newAgi.sendMessage(msg);
        }

        // 8. UI Visibility
        if (open) {
            container.open(newAgi);
        }

        return newAgi;
    }

    /**
     * Generates media (images, audio, video) using a generative AI model in a
     * single shot.
     *
     * @param prompt The descriptive prompt for the media to generate.
     * @param aiProviderUUID The UUID of the AI provider to use.
     * @param modelID The ID of the AI model to use.
     * @param resourceURIs Optional list of resource URIs (e.g. source image
     * files or URLs) to register as context for image-to-image or multimodal
     * generation.
     * @param responseModalities Optional list of response modalities (defaults
     * to ['TEXT', 'IMAGE']).
     * @param nickName Optional nickname for the background AGI session.
     * @param open Whether to open the sub-AGI session in the UI.
     * @param saveToPath Optional file path to save the generated media directly
     * to disk.
     * @param thinkingLevel Optional thinking level. If null or NONE, thoughts
     * are disabled for media generation.
     * @return A markdown formatted summary of the generation result.
     * @throws Exception If an error occurs during generation.
     */
    @AgiTool("Generates media (images, audio, video) using a generative AI model in a single shot.")
    public String generateMedia(
            @AgiToolParam("The descriptive prompt for the media to generate.") String prompt,
            @AgiToolParam(value = "The UUID of the AI provider to use.", required = false) String aiProviderUUID,
            @AgiToolParam(value = "The ID of the AI model to use.", required = false) String modelID,
            @AgiToolParam(value = "Optional list of resource URIs (e.g. images or documents) to provide to the model for image-to-image or editing.", required = false) List<String> resourceURIs,
            @AgiToolParam(value = "List of target response modalities. Defaults to [TEXT, IMAGE]. Can include IMAGE, AUDIO, VIDEO, TEXT.", required = false) List<ResponseModality> responseModalities,
            @AgiToolParam(value = "Optional nickname for the background AGI session. Defaults to 'Media Generation'.", required = false) String nickName,
            @AgiToolParam(value = "Whether to open the sub-AGI session in the UI. Defaults to false.", required = false) boolean open,
            @AgiToolParam(value = "Optional file path to save the generated media directly to disk.", required = false) String saveToPath,
            @AgiToolParam(value = "Optional thinking level/mode for generation.", required = false) ThinkingLevel thinkingLevel
    ) throws Exception {
        List<ResponseModality> modalities = (responseModalities != null && !responseModalities.isEmpty())
                ? new ArrayList<>(responseModalities)
                : new ArrayList<>(List.of(ResponseModality.TEXT, ResponseModality.IMAGE));

        String sessionNick = (nickName != null && !nickName.isBlank()) ? nickName : "Media Generation";

        // Spawn a sub-session with no toolkits
        Agi subAgi = createNewAgiInternal(open, false, sessionNick, aiProviderUUID, modelID, List.of(), resourceURIs, null, null, thinkingLevel, modalities);

        // If thinking level is null or NONE, disable includeThoughts specifically for media generation
        if (thinkingLevel == null || thinkingLevel == ThinkingLevel.NONE) {
            subAgi.getConfig().setIncludeThoughts(false);
        }

        // Send initial message
        AgiUserMessage msg = new AgiUserMessage(subAgi, getAgi().getConfig().getSessionId());
        msg.addTextPart(prompt);
        subAgi.sendMessage(msg);

        // Inspect generated output from history
        List<AbstractMessage> history = subAgi.getContextManager().getHistory();
        AbstractModelMessage lastModelMsg = null;
        for (int i = history.size() - 1; i >= 0; i--) {
            if (history.get(i) instanceof AbstractModelMessage mm) {
                lastModelMsg = mm;
                break;
            }
        }

        List<BlobPart> blobs = new ArrayList<>();
        List<TextPart> textParts = new ArrayList<>();
        if (lastModelMsg != null) {
            for (Object part : lastModelMsg.getParts()) {
                if (part instanceof BlobPart bp) {
                    blobs.add(bp);
                } else if (part instanceof TextPart tp && !tp.isThought()) {
                    textParts.add(tp);
                }
            }
        }

        StringBuilder resultSb = new StringBuilder();
        resultSb.append("### Media Generation Completed\n");
        resultSb.append("- **Session ID**: `").append(subAgi.getConfig().getSessionId()).append("` (").append(sessionNick).append(")\n");
        resultSb.append("- **Generated Media**: ").append(blobs.size()).append(" blob(s)\n");

        int blobIndex = 1;
        for (BlobPart bp : blobs) {
            addAttachment(bp.getData(), bp.getMimeType());
            resultSb.append("  - Blob #").append(blobIndex).append(": MIME `").append(bp.getMimeType())
                    .append("`, Size: ").append(TextUtils.formatSize(bp.getData().length)).append("\n");

            if (saveToPath != null && !saveToPath.isBlank()) {
                Path targetPath = Path.of(saveToPath);
                if (blobs.size() > 1) {
                    String fn = targetPath.getFileName().toString();
                    int dotIdx = fn.lastIndexOf('.');
                    String base = dotIdx != -1 ? fn.substring(0, dotIdx) : fn;
                    String ext = dotIdx != -1 ? fn.substring(dotIdx) : "";
                    targetPath = targetPath.resolveSibling(base + "_" + blobIndex + ext);
                }
                if (targetPath.getParent() != null) {
                    Files.createDirectories(targetPath.getParent());
                }
                Files.write(targetPath, bp.getData());
                resultSb.append("    - Saved to: `").append(targetPath.toAbsolutePath()).append("`\n");
            }
            blobIndex++;
        }

        if (!textParts.isEmpty()) {
            String combinedText = textParts.stream().map(TextPart::getText).filter(Objects::nonNull).collect(Collectors.joining("\n\n"));
            if (!combinedText.isBlank()) {
                resultSb.append("\n**Model Response**:\n").append(combinedText).append("\n");
            }
        }

        return resultSb.toString();
    }

    /**
     * Closes the UI tab/window of a specific active AGI session without
     * disposing it.
     *
     * @param sessionId The unique ID of the session to close.
     * @return A confirmation message.
     */
    @AgiTool("Closes the UI tab/window of a specific active AGI session without disposing it.")
    public String closeAgi(@AgiToolParam("The unique ID of the session to close.") String sessionId) {
        Agi targetAgi = getAsiContainer().getAgi(sessionId);
        if (!targetAgi.isOpen()) {
            return "Session " + sessionId + " (" + targetAgi.getDisplayName() + ") is already closed in the UI.";
        }

        getAsiContainer().close(targetAgi);
        return "Successfully closed UI tab for session: " + targetAgi.getDisplayName() + " (" + sessionId + ")";
    }

    /**
     * Permanently disposes of an active AGI session, closing its UI, shutting
     * down its executors, and archiving its session file.
     *
     * @param sessionId The unique ID of the session to dispose.
     * @return A confirmation message.
     */
    @AgiTool("Permanently disposes of an active AGI session, closing its UI and archiving its session file.")
    public String disposeAgi(@AgiToolParam("The unique ID of the session to dispose.") String sessionId) {
        Agi targetAgi = getAsiContainer().getAgi(sessionId);
        String displayName = targetAgi.getDisplayName();
        getAsiContainer().dispose(targetAgi);
        return "Successfully disposed and archived AGI session: " + displayName + " (" + sessionId + ")";
    }

    /**
     * Returns a plain text dump of the entire conversation history for a
     * session.
     *
     * @param sessionId Optional session ID. If null, the current session is
     * used.
     * @return A text dump of the history.
     */
    @AgiTool("Returns a plain text dump of the conversation history for a session. Does not include effectively pruned parts.")
    public String dumpHistory(@AgiToolParam("The unique ID of the session.") String sessionId) {
        Agi targetAgi = getAsiContainer().getAgi(sessionId);

        return targetAgi.getContextManager().getHistory().stream()
                .map(m -> String.format("[ID: %d | Role: %s | From: %s | Tokens: %d]\n%s",
                m.getSequentialId(), m.getRole(), m.getFrom(), m.getTokenCount(true), m.asText(false)))
                .collect(Collectors.joining("\n\n---\n\n"));
    }

}
