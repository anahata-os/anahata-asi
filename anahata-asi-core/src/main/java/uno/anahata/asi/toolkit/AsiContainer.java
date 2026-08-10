/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.AgiUserMessage;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.provider.AbstractAiProvider;
import uno.anahata.asi.agi.provider.AbstractModel;
import uno.anahata.asi.agi.provider.ThinkingLevel;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.tool.spi.AbstractTool;

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
     * Populates the RAG message with container-level overview metadata, including host application ID, working directory, default template configuration, configured AI providers (summarized model counts), and active AGI sessions.
     * </p>
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
     * Returns a Markdown table of all configured AI providers, including their UUIDs, enabled status, class FQNs, endpoints, API key statuses, and model counts or model IDs.
     * @param includeModelIds Whether to include the full comma-separated list of model IDs (true) or just the total count (false).
     * @return A Markdown formatted table summarizing the container's AI providers.
     */
    @AgiTool("Lists all configured AI providers and their current status.")
    public String listAiProviders(
            @AgiToolParam(value = "Whether to include the full comma-separated list of model IDs or just the total count.", required = false) boolean includeModelIds) {
        List<AbstractAiProvider> providers = getAsiContainer().getAllProviders();
        if (providers.isEmpty()) {
            return "No registered AI providers found in the container.";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("| Display Name | UUID | Enabled | Provider Class | Base URL | Key Configured | Models |\n");
        sb.append("|---|---|---|---|---|---|---|\n");

        for (AbstractAiProvider p : providers) {
            String modelsList;
            if (p.isEnabled() && (!p.isApiKeyRequired() || p.hasKeys())) {
                List<? extends AbstractModel> models = p.getModels();
                if (includeModelIds) {
                    modelsList = (models != null && !models.isEmpty())
                            ? models.stream().map(AbstractModel::getModelId).collect(Collectors.joining(", "))
                            : "None";
                } else {
                    modelsList = (models != null) ? String.valueOf(models.size()) : "0";
                }
            } else {
                modelsList = "N/A (Disabled)";
            }

            sb.append("| ").append(p.getDisplayName() != null ? p.getDisplayName() : "N/A")
                    .append(" | ").append(p.getUuid())
                    .append(" | ").append(p.isEnabled() ? "✅ YES" : "❌ NO")
                    .append(" | ").append(p.getClass().getName())
                    .append(" | ").append(p.getBaseUrl() != null ? p.getBaseUrl() : "Default Cloud")
                    .append(" | ").append(p.hasKeys() ? "✅ YES" : "❌ NO (Required: " + p.isApiKeyRequired() + ")")
                    .append(" | ").append(modelsList)
                    .append(" |\n");
        }
        return sb.toString();
    }

    /**
     * Returns a Markdown table of available models for a specific AI provider or all providers if providerUuid is null.
     * @param providerUuid Optional unique UUID of the AI provider. If null or empty, lists models across all providers.
     * @return A Markdown table of models.
     */
    @AgiTool("Lists all available models for a specific AI provider, or all models if providerUuid is null.")
    public String listAiModels(@AgiToolParam(value = "The unique UUID of the AI provider. If null, lists models for all providers.", required = false) String providerUuid) {
        List<AbstractAiProvider> targetProviders;
        if (providerUuid != null && !providerUuid.isBlank()) {
            AbstractAiProvider p = getAsiContainer().getProvider(providerUuid);
            if (p == null) {
                return "AI provider not found with UUID: " + providerUuid;
            }
            targetProviders = List.of(p);
        } else {
            targetProviders = getAsiContainer().getAllProviders();
        }

        if (targetProviders.isEmpty()) {
            return "No registered AI providers found in the container.";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("| Provider UUID | Provider Name | Enabled | Model ID | Display Name | Input Tokens | Output Tokens | Actions |\n");
        sb.append("|---|---|---|---|---|---|---|---|\n");

        for (AbstractAiProvider p : targetProviders) {
            String uuid = p.getUuid();
            String name = p.getDisplayName() != null ? p.getDisplayName() : "N/A";

            if (!p.isEnabled()) {
                sb.append("| ").append(uuid)
                        .append(" | ").append(name)
                        .append(" | ❌ NO | | | | | |\n");
                continue;
            }

            List<? extends AbstractModel> models = p.getModels();
            if (models == null || models.isEmpty()) {
                sb.append("| ").append(uuid)
                        .append(" | ").append(name)
                        .append(" | ✅ YES | None | No models configured | | | |\n");
            } else {
                for (AbstractModel m : models) {
                    String actions = m.getSupportedActions() != null ? String.join(", ", m.getSupportedActions()) : "N/A";
                    sb.append("| ").append(uuid)
                            .append(" | ").append(name)
                            .append(" | ✅ YES")
                            .append(" | ").append(m.getModelId())
                            .append(" | ").append(m.getDisplayName() != null ? m.getDisplayName() : "N/A")
                            .append(" | ").append(m.getMaxInputTokens() != null ? m.getMaxInputTokens() : "Unbounded")
                            .append(" | ").append(m.getMaxOutputTokens() != null ? m.getMaxOutputTokens() : "Unbounded")
                            .append(" | ").append(actions)
                            .append(" |\n");
                }
            }
        }
        return sb.toString();
    }

    /**
     * Returns detailed metadata for a specific AGI session, including its enabled toolkits, context providers, and managed resources.
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
     * @param resourceURIs Optional list of resource URIs to register in the new session.
     * @param aiProviderUUID Optional UUID of the AI provider to use. Will use container default if null.
     * @param nickName the nickname for the new AGI
     * @param open Whether to open the new AGI session in the host UI.
     * @param autoReplyTools Whether to automatically execute tool calls for the new session without waiting for manual user intervention.
     * @param toolPermissions Optional map of tool permission overrides for this session (e.g. tool name -> PROMPT, APPROVE_ALWAYS, DENY).
     * @param initialMessage Optional message to send to the new AGI immediately after creation.
     * @param modelID Optional ID of the AI model to select. Will use container default if null.
     * @param toolkitFqns Optional list of fully qualified toolkit class names to enable.
     * @param thinkingLevel the startup thinking level for the new AGI
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
            @AgiToolParam(value = "Optional thinking level/mode for the new session.", required = false) ThinkingLevel thinkingLevel
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
        if (toolkitFqns != null && !toolkitFqns.isEmpty()) {
            config.getToolClasses().clear();
            for (String fqn : toolkitFqns) {
                try {
                    config.getToolClasses().add(Class.forName(fqn));
                } catch (ClassNotFoundException e) {
                    error("Failed to load toolkit class: " + fqn + " (" + e.getMessage() + ")");
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

        return "Successfully created and registered new AGI session: " + newAgi.getConfig().getSessionId();
    }

    /**
     * Closes the UI tab/window of a specific active AGI session without disposing it.
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
     * Permanently disposes of an active AGI session, closing its UI, shutting down its executors, and archiving its session file.
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
