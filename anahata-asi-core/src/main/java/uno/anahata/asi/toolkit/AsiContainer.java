/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit;

import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.AbstractAsiContainer;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.message.AgiUserMessage;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiTool;
import uno.anahata.asi.agi.tool.AgiToolParam;

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
     * Returns detailed metadata for a specific AGI session, including its
     * enabled toolkits, context providers, and managed resources.
     *
     * @param sessionId The unique ID of the session.
     * @return A Markdown summary of the session details.
     */
    @AgiTool("Returns detailed metadata for a specific AGI session by its UUID.")
    public String getAgiDetails(@AgiToolParam("The unique ID of the session to inspect.") String sessionId) {
        return getAsiContainer().getActiveAgis().stream()
                .filter(agi -> agi.getConfig().getSessionId().equals(sessionId))
                .findFirst()
                .map(agi -> {
                    StringBuilder sb = new StringBuilder();
                    sb.append("### AGI Session Details: ").append(agi.getDisplayName()).append("\n\n");
                    sb.append("## Current Session Metadata:\n");
                    sb.append("- **AI Provider Class**: ").append(agi.getSelectedModel().getProvider().getClass());
                    sb.append("- **AI Provider uuid**: ").append(agi.getSelectedModel().getProvider().getUuid());
                    sb.append("- **Model Class **: ").append(agi.getSelectedModel() != null ? agi.getSelectedModel().getClass().getName(): "None").append("\n");
                    sb.append("- **Model Id **: ").append(agi.getSelectedModel() != null ? agi.getSelectedModel().getModelId() : "None").append("\n");
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
                })
                .orElse("No session found with ID: " + sessionId);
    }

    /**
     * Creates a new AGI session with optional model and tool configuration.
     *
     * @param open Whether to open the new AGI session in the host UI.
     * @param agiProviderUUID Optional UUID of the AI provider to use.
     * @param modelID Optional ID of the AI model to select.
     * @param toolkitFqns Optional list of fully qualified toolkit class names
     * to enable.
     * @param resourceURIs Optional list of resource URIs to register in the new
     * session.
     * @param initialMessage Optional message to send to the new AGI immediately
     * after creation.
     * @return A confirmation message with the new session ID.
     */
    @AgiTool("Creates a brand new AGI session with comprehensive configuration options.")
    public String createNewAgi(
            @AgiToolParam("Whether to open the new AGI session in the UI.") boolean open,
            @AgiToolParam(value = "The UUID of the AI provider to use. Will use the Asi Container default if not provided.", required = false) String agiProviderUUID,
            @AgiToolParam(value = "The ID of the AI model to use. Leave emtpy for default. Will use the Asi Container default if not provided", required = false) String modelID,
            @AgiToolParam(value = "List of toolkit fully qualified class names to enable. If not provided, will use all toolkits in the Asi Container preferences.", required = false) List<String> toolkitFqns,
            @AgiToolParam(value = "Optional List of resource URIs to register.", required = false) List<String> resourceURIs,
            @AgiToolParam(value = "An optional initial message to send to the new AGI.", required = false) String initialMessage
    ) {
        AbstractAsiContainer container = getAsiContainer();
        AgiConfig config = container.createNewAgiConfig();

        // 1. Ancestry Tracking
        config.setParentUuid(getAgi().getConfig().getSessionId());

        // 2. Model & Provider Overrides
        if (agiProviderUUID != null) {
            config.setSelectedProviderUuid(agiProviderUUID);
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

        // 5. Resource Bootstrapping
        if (resourceURIs != null) {
            for (String uriStr : resourceURIs) {
                try {
                    java.net.URI uri = java.net.URI.create(uriStr);
                    newAgi.getResourceManager().registerHandle(config.createResourceHandle(uri),
                            "Spawned by session: " + getAgi().getDisplayName());
                } catch (Exception e) {
                    error("Failed to register resource URI '" + uriStr + "' in new session: " + e.getMessage());
                }
            }
        }

        // 6. Initial Prompting
        if (initialMessage != null && !initialMessage.isBlank()) {
            AgiUserMessage msg = new AgiUserMessage(newAgi, getAgi().getConfig().getSessionId());
            msg.addTextPart(initialMessage);
            newAgi.sendMessage(msg);
        }

        // 7. UI Visibility
        if (open) {
            container.open(newAgi);
        }

        return "Successfully created and registered new AGI session: " + newAgi.getConfig().getSessionId();
    }

    /**
     * Returns a plain text dump of the entire conversation history for a
     * session.
     *
     * @param sessionId Optional session ID. If null, the current session is
     * used.
     * @return A text dump of the history.
     */
    @AgiTool("Returns a plain text dump of the conversation history for a session.")
    public String dumpHistory(@AgiToolParam("The unique ID of the session. If null, uses the current session.") String sessionId) {
        Agi targetAgi = getAgi();
        if (sessionId != null) {
            targetAgi = getAsiContainer().getActiveAgis().stream()
                    .filter(a -> a.getConfig().getSessionId().equals(sessionId))
                    .findFirst().orElse(null);
        }

        if (targetAgi == null) {
            return "Session not found.";
        }

        return targetAgi.getContextManager().getHistory().stream()
                .map(m -> String.format("[ID: %d | Role: %s | From: %s]\n%s",
                m.getSequentialId(), m.getRole(), m.getFrom(), m.asText(true)))
                .collect(Collectors.joining("\n\n---\n\n"));
    }
}
