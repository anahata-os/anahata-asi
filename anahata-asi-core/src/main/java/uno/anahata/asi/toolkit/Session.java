/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.toolkit;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.context.ContextManager;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.PruningState;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolException;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;

/**
 * The definitive toolkit for managing the current agi session's metadata, tool
 * capabilities, and context pruning policies.
 * <p>
 * This toolkit provides high-level control over the session environment,
 * allowing the model to optimize its own context window, toggle available
 * toolkits, and manage the lifecycle of stateful resources.
 * </p>
 *
 * @author anahata-ai
 */
@Slf4j
@AiToolkit("Toolkit for managing the current agi session's metadata and context policies.")
public class Session extends AnahataToolkit {

    /**
     * The time when the session started.
     */
    private Date sessionStart = new Date();
    
    /**
     * The last time the session got deserialized.
     */
    private Date sessionRestored;
    
    /**
     * Captures the sessionRestored time during deserialization;
     */
    @Override
    public void postActivate() {
        sessionRestored = new Date();
    }
    
    /**
     * Updates the human-readable summary of the current session.
     * <p>
     * <b>STRICT USAGE RULE:</b> This tool MUST ONLY be called if there are
     * other "task-related" tool calls (e.g., file manipulation, shell commands,
     * pruning) being made in the same turn. It should NEVER be called as the
     * sole tool in a turn.
     * </p>
     * <p>
     * The summary is visible to the user in the container dashboard and helps
     * maintain continuity across session reloads.
     * </p>
     *
     * @param summary A concise summary of the conversation's current state or
     * topic.
     * @return A confirmation message.
     */
    @AiTool(value = "Updates the current AGI session's summary. This shows the ASI container's dashboard, update it with a brief summary of what you are doing or what you have just accomplished. "
            + "Usage: Call this if you are calling other real-task tools in the same batch, once per turn max, never as the only toll call in the turn.",
            requiresApproval = false)
    public String updateSessionSummary(@AiToolParam("A concise summary of the conversation's current state.") String summary) {
        uno.anahata.asi.agi.Agi domainAgi = getAgi();
        if (summary != null && !summary.isBlank()) {
            domainAgi.setSummary(summary);
        }
        log.info("Session summary updated: summary={}", summary);
        return "Session summary updated successfully.";
    }

    /**
     * Enables or disables specific context providers within the session
     * hierarchy.
     * <p>
     * This tool allows the model to surgically control which providers
     * contribute to the RAG message, helping to reduce noise and optimize token
     * usage.
     * </p>
     *
     * @param providing Whether to enable or disable the providers.
     * @param providerIds The fully qualified IDs of the context providers to
     * update.
     */
    @AiTool(value = "Sets the 'providing' flag of various context providers. Use this to enable/disable context providers. Disabl")
    public void setContextProviderProviding(
            @AiToolParam("Whether to enable or disable the providers.") boolean providing,
            @AiToolParam("The IDs of the context providers to update.") List<String> providerIds) {
        ContextManager cm = getAgi().getContextManager();
        for (ContextProvider root : cm.getProviders()) {
            for (ContextProvider cp : root.getFlattenedHierarchy(false)) {
                if (providerIds.contains(cp.getFullyQualifiedId())) {
                    cp.setProviding(providing);
                    log((providing ? "Enabled" : "Disabled") + " provider: " + cp.getName());
                }
            }
        }
    }

    /**
     * Enables or disables multiple toolkits by their names.
     * <p>
     * Disabling a toolkit makes its tools invisible to the model and can reduce
     * complexity when certain capabilities are no longer needed.
     * </p>
     *
     * @param enabled Whether to enable or disable the toolkits.
     * @param toolkitNames The names (IDs) of the toolkits to update (e.g.,
     * 'Audio', 'Chrome').
     */
    @AiTool("Enables or disables multiple toolkits by their names (IDs).")
    public void setToolkitEnabled(
            @AiToolParam("Whether to enable or disable.") boolean enabled,
            @AiToolParam("The names of the toolkits to update (e.g., 'Audio', 'Browser').") List<String> toolkitNames) {
        getAgi().getToolManager().updateToolkits(enabled, toolkitNames);
        log((enabled ? "Enabled" : "Disabled") + " toolkits: " + toolkitNames);
    }

    /**
     * Signals one or more currently executing background tools to stop.
     * <p>
     * This is useful for cancelling long-running tasks like large file reads,
     * complex shell commands, or audio recording.
     * </p>
     *
     * @param toolCallIds The unique IDs of the tool calls to stop.
     * @return A detailed report of the stopping operations.
     */
    @AiTool(value = "Stops one or more currently executing tools by their IDs.", requiresApproval = false)
    public String stopRunningTools(@AiToolParam("The unique IDs of the tool calls to stop.") List<String> toolCallIds) {
        List<AbstractToolCall<?, ?>> executing = getAgi().getToolManager().getExecutingCalls();
        int stoppedCount = 0;
        StringBuilder logBuilder = new StringBuilder();

        for (String id : toolCallIds) {
            AbstractToolCall<?, ?> call = executing.stream()
                    .filter(tc -> tc.getId().equals(id))
                    .findFirst()
                    .orElse(null);

            if (call != null) {
                if (call.getResponse().getStatus() == ToolExecutionStatus.EXECUTING) {
                    call.getResponse().stop();
                    stoppedCount++;
                    logBuilder.append("Stopped tool: ").append(call.getToolName()).append(" (ID: ").append(id).append(")\n");
                } else {
                    logBuilder.append("Did not stop tool: ").append(call.getToolName()).append(" (ID: ").append(id)
                            .append(") because its status is ").append(call.getResponse().getStatus()).append("\n");
                }
            } else {
                logBuilder.append("Tool call ID not found in executing list: ").append(id).append("\n");
            }
        }

        String result = logBuilder.toString();
        log.info("stopRunningTools result: {}", result);
        return stoppedCount + " tool(s) have been signaled to stop.\n" + result;
    }


    /**
     * Switches the session to server-side tool mode.
     * <p>
     * <b>CRITICAL:</b> After calling this, local Java tools are disabled until
     * manually re-enabled by the user. Use this only when a capability provided
     * by the model host (e.g., Google Search) is explicitly required.
     * </p>
     *
     * @return Confirmation message describing the impact.
     */
    @AiTool(value = "Disables local Java tools and enables hosted server tools (e.g., Google Search, Maps). "
            + "CRITICAL: After calling this, you will lose access to all local tools until the user manually reenables them "
            + "by clicking the Java icon in the toolbar. Use this only if you specifically need a server-side capability.",
            requiresApproval = true)
    public String enableHostedTools() {
        getAgi().getConfig().setHostedToolsEnabled(true);
        return "Server tools have been enabled. Local tools are now disabled. "
                + "You can now use tools like Google Search or Maps if supported by the model.";
    }

    /**
     * Populates the RAG message with comprehensive session metadata.
     * <p>
     * Provides a Markdown summary of session identity, model configuration,
     * context window usage, and the status of executing tools.
     * </p>
     *
     * @param ragMessage The target RAG message.
     * @throws Exception if metadata extraction fails.
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        uno.anahata.asi.agi.Agi domainAgi = ragMessage.getAgi();
        AgiConfig config = domainAgi.getConfig();
        StringBuilder sb = new StringBuilder();
        sb.append("## Current Session Metadata:\n");
        sb.append("- **Session ID**: ").append(config.getSessionId()).append("\n");
        sb.append("- **Start Time**: ").append(sessionStart).append("\n");
        sb.append("- **Last Restore Time**: ").append(sessionRestored != null ? sessionRestored : " This session has not been deserialized").append(" (The last time the session was deserialized) \n");
        sb.append("- **Nickname**: ").append(domainAgi.getNickname()).append("\n");
        sb.append("- **Display Name**: ").append(domainAgi.getDisplayName()).append("\n");
        sb.append("- **Summary**: ").append(domainAgi.getConversationSummary() != null ? domainAgi.getConversationSummary() : "N/A").append("\n");
        sb.append("- **Selected Model**: ").append(domainAgi.getSelectedModel() != null ? domainAgi.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- **Expand Thoughts**: ").append(config.isExpandThoughts()).append(config.isExpandThoughts() ? " (user's ui expands the thought parts with your reasoning when a new part arrives)" : "(**reasonig not showing**)\n");
        sb.append("- **Context Window Usage (previous turn)**: ").append(String.format("%.1f%%", domainAgi.getContextWindowUsage() * 100))
                .append(" (").append(domainAgi.getLastTotalTokenCount()).append(" / ").append(config.getTokenThreshold()).append(" tokens)\n");

        sb.append("\n Default Max Depth Policies:\n");
        sb.append("- **Text Parts**: ").append(config.getDefaultTextPartMaxDepth()).append("\n");
        sb.append("- **Model Thought Parts**: ").append(config.getDefaultThoughtPartMaxDepth()).append("\n");
        sb.append("- **Tool Calls**: ").append(config.getDefaultToolMaxDepth()).append("\n");
        sb.append("- **Blob Parts**: ").append(config.getDefaultBlobPartMaxDepth()).append("\n");        
        sb.append("*(Note: Individual tools or toolkits may override these defaults)*\n");

        sb.append("\n Capabilities:\n");
        sb.append("- **Local Java Tools**: ").append(config.isLocalToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");
        sb.append("- **Hosted Server Tools**: ").append(config.isHostedToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");

        if (!config.isHostedToolsEnabled() && domainAgi.getSelectedModel() != null) {
            List<ServerTool> serverTools = domainAgi.getSelectedModel().getAvailableServerTools();
            if (!serverTools.isEmpty()) {
                sb.append("\n Available Hosted Tools (Currently Disabled):\n");
                sb.append("The following tools are available but cannot be used while Local Tools are enabled. "
                        + "Use `enableHostedTools()` to switch modes.\n");
                for (ServerTool st : serverTools) {
                    sb.append("- **").append(st.getDisplayName()).append("**: ").append(st.getDescription()).append("\n");
                }
            }
        }

        List<AbstractToolCall<?, ?>> executing = domainAgi.getToolManager().getExecutingCalls();
        if (!executing.isEmpty()) {
            sb.append("\n- **Currently Executing Tools**: ");
            sb.append(executing.stream()
                    .map(tc -> tc.getToolName() + " (ID: " + tc.getId() + ")")
                    .collect(Collectors.joining(", ")));
            sb.append("\n");
        }

        ragMessage.addTextPart(sb.toString());
    }
}
