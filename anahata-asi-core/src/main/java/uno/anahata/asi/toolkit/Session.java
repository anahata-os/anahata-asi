/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.toolkit;

import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.PruningState;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.provider.ServerTool;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * The definitive toolkit for managing the current agi session's metadata, 
 * tool capabilities, and context pruning policies.
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
     * Updates the human-readable summary of the current session.
     * <p>
     * <b>STRICT USAGE RULE:</b> This tool MUST ONLY be called if there are other 
     * "task-related" tool calls (e.g., file manipulation, shell commands, pruning) 
     * being made in the same turn. It should NEVER be called as the sole tool 
     * in a turn.
     * </p>
     * <p>
     * The summary is visible to the user in the container dashboard and helps 
     * maintain continuity across session reloads.
     * </p>
     * 
     * @param summary A concise summary of the conversation's current state or topic.
     * @return A confirmation message.
     */
    @AiTool(value = "Updates the current agi session's summary. This shows in the ASI container's dashboard, update it with a brief summary of what you are doing or what you did if you are done." +
            "STRICT USAGE RULE: Only call this if you are calling other real-task related tools.",
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
     * Enables or disables specific context providers within the session hierarchy.
     * <p>
     * This tool allows the model to surgically control which providers contribute 
     * to the RAG message, helping to reduce noise and optimize token usage.
     * </p>
     * 
     * @param enabled Whether to enable or disable the providers.
     * @param providerIds The fully qualified IDs of the context providers to update.
     */
    @AiTool(value = "Enables or disables context providers.")
    public void updateContextProviders(
            @AiToolParam("Whether to enable or disable the providers.") boolean enabled, 
            @AiToolParam("The IDs of the context providers to update.") List<String> providerIds) {
        ContextManager cm = getAgi().getContextManager();
        for (ContextProvider root : cm.getProviders()) {
            for (ContextProvider cp : root.getFlattenedHierarchy(false)) {
                if (providerIds.contains(cp.getFullyQualifiedId())) {
                    cp.setProviding(enabled);
                    log((enabled ? "Enabled" : "Disabled") + " provider: " + cp.getName());
                }
            }
        }
    }

    /**
     * Enables or disables multiple toolkits by their names.
     * <p>
     * Disabling a toolkit makes its tools invisible to the model and can 
     * reduce complexity when certain capabilities are no longer needed.
     * </p>
     * 
     * @param enabled Whether to enable or disable the toolkits.
     * @param toolkitNames The names (IDs) of the toolkits to update (e.g., 'Audio', 'Chrome').
     */
    @AiTool("Enables or disables multiple toolkits by their names (IDs).")
    public void updateToolkits(
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
     * Updates the pruning policy for all parts of one or more messages.
     * <p>
     * Pruning states:
     * <ul>
     *   <li><b>PINNED</b>: Never garbage collected; always remains in context.</li>
     *   <li><b>PRUNED</b>: Excluded from the next API payload but kept in history.</li>
     *   <li><b>AUTO</b>: Standard lifecycle managed by the CwGC system.</li>
     * </ul>
     * </p>
     * 
     * @param messageIds The sequential IDs of the messages to update.
     * @param newState The new pruning state to apply.
     * @return Confirmation message.
     */
    @AiTool(value = "Bulk sets the pruning/pinning state of all parts for one or more messages. ")
    public String setMessagePruningState(
            @AiToolParam("The sequential IDs of the messages to update.") List<Long> messageIds,
            @AiToolParam("The new pruning state.") PruningState newState) {
        List<AbstractMessage> history = getAgi().getContextManager().getHistory();
        int count = 0;
        for (AbstractMessage msg : history) {
            if (messageIds.contains(msg.getSequentialId())) {
                switch(newState) {
                    case PINNED -> msg.pinAllParts();
                    case PRUNED -> msg.pruneAllParts();
                    case AUTO -> msg.setAutoAllParts();
                }
                count++;
            }
        }
        return "Updated " + count + " message(s).";
    }

    /**
     * Updates the pruning policy for specific message parts.
     * <p>
     * This tool allows for granular control over individual content segments, 
     * such as pinning a critical code block while allowing the rest of the 
     * message to be pruned.
     * </p>
     * 
     * @param partIds The sequential IDs of the parts to update.
     * @param newState The new pruning state.
     * @return Confirmation message.
     */
    @AiTool(value = "Sets the pruning/pinning state of one or more message parts. ")
    public String setPartPruningState(
            @AiToolParam("The sequential IDs of the parts to update.") List<Long> partIds,
            @AiToolParam("The new pruning state.") PruningState newState) {
        List<AbstractMessage> history = getAgi().getContextManager().getHistory();
        int count = 0;
        for (AbstractMessage msg : history) {
            for (AbstractPart part : msg.getParts(true)) {
                if (partIds.contains(part.getSequentialId())) {
                    part.setPruningState(newState);
                    count++;
                }
            }
        }
        return "Updated " + count + " part(s).";
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
    @AiTool(value = "Disables local Java tools and enables hosted server tools (e.g., Google Search, Maps). " +
            "CRITICAL: After calling this, you will lose access to all local tools until the user manually reenables them " +
            "by clicking the Java icon in the toolbar. Use this only if you specifically need a server-side capability.",
            requiresApproval = true)
    public String enableHostedTools() {
        getAgi().getConfig().setHostedToolsEnabled(true);
        return "Server tools have been enabled. Local tools are now disabled. " +
               "You can now use tools like Google Search or Maps if supported by the model.";
    }
    
    /**
     * Removes multiple managed resources from the active context in a single 
     * high-performance operation.
     * <p>
     * Uses the batch unregistration path in {@link uno.anahata.asi.resource.ResourceManager}. 
     * This ensures that the user receives descriptive feedback on exactly which 
     * resources were removed while the UI only rebuilds once.
     * </p>
     * 
     * @param resourceIds The unique IDs of the resources to unload.
     */
    @AiTool(value = "Removes the provided managed resources from the active workspace (the RAG message).")
    public void unloadResource(
            @AiToolParam("The unique IDs of the resources to unload.") List<String> resourceIds) {
        if (resourceIds == null || resourceIds.isEmpty()) {
            log("No resource IDs provided for unloading.");
            return;
        }

        List<AbstractResource<?, ?>> removed = getResourceManager().unregisterAll(resourceIds);
        
        if (removed.isEmpty()) {
            log("No resources were found matching the provided IDs.");
        } else {
            String names = removed.stream()
                    .map(AbstractResource::getName)
                    .collect(Collectors.joining(", "));
            log("Successfully unregistered " + removed.size() + " resources: " + names);
        }
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
        sb.append("- **Nickname**: ").append(domainAgi.getNickname()).append("\n");
        sb.append("- **Display Name**: ").append(domainAgi.getDisplayName()).append("\n");
        sb.append("- **Selected Model**: ").append(domainAgi.getSelectedModel() != null ? domainAgi.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- **Summary**: ").append(domainAgi.getConversationSummary() != null ? domainAgi.getConversationSummary() : "N/A").append("\n");
        sb.append("- **Expand Thoughts**: ").append(config.isExpandThoughts()).append(config.isExpandThoughts() ? " (user's ui expands the thought parts with your reasoning when a new part arrives)" : "(**reasonig not showing**)\n");
        sb.append("- **Total Messages**: ").append(domainAgi.getContextManager().getHistory().size()).append("\n");
        sb.append("- **Context Usage**: ").append(String.format("%.1f%%", domainAgi.getContextWindowUsage() * 100))
          .append(" (").append(domainAgi.getLastTotalTokenCount()).append(" / ").append(config.getTokenThreshold()).append(" tokens)\n");
        
        sb.append("\n Default Max Depth Policies:\n");
        sb.append("- **Text Parts**: ").append(config.getDefaultTextPartMaxDepth()).append("\n");
        sb.append("- **Tool Calls**: ").append(config.getDefaultToolMaxDepth()).append("\n");
        sb.append("- **Blob Parts**: ").append(config.getDefaultBlobPartMaxDepth()).append("\n");
        sb.append("*(Note: Individual tools or toolkits may override these defaults)*\n");
        
        sb.append("\n Capabilities:\n");
        sb.append("- **Local Java Tools**: ").append(config.isLocalToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");
        sb.append("- **Hosted Server Tools**: ").append(config.isHostedToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");

        if (!config.isHostedToolsEnabled() && domainAgi.getSelectedModel() != null) {
            List<ServerTool> serverTools = domainAgi.getSelectedModel().getAvailableServerTools();
            if (!serverTools.isEmpty()) {
                sb.append("\n Available Server Tools (Currently Disabled):\n");
                sb.append("The following tools are available but cannot be used while Local Tools are enabled. " +
                          "Use `enableHostedTools()` to switch modes.\n");
                for (ServerTool st : serverTools) {
                    sb.append("- **").append(st.getDisplayName()).append("**: ").append(st.getDescription()).append("\n");
                }
            }
        }

        List<AbstractToolCall<?, ?>> executing = domainAgi.getToolManager().getExecutingCalls();
        if (!executing.isEmpty()) {
            sb.append("\n- **Executing Tools**: ");
            sb.append(executing.stream()
                .map(tc -> tc.getToolName() + " (ID: " + tc.getId() + ")")
                .collect(Collectors.joining(", ")));
            sb.append("\n");
        }
        
        ragMessage.addTextPart(sb.toString());
    }
}
