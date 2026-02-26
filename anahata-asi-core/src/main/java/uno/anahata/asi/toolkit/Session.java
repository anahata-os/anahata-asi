/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.toolkit;

import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.ChatConfig;
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
 * A toolkit for managing the current chat session's metadata and context policies.
 * 
 * @author anahata
 */
@Slf4j
@AiToolkit("Toolkit for managing the current chat session's metadata and context policies.")
public class Session extends AnahataToolkit {

    /**
     * Updates the current chat session's summary.
     * <p>
     * <b>STRICT USAGE RULE:</b> This tool MUST ONLY be called if there are other 
     * "task-related" tool calls (e.g., file manipulation, shell commands, pruning) 
     * being made in the same turn. It should NEVER be called as the sole tool 
     * in a turn.
     * 
     * @param summary A concise summary of the conversation's current state or topic.
     * @return A confirmation message.
     */
    @AiTool(value = "Updates the current chat session's summary. This shows in the ASI container's dashboard, update it with a brief summary of what you are doing or what you did if you are done." +
            "STRICT USAGE RULE: Only call this if you are calling other real-task related tools.",
            requiresApproval = false)
    public String updateSessionSummary(@AiToolParam("A concise summary of the conversation's current state.") String summary) {
        uno.anahata.asi.chat.Chat domainChat = getChat();
        if (summary != null && !summary.isBlank()) {
            domainChat.setSummary(summary);
        }
        log.info("Session summary updated: summary={}", summary);
        return "Session summary updated successfully.";
    }
    
    @AiTool(value = "Enables or disables context providers.")
    public void updateContextProviders(
            @AiToolParam("Whether to enable or disable the providers.") boolean enabled, 
            @AiToolParam("The IDs of the context providers to update.") List<String> providerIds) {
        ContextManager cm = getChat().getContextManager();
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
     * Enables or disables multiple toolkits by their names (IDs).
     * 
     * @param enabled Whether to enable or disable the toolkits.
     * @param toolkitNames The names (IDs) of the toolkits to update (e.g., 'Audio', 'Browser').
     */
    @AiTool("Enables or disables multiple toolkits by their names (IDs).")
    public void updateToolkits(
            @AiToolParam("Whether to enable or disable.") boolean enabled, 
            @AiToolParam("The names of the toolkits to update (e.g., 'Audio', 'Browser').") List<String> toolkitNames) {
        getChat().getToolManager().updateToolkits(enabled, toolkitNames);
        log((enabled ? "Enabled" : "Disabled") + " toolkits: " + toolkitNames);
    }

    @AiTool(value = "Stops one or more currently executing tools by their IDs.", requiresApproval = false)
    public String stopRunningTools(@AiToolParam("The unique IDs of the tool calls to stop.") List<String> toolCallIds) {
        List<AbstractToolCall<?, ?>> executing = getChat().getToolManager().getExecutingCalls();
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

    @AiTool(value = "Bulk sets the pruning/pinning state of all parts for one or more messages. ")
    public String setMessagePruningState(
            @AiToolParam("The sequential IDs of the messages to update.") List<Long> messageIds,
            @AiToolParam("The new pruning state.") PruningState newState) {
        List<AbstractMessage> history = getChat().getContextManager().getHistory();
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

    @AiTool(value = "Sets the pruning/pinning state of one or more message parts. ")
    public String setPartPruningState(
            @AiToolParam("The sequential IDs of the parts to update.") List<Long> partIds,
            @AiToolParam("The new pruning state.") PruningState newState) {
        List<AbstractMessage> history = getChat().getContextManager().getHistory();
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

    @AiTool(value = "Disables local Java tools and enables hosted server tools (e.g., Google Search, Maps). " +
            "CRITICAL: After calling this, you will lose access to all local tools until the user manually reenables them " +
            "by clicking the Java icon in the toolbar. Use this only if you specifically need a server-side capability.",
            requiresApproval = true)
    public String enableHostedTools() {
        getChat().getConfig().setHostedToolsEnabled(true);
        return "Server tools have been enabled. Local tools are now disabled. " +
               "You can now use tools like Google Search or Maps if supported by the model.";
    }
    
    /**
     * Removes the provided managed resources from the active workspace.
     * 
     * @param resourceIds The unique IDs of the resources to unload.
     * @throws Exception if an error occurs during unregistration.
     */
    @AiTool(value = "Removes the provided managed resources from the active workspace (the RAG message).")
    public void unloadResource(
            @AiToolParam("The unique IDs of the resources to unload.") List<String> resourceIds) throws Exception {
        ResourceManager rm = getResourceManager();
        for (String resourceId : resourceIds) {
            AbstractResource<?, ?> resource = rm.getResource(resourceId);
            String displayName = resource != null ? resource.getName() : resourceId;
            log("Unregistering... " + displayName);
            rm.unregister(resourceId);
            log("Unregistered OK " + displayName);
        }
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        uno.anahata.asi.chat.Chat domainChat = ragMessage.getChat();
        ChatConfig config = domainChat.getConfig();
        StringBuilder sb = new StringBuilder();
        sb.append("## Current Session Metadata:\n");
        sb.append("- **Session ID**: ").append(config.getSessionId()).append("\n");
        sb.append("- **Nickname**: ").append(domainChat.getNickname()).append("\n");
        sb.append("- **Display Name**: ").append(domainChat.getDisplayName()).append("\n");
        sb.append("- **Selected Model**: ").append(domainChat.getSelectedModel() != null ? domainChat.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- **Summary**: ").append(domainChat.getConversationSummary() != null ? domainChat.getConversationSummary() : "N/A").append("\n");
        sb.append("- **Expand Thoughts**: ").append(config.isExpandThoughts()).append(config.isExpandThoughts() ? " (user's ui expands the thought parts with your reasoning when a new part arrives)" : "(**reasonig not showing**)\n");
        sb.append("- **Total Messages**: ").append(domainChat.getContextManager().getHistory().size()).append("\n");
        sb.append("- **Context Usage**: ").append(String.format("%.1f%%", domainChat.getContextWindowUsage() * 100))
          .append(" (").append(domainChat.getLastTotalTokenCount()).append(" / ").append(config.getTokenThreshold()).append(" tokens)\n");
        
        sb.append("\n### Default Max Depth Policies:\n");
        sb.append("- **Text Parts**: ").append(config.getDefaultTextPartMaxDepth()).append("\n");
        sb.append("- **Tool Calls**: ").append(config.getDefaultToolMaxDepth()).append("\n");
        sb.append("- **Blob Parts**: ").append(config.getDefaultBlobPartMaxDepth()).append("\n");
        sb.append("*(Note: Individual tools or toolkits may override these defaults)*\n");
        
        sb.append("\n### Capabilities:\n");
        sb.append("- **Local Java Tools**: ").append(config.isLocalToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");
        sb.append("- **Hosted Server Tools**: ").append(config.isHostedToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");

        if (!config.isHostedToolsEnabled() && domainChat.getSelectedModel() != null) {
            List<ServerTool> serverTools = domainChat.getSelectedModel().getAvailableServerTools();
            if (!serverTools.isEmpty()) {
                sb.append("\n#### Available Server Tools (Currently Disabled):\n");
                sb.append("The following tools are available but cannot be used while Local Tools are enabled. " +
                          "Use `enableHostedTools()` to switch modes.\n");
                for (ServerTool st : serverTools) {
                    sb.append("- **").append(st.getDisplayName()).append("**: ").append(st.getDescription()).append("\n");
                }
            }
        }

        List<AbstractToolCall<?, ?>> executing = domainChat.getToolManager().getExecutingCalls();
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
