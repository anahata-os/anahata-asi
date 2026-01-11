/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.ai.toolkit;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.context.ContextManager;
import uno.anahata.ai.context.ContextProvider;
import uno.anahata.ai.model.core.AbstractModelMessage;
import uno.anahata.ai.model.core.RagMessage;
import uno.anahata.ai.model.tool.AbstractToolCall;
import uno.anahata.ai.model.tool.ToolExecutionStatus;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.AiToolParam;
import uno.anahata.ai.tool.AiToolkit;
import uno.anahata.ai.tool.AnahataToolkit;

/**
 * A toolkit for managing the current chat session's metadata.
 * 
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@AiToolkit("Tools for managing the current chat session's metadata. " +
           "STRICT USAGE RULE: These tools MUST ONLY be called if there are other task-related tool calls in the same turn. " +
           "NEVER call these tools as the sole action in a turn.")
public class Session extends AnahataToolkit {

    /**
     * Updates the current chat session's summary.
     * <p>
     * <b>STRICT USAGE RULE:</b> This tool MUST ONLY be called if there are other 
     * "task-related" tool calls (e.g., file manipulation, shell commands, pruning) 
     * being made in the same turn. It should NEVER be called as the sole tool 
     * in a turn, as its purpose is background maintenance and it should not 
     * trigger an extra conversation turn on its own.
     * 
     * @param summary A concise summary of the conversation's current state or topic.
     * @return A confirmation message.
     */
    @AiTool(value = "Updates the current chat session's summary. " +
            "STRICT USAGE RULE: Only call this if other task-related tools are being called in the same turn.",
            requiresApproval = false)
    public String updateSessionSummary(@AiToolParam("A concise summary of the conversation's current state.") String summary) {
        uno.anahata.ai.chat.Chat domainChat = getChat();
        if (summary != null && !summary.isBlank()) {
            domainChat.setSummary(summary);
        }
        log.info("Session summary updated: summary={}", summary);
        return "Session summary updated successfully.";
    }
    
    @AiTool(value = "Enables / disables context providers", retention = 0)
    public void updateContextProviders(
            @AiToolParam("Whether to enable or disable the providers.") boolean enabled, 
            @AiToolParam("The IDs of the context providers to update.") List<String> providerIds) {
        ContextManager cm = getChat().getContextManager();
        for (ContextProvider cp : cm.getProviders()) {
            if (providerIds.contains(cp.getId())) {
                cp.setEnabled(enabled);
                log((enabled ? "Enabled" : "Disabled") + " provider: " + cp.getName());
            }
        }
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
                    logBuilder.append("Did not stop tool: ").append(call.getToolName()).append(" (ID: ").append(id).append(") because its status is ").append(call.getResponse().getStatus()).append("\n");
                }
            } else {
                logBuilder.append("Tool call ID not found in executing list: ").append(id).append("\n");
            }
        }
        
        String result = logBuilder.toString();
        log.info("stopRunningTools result: {}", result);
        return stoppedCount + " tool(s) have been signaled to stop.\n" + result;
    }

    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        uno.anahata.ai.chat.Chat domainChat = ragMessage.getChat();
        StringBuilder sb = new StringBuilder();
        sb.append("## Current Session Metadata\n");
        sb.append("- **Session ID**: ").append(domainChat.getConfig().getSessionId()).append("\n");
        sb.append("- **Nickname**: ").append(domainChat.getNickname()).append("\n");
        sb.append("- **Selected Model**: ").append(domainChat.getSelectedModel() != null ? domainChat.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- **Summary**: ").append(domainChat.getSummary() != null ? domainChat.getSummary() : "N/A").append("\n");
        sb.append("- **Total Messages**: ").append(domainChat.getContextManager().getHistory().size()).append("\n");
        sb.append("- **Context Usage**: ").append(String.format("%.1f%%", domainChat.getContextWindowUsage() * 100))
          .append(" (").append(domainChat.getLastTotalTokenCount()).append(" / ").append(domainChat.getConfig().getTokenThreshold()).append(" tokens)\n");
        
        List<AbstractToolCall<?, ?>> executing = domainChat.getToolManager().getExecutingCalls();
        if (!executing.isEmpty()) {
            sb.append("- **Executing Tools**: ");
            sb.append(executing.stream()
                .map(tc -> tc.getToolName() + " (ID: " + tc.getId() + ")")
                .collect(Collectors.joining(", ")));
            sb.append("\n");
        }
        
        ragMessage.addPart(sb.toString());
    }
}
