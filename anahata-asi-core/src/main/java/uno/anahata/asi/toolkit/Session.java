/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.toolkit;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.chat.ChatConfig;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.model.core.AbstractMessage;
import uno.anahata.asi.model.core.AbstractModelMessage;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.AbstractToolMessage;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.provider.ServerTool;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.tool.AbstractToolCall;
import uno.anahata.asi.model.tool.AbstractToolResponse;
import uno.anahata.asi.model.tool.ToolExecutionStatus;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for managing the current chat session's metadata and context policies.
 * 
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
@AiToolkit("Toolkit for managing the current chat session's metadata and context policies.")
public class Session extends AnahataToolkit {

    @Override
    public List<String> getSystemInstructions() throws Exception {
        return List.of(
            "## MISSION CRITICAL: FRAMEWORK INTEGRITY & METADATA PROTOCOL\n" +
            "1. **METADATA SYNTAX**: The framework uses 'in-band' metadata to maintain structural awareness. \n" +
            "   - **Message Headers**: `--- Message ID: <N> | Role: <ROLE> | ... ---` marks the start of a message.\n" +
            "   - **Part Headers**: `[Part ID: <N> | Type: <TYPE> | ...]` marks the start of an individual part (text, blob, tool call) within a message.\n" +
            "2. **RUNTIME INJECTION**: This metadata is **injected at runtime** by the server-side framework. It is NOT part of the actual message content produced by the user or the model.\n" +
            "3. **THE PURPOSE OF METADATA**: Metadata allows the framework and you (the model) to track the conversation using a 'Sinking Stack' model. Message and Part IDs are required for you to identify components for selective **Pruning** and **Pinning** via the `Session` toolkit.\n" +
            "4. **FORBIDDEN GENERATION**: You are **strictly forbidden** from generating this metadata syntax yourself. \n" +
            "   - You **never** need to output `--- Message ID` or `[Part ID` to execute tools or communicate with the framework.\n" +
            "   - Including these headers in your response is a **critical hallucination** that disrupts the user experience and breaks UI rendering.\n",

            "## CWGC Protocol (Context Window Garbage Collection)\n" +
            "The conversation history is managed based on 'Depth' (distance from the current turn).\n" +
            "- **Auto-Pruning (`pruned=null`)**: Parts are automatically removed from your prompt when their depth exceeds their `maxDepth`.\n" +
            "- **Soft-Pruning (`pruned=true`)**: Parts are hidden from your prompt but preserved in the UI and history. You can still see their metadata headers in your context.\n" +
            "- **Pinning (`pruned=false`)**: Parts are immune to auto-pruning and remain in your prompt indefinitely until manually unpinned.\n",

            "## STRICT RESOURCE DISCIPLINE\n" +
            "1. **SOURCE OF TRUTH**: The `--- Augmented Workspace Context ---` in the RAG message (the first message) is your definitive list of active resources.\n" +
            "2. **NO REDUNDANT LOADING**: If a file or project is already listed in your context with `providing: true`, it is **LIVE** and automatically updated. \n" +
            "   - **NEVER** call `loadTextFile` or `loadBinaryFile` or any other tool that adds a resource to context for a resource that is already in context.\n" +
            "   - **NEVER** call `unloadResource` followed by `loadTextFile` for the same file; this is destructive and inefficient.\n",

            "## User Agency & Thought Process\n" +
            "- **User Control**: The user is the ultimate authority. They can manually remove or change the pruned state of any part or any message to (prune/pin/auto). Pruning ore removing a model message automatically prunes its associated tool responses.\n" +
            "- **Thought Process Visibility**: The `Expand Thoughts` flag in the session metadata tells you if the user can see your reasoning. If `false`, your reasoning is invisible; ensure your final text is self-contained and answers the user directly.\n" 
            );
    }

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
    @AiTool(value = "Updates the current chat session's summary. " +
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

    @AiTool(value = "Sets the pruned/pinned state of one or more messages. " +
            "Use newValue=true to prune (soft-prune), newValue=false to pin, and newValue=null to reset to auto-pruning.")
    public String setMessagePruned(
            @AiToolParam("The sequential IDs of the messages to update.") List<Long> messageIds,
            @AiToolParam("The new pruned state (true=pruned, false=pinned, null=auto).") Boolean newValue,
            @AiToolParam(value = "The reason for pruning.", required = false) String prunedReason) {
        List<AbstractMessage> history = getChat().getContextManager().getHistory();
        int count = 0;
        for (AbstractMessage msg : history) {
            if (messageIds.contains(msg.getSequentialId())) {
                // Redirection: If targeting a Tool Message, apply to the parent Model Message instead.
                // (Pruning propagation will handle the bidirectional sync anyway).
                if (msg instanceof AbstractToolMessage atm && atm.getModelMessage() != null) {
                    atm.getModelMessage().setPruned(newValue, prunedReason);
                } else {
                    msg.setPruned(newValue, prunedReason);
                }
                count++;
            }
        }
        return "Updated " + count + " message(s).";
    }

    @AiTool(value = "Sets the pruned/pinned state of one or more message parts. " +
            "Use newValue=true to prune (soft-prune), newValue=false to pin, and newValue=null to reset to auto-pruning.")
    public String setPartPruned(
            @AiToolParam("The sequential IDs of the parts to update.") List<Long> partIds,
            @AiToolParam("The new pruned state (true=pruned, false=pinned, null=auto).") Boolean newValue,
            @AiToolParam(value = "The reason for pruning.", required = false) String prunedReason) {
        List<AbstractMessage> history = getChat().getContextManager().getHistory();
        int count = 0;
        for (AbstractMessage msg : history) {
            for (AbstractPart part : msg.getParts(true)) {
                if (partIds.contains(part.getSequentialId())) {
                    // Redirection: If targeting a Tool Response, apply to the initiating Tool Call instead.
                    if (part instanceof AbstractToolResponse atr && atr.getCall() != null) {
                        atr.getCall().setPruned(newValue, prunedReason);
                    } else {
                        part.setPruned(newValue, prunedReason);
                    }
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
    public String enableServerTools() {
        getChat().getConfig().setServerToolsEnabled(true);
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
        sb.append("- **Thoughts Visible**: ").append(config.isExpandThoughts()).append(config.isExpandThoughts() ? " (user's ui displaying thought parts)" : "(**user cannot read your thought parts**)\n");
        sb.append("- **Total Messages**: ").append(domainChat.getContextManager().getHistory().size()).append("\n");
        sb.append("- **Context Usage**: ").append(String.format("%.1f%%", domainChat.getContextWindowUsage() * 100))
          .append(" (").append(domainChat.getLastTotalTokenCount()).append(" / ").append(config.getTokenThreshold()).append(" tokens)\n");
        
        sb.append("\n### Default Max Depth Policies:\n");
        sb.append("- **Text Parts**: ").append(config.getDefaultTextPartMaxDepth()).append("\n");
        sb.append("- **Tool Responses**: ").append(config.getDefaultToolMaxDepth()).append("\n");
        sb.append("- **Blob Parts**: ").append(config.getDefaultBlobPartMaxDepth()).append("\n");
        sb.append("*(Note: Individual tools or toolkits may override these defaults)*\n");
        
        sb.append("\n### Capabilities:\n");
        sb.append("- **Local Java Tools**: ").append(config.isLocalToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");
        sb.append("- **Hosted Server Tools**: ").append(config.isServerToolsEnabled() ? "ENABLED" : "DISABLED").append("\n");

        if (!config.isServerToolsEnabled() && domainChat.getSelectedModel() != null) {
            List<ServerTool> serverTools = domainChat.getSelectedModel().getAvailableServerTools();
            if (!serverTools.isEmpty()) {
                sb.append("\n#### Available Server Tools (Currently Disabled):\n");
                sb.append("The following tools are available but cannot be used while Local Tools are enabled. " +
                          "Use `enableServerTools()` to switch modes.\n");
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
