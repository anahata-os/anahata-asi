/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.toolkit;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.context.ContextManager;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;
import uno.anahata.asi.agi.tool.ToolExecutionStatus;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;

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
@AgiToolkit("Toolkit for managing the current agi session's metadata and context policies.")
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
        log.info("Session toolkit postActivate. sessionRestored timestamp: " + sessionRestored);
    }

    @Override
    public List<String> getSystemInstructions() throws Exception {
        String s = 
                "The Session toolkit is low-level toolkit to control the most core, low level aspects of this Agi (also known as 'Agi Container' or simply 'session'). "
                + "This and the History toolkit are absolutely essential for long running agentic workflows."
                + "\nThe Agi (or session) is in charged of the turn execution and flow control and its primary components are: "
                + "\n-The ToolManager - has collection of toolkits (set of tools), "
                + "\n-The ResourceManager - tracks resources in context. Accessible via Resources toolkit, "
                + "\n-Context Manager: tracks messages (history) and registered ContextProvider(s). (ToolManager and ResourceManager are both registered context providers)."
                + "\n-Context Window Garbage Collector (TM): Anahata's very own, unique, sinking stack / depth based system to gradually fade different message parts based on their type and preconfigured (max depth / turns to keep)."
                + "\n\nMost toolkits implement ContextProvider so in many cases you will see a toolkit and a context provider with the toolkit name in its id. "
                + "All toolkits and context providers are fully stateful java objects and the entire Agi (session) gets 'auto-backedup' (serialized) to a kryo file on the sessions dir of the AsiContainer on every turn."
                + "Both the user and the model can spawn new Agis sessions on this AsiContainer where you are running in, which in this case is: " + getAsiContainer() + "."
                + "\n\nThis toolkit (and its associated context provider) are critical for agentic workflow as it provides session-control data and session-control tools to:"
                + "\n-  See the current metadata for this session "
                + "\n-  manage the session task dashboard / summary (totally critical for keeping track of completed tasks, tasks in progress, tasks on hold) "
                + "\n-  enable / disable toolkits (can significally reduce the token bloat)"
                + "\n-  toggle the 'providing' status of context providers (resources are also context providers and their prompt visibility can be easily toggled with setContextProviderProviding(resourceUuid) to include or not include the content of the resources in the prompt. By setting a resource to 'not providing', the resource header with all its metadata e.g.: size, description, lastModified, uri, etc will always be in the rag message ragardless of whether it is providing or not."
                + "\n-  kill long running tool calls that are still in EXECUTING state from previous turns (This is pure Anahata ASI Magic, the user can click send whil a tool you proposed is still EXECUTING and you can kill the execution or check the logs/results/errors in subsequent turns).";
        return Collections.singletonList(s);

    }

    /**
     * Updates the AGIs nickname.
     *
     * @param nickName the new nickname
     * @return a confirmation message
     */
    @AgiTool(value = "Updates the current AGI nickname. This shows on the ASI container's dashboard. "
            + "Only call this tool if a) the session doesn't have one (because it has just been created) or the topic has changed to the point that it is worth changing and the user hasn't updated the nickname manually on the UI to reflect this change. Keep it as short as possible. Two words or 20 characters max.",
            permission = ToolPermission.PROMPT, maxDepth = 2)
    public String updateSessionNickname(@AgiToolParam("A concise summary of the conversation's current state.") String nickName) {
        uno.anahata.asi.agi.Agi domainAgi = getAgi();
        if (nickName != null && !nickName.isBlank()) {
            domainAgi.setNickname(nickName);
        }
        log.info("Session nickname updated: {}", nickName);
        return "Session nickname updated successfully to " + nickName;
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
     * The summary is visible to the user in the container dashboard and the
     * parent agi if spawned by another agi.
     * </p>
     *
     * @param summary A concise summary of the conversation's current state or
     * topic.
     * @return A confirmation message.
     */
    @AgiTool(value = "Updates the current AGI session summary / task dasbhoard. This shows on the ASI container's dashboard."
            + " You must use it as the session's task dashboard to reflect and keep track of tasks in progress / tasks completed / tasks on hold, etc. "
            + "It is very important hat you use this tool every time a task changes in status or description as other wise, you can suffer from Garbage Collector amnesia"
            + " (e.g. you update a file, the findAndReplaceInTextResource tool call you used to complete the task gets garbage collected 4 turns later and on the following turn you think it is still pending),"
            + " always look closely at both: the default 'maxDepth' for parts based on type (thouhts, text, tools, etc) but also the maxDepth of every individual tool as not all tools have the same maxDepth (e.g. some token heavy tools like updateTextResource may have less than the default tool max depth setting)."
            + "Usage: If you have been created by another AGI, then you must update it in every single turn as your 'parent agi' (the session that created this ssession) will see it on every turn."
            + "If this session was manually created by the user, you should updated it every time a task that has been discussed througout the course of the session changes in scope, status or  anything else."
            + "Do not remove completed tasks from this summary / dashboard until the user 'signs off'. If you think you have completed a task, mark it as completed in the session summary but don't remove it until you have direct confirmation from the user (or the user tells you explicitely that there is no need to track tasks). "
            + "If you are tracking project tasks let's say in a tasks.md file, you still have to keep updating the session summary with the tasks pertaining to this session (e.g. a tasks.md file, tasks in anahata.md or a tasks repo like github are project-level or organization-level tasks, but this summary is for a totally different purpose, it is for you to track 'session level tasks' and improve agentic workflow in the sliding context window the anahata asi framework provides) ",
            permission = ToolPermission.APPROVE_ALWAYS,
            maxDepth = 1)
    public String updateSessionSummary(@AgiToolParam("A concise summary of the conversation's current state.") String summary) {
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
    @AgiTool(value = "Sets the 'providing' flag of various context providers by their context provider id. "
            + "Use this to enable/disable any context providers. "
            + " For toolkits, it refers to the instructions and the content they contribute to the rag message. Does not affect the availability of tools from a tool calling perspective. You could disable a toolkits context provider and still be able to call its tools. "
            + " For resources, it can be used if you dont want to unload them fully. By changing a resource to providing=false, the resource will still be registered with the ResourceManager and you will still get the metadata of the resource (the header with the location, size, last modified, etc) but not the part with the content that follows the header (the View). Can be useful to juggle resources when context window usage demands it.")
    public void setContextProviderProviding(
            @AgiToolParam("Whether to enable or disable the providers.") boolean providing,
            @AgiToolParam("The IDs of the context providers to update.") List<String> providerIds) {
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
    @AgiTool("Enables or disables multiple toolkits by their names (IDs)."
            + " Disabling a toolkit, not only removes all its tools from the tools "
            + "declration section of the next payload but also its associated context "
            + "provider (so that toolkit's context provider or any of it's children "
            + "context providers will not be contributing to the rag message)")
    public void setToolkitEnabled(
            @AgiToolParam("Whether to enable or disable.") boolean enabled,
            @AgiToolParam("The names of the toolkits to update (e.g., 'Audio', 'Browser').") List<String> toolkitNames) {
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
    @AgiTool(value = "Kills one or more currently executing tools by their [x-anahata-part-id].", permission = ToolPermission.APPROVE_ALWAYS)
    public String killRunningTools(@AgiToolParam("The [x-anahata-part-id] of the tool calls to kill.") List<Long> toolCallIds) {
        List<AbstractToolCall<?, ?>> executing = getAgi().getToolManager().getExecutingCalls();
        int stoppedCount = 0;
        StringBuilder logBuilder = new StringBuilder();

        for (Long id : toolCallIds) {
            AbstractToolCall<?, ?> call = executing.stream()
                    .filter(tc -> tc.getSequentialId() == id)
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
    @AgiTool(value = "Disables local Java tools (if enabled) and enables hosted server tools (e.g., Google Search, Maps, etc.). "
            + "CRITICAL: Local tools and hosted tools are mutually exclusive. After calling this tool, you will loose access to local java tools until the user manually "
            + "clicks the Cigar icon in the toolbar. Use this only if you specifically need a server-side capability like hosted google search and on your next response "
            + "ask the user to re-enable local tools ('please click on the cigar icon on the tool bar so i can continue using local tools')"
            + " (when local tools are enabled the ui shows a java coffee cup logo and when they are disabled, it shows a smoking cigar icon so if the user had local tools"
            + " enabled when you changed to hosted tools, he would have seen the icon changing from a coffe cup to a cigar) "
            + " .")
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
        sb.append("- **AI Provider Class**: ").append(domainAgi.getSelectedModel().getProvider().getClass()).append("\n");
        sb.append("- **AI Provider uuid**: ").append(domainAgi.getSelectedModel().getProvider().getUuid()).append("\n");
        sb.append("- **Model Class**: ").append(domainAgi.getSelectedModel().getClass().getName()).append("\n");
        sb.append("- **Model Id (you)**: ").append(domainAgi.getSelectedModel() != null ? domainAgi.getSelectedModel().getModelId() : "None").append("\n");
        sb.append("- **Thinking Level**: ").append(domainAgi.getRequestConfig().getThinkingLevel()).append("\n");
        sb.append("- **Session ID**: ").append(config.getSessionId()).append("\n");
        sb.append("- **Start Time**: ").append(sessionStart).append("\n");
        sb.append("- **Last Restore Time** (last time this session was loaded/deserialized from disk): ").append(sessionRestored != null ? sessionRestored : " This session has not yet been deserialized").append("\n");
        sb.append("- **Nickname**: ").append(domainAgi.getNickname()).append("\n");
        sb.append("- **Display Name**: ").append(domainAgi.getDisplayName()).append("\n");
        sb.append("- **Summary**: ").append(domainAgi.getConversationSummary() != null ? domainAgi.getConversationSummary() : "N/A").append("\n");
        sb.append("- **Auto-reply tools enabled**: ").append(config.isAutoReplyTools()).append(
                config.isAutoReplyTools()
                ? " (If all the tools you propose in a batch have ALWAYS permission, the tool calls will be automatically executed and the responses will be sent to you inmediatly without user intervention )"
                : " (User has to click 'Run pending and send' for you to get the results.)");
        sb.append("- **Expand Thoughts**: ").append(config.isExpandThoughts()).append(config.isExpandThoughts() ? " (user's ui expands the thought parts with your reasoning when a new part arrives)" : "(**reasonig not showing**)\n");
        sb.append("- **Context Window Usage (Last turn)**: ").append(String.format("%.1f%%", domainAgi.getContextWindowUsage() * 100))
                .append(" (").append(domainAgi.getLastTotalTokenCount()).append(" / ").append(config.getTokenThreshold()).append(" tokens)\n");

        sb.append("\n Capabilities:\n");
        sb.append("- **Local Java Tools**: ").append(config.isLocalToolsEnabled() ? "ENABLED" : "DISABLED **Do not call local java tools**").append("\n");
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

        // Child AGI Status Tracking: Provide visibility into spawned sessions
        List<Agi> children = domainAgi.getChildren();

        if (!children.isEmpty()) {
            sb.append("\n### Child AGI Sessions\n");
            Optional<AsiContainer> acOpt = domainAgi.getToolkit(AsiContainer.class);
            if (acOpt.isPresent()) {
                AsiContainer ac = acOpt.get();
                for (Agi child : children) {
                    // Leverage the beautiful detailed dump from the AsiContainer toolkit
                    sb.append(ac.getAgiDetails(child.getConfig().getSessionId())).append("\n");
                }
            } else {
                // Fallback if the toolkit is disabled
                for (Agi child : children) {
                    sb.append("- **").append(child.getDisplayName()).append("** (ID: ").append(child.getConfig().getSessionId()).append(")\n");
                }
            }
        }

        ragMessage.addTextPart(sb.toString());
    }
}
