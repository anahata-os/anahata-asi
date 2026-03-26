/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.toolkit;

import java.util.Collections;
import java.util.List;
import java.util.logging.LogRecord;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.AgiConfig;
import uno.anahata.asi.agi.context.ContextManager;
import uno.anahata.asi.agi.context.ContextProvider;
import uno.anahata.asi.agi.context.GarbageCollectorRecord;
import uno.anahata.asi.agi.message.AbstractMessage;
import uno.anahata.asi.agi.message.AbstractPart;
import uno.anahata.asi.agi.message.PruningState;
import uno.anahata.asi.agi.message.RagMessage;
import uno.anahata.asi.agi.provider.ServerTool;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolException;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.spi.AbstractToolCall;

/**
 * A specialized toolkit for managing the conversation history, including
 * metadata injection and surgical pruning of messages and parts.
 * <p>
 * This toolkit is primarily used during 'History Compression' turns where the
 * model is forced to evaluate its own context window and decide which
 * information remains relevant.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AiToolkit("A toolkit for managing the 'pruningState' flag of any items in the conversation history.")
public class History extends AnahataToolkit {

    @Override
    public List<String> getSystemInstructions() throws Exception {
        String s = "\n\n**Context Window Garbage Collector(CwGC)**:\n"
                + "The Anahata ASI platform features a revolutionary invention for managing the history size in order to create indefenitly long running conversations that feel like an incredible smooth flow by: "
                + "\na) Automatically deeming parts in the history as 'effectively pruned' when their 'Remaining Depth' reaches 0."
                + "\nb) Permanently removing messages from the history once all parts in that message are 'effectively prunned'. "
                + "\nNote: The term 'Depth' refers to the distance from the current turn. When a new message is added to the conversation, each part is given an associated 'expiry depth' depending on the type of the part (see RAG message for current values). \n"
                + "\n"
                + "\n\nAll parts in the history have a 'prunningState' attribute that can have three values: AUTO, PRUNED, or PINED: these are the semantics:\n"
                + "- `AUTO`: Parts remain in context but are deemed 'effectively prunned' when the 'Remaining Depth' reaches 0. \n"
                + "- `PRUNED`: The part has been explicitely pruned but will not be 'garbage collected' for as long as:\n"
                + "     a) Its Remaining Depth > 0 \n"
                + "     OR \n"
                + "     b) There are other parts in the message that are not 'effectively pruned'.\n"
                + "- `PINED` : Pinned Parts do not get garbage collected and remain in the prompt until unpined \n"
                + "\n\n"
                + "\nWhen ALL parts in a message are 'effectively pruned' (i.e. PRUNED or AUTO + Remaining Depth <0), the entire message will be garbage collected (permanently removed from the prompt)."
                + "\nWhen a part is 'effectively pruned', the part's metadata will contain a hint about the content of the part but the actual part will not be included in the prompt unless the user sets the 'include pruned parts' checkbox in the request config panel. "
                + "You can change the 'prunningState' attribute of any parts in the prompt regardless of whether they are 'effectively pruned' or not (i.e. for as long as the message it belongs to has not been garbage collected).";

        s += "\n\nThe History toolkit allows you to:\n"
                + "a) Change the pruningState flag of any parts or entire messages in the conversation history to:"
                + "\n -PRUNED,"
                + "\n -PINED"
                + "\n -AUTO"
                + "b) Configure history metadata position: ('In-band injection' vs 'Consolidated Index in RAG message').\n"
                + "c) Clear the Context Window Garbage Collector logs.\n";

        s += "\n\n**History Metadata Format**: \n"
                + "There is a sequence generator for both parts and messages that starts at 1 when a session is created but these ids will most likely not be contiguous as messages and parts can be pruned, deleted or garbage collected. The main thing about the ids of parts and messages is that they are garanteed to be unique within the session and they go in ascending order. These ids are for you to identify the parts or messages of the prompt that whose pruningState flag you want to change (they are just like primary keys for each element in the history)\n"
                + "\n1. **Message Metadata**: These provide interaction-level context (turn-level context)"
                + " e.g. `[x-anahata-message-id: 12 | From: user | Device: papa-linux | Time: 12:34:56 | Tokens: 450 | Depth: 4]`\n"
                + "\n2. **Part Metadata**: These provide granular context for each content block (part)"
                + " e.g., `[x-anahata-part-id: 45 | Type: TextPart | Tokens: 120 | Remaining Depth: 108 | pruningState: AUTO | expanded: true]`\n"
                + "   - . The UI renders parts in collapsible panels so the expanded attribute is just to tell you if the contents of those parts are visible to the user... nothing that important.\n"
                + "\n3. **Tool messages**: No metadata is produced for tool messages nor function response parts however, the metadata of the tool's response (tool execution results, token count of the respose, pruned hints, etc.) is included in the metadata of the tool call part (in the preceeding model message containing the tool calls)\n"
                + "\n3. **Rag Message (with user role)**: This message is dynamically generated on every turn, it is not considered part of the 'history' so no metadata is produced for the RAG message.\n"
                + "\n4. **Pruning Hints**: If a part is effectively pruned, its content is removed from the prompt, but its header remains with a `Hint` (e.g., `| Hint: do a search...`). This allows you to maintain semantic flow without raw data overhead.\n";

        s += "\n\n**History Metadata Positioning**: \n";
        s += "The ASI framework supports two strategys for providing message and part metadata for all entries in the session's history:\n"
                + "a) **In-Band metadata injection**:"
                + "\n\t When enabled, Message metadata is injected as a text part at the start of each message and Part metadata headers will be injected as text before each part."
                + " In this mode, you will be restricted to only using the tools from the History toolkit so you must disable in-band metadata injection once you are done with the pruning or pining operations. "
                + "b) **Consolidated Metadata Index in RAG Message**:"
                + "\n\t All history metadata is consolidated into a single text part in the RAG message. No tool calling restrictions in this mode, you can prune, pin or set to auto any time you want.";
        s += "\n\n**Instructions**: "
                + "\n1)You must keep context window size within bounds and you should also maximize token usage efficency for the user so prune any parts or messages from the history that are redundant or unneccessary as-you-go (you don't need to wait for the context window usage to be 90%+ or the user to ask you, prune anything that you think should be pruned)."
                + "\n2)It is your risponsability to pin any parts or messages that you need to keep in context before they get automatically pruned or garbage collected. While the default max depth policies are a general template, you can't expect the user to pin tool calls manually or any other parts or messages that need to stay in context beyond its default retations turns";

/*        
        s += "\n\nActive History Metadata Positioning Strategy: ";
        if (getAgi().getRequestConfig().isInjectInbandMetadata()) {
            s += "**In-Band Metadata injection**";
        } else {
            s += "**Consolidated Metadata Index in RAG Message.**";
        }
        */

        return Collections.singletonList(s);
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
        sb.append("## History Metadata:\n");
        sb.append("- **Active History Metadata Strategy**: ").append(getAgi().getRequestConfig().isInjectInbandMetadata() ? "In-band injection" : "Consolidated Index in RAG Message").append("\n");
        sb.append("- **Total number of messages in history**: ").append(domainAgi.getContextManager().getHistory().size()).append("\n");

        sb.append("\n Default Max-Depth Policies:\n");
        sb.append("- **Text Parts**: ").append(config.getDefaultTextPartMaxDepth()).append("\n");
        sb.append("- **Model Thought Parts**: ").append(config.getDefaultThoughtPartMaxDepth()).append("\n");
        sb.append("- **Blob Parts**: ").append(config.getDefaultBlobPartMaxDepth()).append("\n");
        sb.append("- **Tool Calls**: ").append(config.getDefaultToolMaxDepth()).append("\n");
        sb.append("* (Note: Individual tools or toolkits may override these defaults, see tool definitions for effective maxDepth values)*\n");

        if (!getAgi().getRequestConfig().isInjectInbandMetadata()) {
            sb.append("\n");
            sb.append(createConsolidatedIndex());
        }

        sb.append("\n Garbage Collector Logs:\n");
        for (GarbageCollectorRecord lr : domainAgi.getContextManager().getGarbageCollector().getLogRecords()) {
            sb.append("\n- ").append(lr);
        }

        ragMessage.addTextPart(sb.toString());
    }

    /**
     * Creates the consolidated history index.
     * 
     * @return
     */
    public String createConsolidatedIndex() {
        StringBuilder sb = new StringBuilder("**Consolidated History Metadata Index**\n");
        ContextManager cm = getAgi().getContextManager();
        List<AbstractMessage> history = cm.getHistory();
        for (AbstractMessage am : history) {
            if (am.shouldCreateMetadata()) {
                sb.append("\n");
                sb.append(am.createMetadataHeader());
                for (AbstractPart ap : am.getParts()) {
                    sb.append("\n\t");
                    sb.append(ap.createMetadataHeader());
                    //here we should do include the hint if it is not effectively pruned
                    if (!ap.isEffectivelyPruned()) {
                        sb.append(" (").append(ap.getPrunedHint()).append(")");
                    }
                }
                sb.append("\n");
            }
        }
        return sb.toString();
    }

    @AiTool(value = "Deletes the garbage collector logs")
    public String clearGarbageCollectorLogs() {
        List<GarbageCollectorRecord> lr = getAgi().getContextManager().getGarbageCollector().getLogRecords();
        getAgi().getContextManager().getGarbageCollector().clearLog();
        return lr.size() + " records cleared";
    }

    /**
     * Updates the pruning policy for all parts of one or more messages.
     * <p>
     * Pruning states:
     * <ul>
     * <li><b>PINNED</b>: Never garbage collected; always remains in
     * context.</li>
     * <li><b>PRUNED</b>: Excluded from the next API payload but kept in
     * history.</li>
     * <li><b>AUTO</b>: Standard lifecycle managed by the CwGC system.</li>
     * </ul>
     * </p>
     *
     * @param messageIds The sequential IDs of the messages to update.
     * @param newState The new pruning state to apply.
     * @return Confirmation message.
     */
    @AiTool(value = "Bulk sets the pruningState of all parts for one or more messages. Requires in-band metadata to be enabled.")
    public String setMessagePruningState(
            @AiToolParam("The x-anahata-message-id of the messages to update.") List<Long> messageIds,
            @AiToolParam("The new pruning state.") PruningState newState) {

        if (!getAgi().getRequestConfig().isInjectInbandMetadata()) {
            throw new AiToolException("In-band metadata injection is not currently enabled. You must enable it first to see the IDs.");
        }

        List<AbstractMessage> history = getAgi().getContextManager().getHistory();
        int count = 0;
        for (AbstractMessage msg : history) {
            if (messageIds.contains(msg.getSequentialId())) {
                switch (newState) {
                    case PINNED ->
                        msg.pinAllParts();
                    case PRUNED ->
                        msg.pruneAllParts();
                    case AUTO ->
                        msg.setAutoAllParts();
                }
                count++;
            }
        }
        return "Updated " + count + " message(s). Remember to disable metadata injection when finished.";
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
    @AiTool(value = "Sets the pruningState state of one or more message parts. Requires in-band metadata to be enabled.")
    public String setPartPruningState(
            @AiToolParam("The x-anahata-part-id of the parts to update.") List<Long> partIds,
            @AiToolParam("The new pruning state.") PruningState newState) {

        if (!getAgi().getRequestConfig().isInjectInbandMetadata()) {
            throw new AiToolException("In-band metadata injection is not currently enabled. You must enable it first to see the IDs.");
        }

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
        return "Updated " + count + " part(s). Remember to disable metadata injection when finished.";
    }

    /**
     * Toggles the injection of in-band metadata headers.
     * <p>
     * When enabled, the model receives unique sequential IDs for every message
     * and part in the history, enabling the use of pruning tools.
     * </p>
     *
     * @param enabled If true, metadata headers are included in the prompt.
     * @return Confirmation message.
     */
    @AiTool(value = "Toggles the injection of in-band metadata headers for history pruning.",
            requiresApproval = false)
    public String setInjectInbandMetadata(boolean enabled) {
        getAgi().getRequestConfig().setInjectInbandMetadata(enabled);
        if (enabled) {
            return "In-band metadata injection enabled. You can now see IDs and use pruning tools.";
        } else {
            return "In-band metadata injection disabled. Pruning tools are now inactive.";
        }
    }
}
