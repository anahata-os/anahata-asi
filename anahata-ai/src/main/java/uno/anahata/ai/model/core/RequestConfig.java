/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.model.core;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.List;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.ai.Chat;
import uno.anahata.ai.model.tool.AbstractTool;

/**
 * The definitive, model-agnostic configuration object for a single API request.
 * It holds both the static behavioral parameters and provides live, just-in-time
 * access to dynamic request data like tools and system instructions via its
 * reference to the parent Chat.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
@RequiredArgsConstructor
public class RequestConfig {
    @NonNull
    @JsonIgnore
    private final Chat chat;

    //== Behavioral Parameters ==//
    private Float temperature;
    private Integer maxOutputTokens;
    private Integer topK;
    private Float topP;
    
    /** If true, the adapter should include pruned messages and parts in the API request. For debugging. */
    private boolean includePruned = false;

    //== Live Data Getters ==//
    /**
     * Gets the system instructions for this request, assembled just-in-time
     * by the ContextManager.
     * @return A list of TextParts representing the system instructions.
     */
    public List<TextPart> getSystemInstructions() {
        return chat.getContextManager().getSystemInstructions();
    }

    /**
     * Gets the tools for this request, determined just-in-time based on the
     * chat's configuration (local vs. server-side).
     * @return A list of AbstractTools, or null if server-side tools are active.
     */
    public List<? extends AbstractTool> getTools() {
        if (chat.getConfig().isLocalToolsEnabled()) {
            return chat.getToolManager().getEnabledTools();
        }
        // In the future, this could return a representation of server-side tools.
        return null;
    }
}
