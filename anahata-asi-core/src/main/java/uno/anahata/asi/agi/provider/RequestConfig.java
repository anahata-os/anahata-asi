/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.agi.provider;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.agi.tool.spi.AbstractTool;
import uno.anahata.asi.agi.event.BasicPropertyChangeSource;

/**
 * The definitive, model-agnostic configuration object for a single API request.
 * It holds both the static behavioral parameters and provides live, just-in-time
 * access to dynamic request data like tools and system instructions via its
 * reference to the parent Agi.
 *
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Setter
public class RequestConfig extends BasicPropertyChangeSource {

    /** The parent Agi session this configuration belongs to. */
    @JsonIgnore
    private Agi agi;

    /**
     * Constructs a new request configuration for the specified Agi.
     * @param agi The parent session.
     */
    public RequestConfig(Agi agi) {
        this.agi = agi;
    }

    //== Behavioral Parameters ==//
    /** The temperature for the request. */
    private Float temperature;
    /** The maximum number of output tokens. */
    private Integer maxOutputTokens;
    /** The top K parameter. */
    private Integer topK;
    /** The top P parameter. */
    private Float topP;
    
    /** The number of response variations to generate. */
    private Integer candidateCount = 1;

    /** The level of thinking tokens that the model should generate. */
    private ThinkingLevel thinkingLevel = ThinkingLevel.THINKING_LEVEL_UNSPECIFIED;

    /** The list of response modalities requested for this specific request. */
    private List<String> responseModalities = new ArrayList<>(List.of("TEXT"));

    /** The list of server-side tools enabled for this specific request. */
    private List<ServerTool> enabledServerTools = new ArrayList<>();

    /** If true, the adapter should include pruned messages and parts in the API request. For debugging. */
    private boolean includePruned = false;
    
    /** If true, in-band metadata headers are injected into the prompt for history pruning purposes. */
    private boolean injectInbandMetadata = false;

    public void setInjectInbandMetadata(boolean injectInbandMetadata) {
        boolean old = this.injectInbandMetadata;
        if (old != injectInbandMetadata) {
            this.injectInbandMetadata = injectInbandMetadata;
            propertyChangeSupport.firePropertyChange("injectInbandMetadata", old, injectInbandMetadata);
        }
    }
    
    /** 
     * Whether to use the native provider Schema objects for tool parameters
     * and responses, or to use our custom, purified JSON schemas.
     * Defaults to false (using custom schemas).
     */
    private boolean useNativeSchemas = false;

    //== Provider-Accurate Metadata (Set by Adapters) ==//
    /** The total token count of the system instructions as reported by the provider. */
    private int systemInstructionsTokenCount;
    /** The raw JSON representation of the system instructions. */
    private String systemInstructionsRawJson;

    //== Live Data Getters ==//
    /**
     * Gets the system instructions for this request, assembled just-in-time
     * by the ContextManager.
     * @return A list of TextParts representing the system instructions.
     */
    public List<String> getSystemInstructions() {
        return agi.getContextManager().getSystemInstructions();
    }

    /**
     * Gets the tools for this request, determined just-in-time based on the
     * agi's configuration (local vs. server-side).
     * @return A list of AbstractTools, or null if server-side tools are active.
     */
    public List<? extends AbstractTool> getLocalTools() {
        if (agi.getConfig().isLocalToolsEnabled()) {
            return agi.getToolManager().getEnabledTools();
        }
        return null;
    }

    /**
     * Checks if server-side tools (like Google Search) are enabled for this request.
     * @return true if server tools are enabled.
     */
    public boolean isServerToolsEnabled() {
        return agi.getConfig().isHostedToolsEnabled();
    }
}
