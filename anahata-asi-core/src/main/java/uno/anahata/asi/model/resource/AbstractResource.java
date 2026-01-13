package uno.anahata.asi.model.resource;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.context.RefreshPolicy;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.RagMessage;

/**
 * The abstract base class for all managed resources in the V2 framework.
 *
 * This rich, foundational class contains all the common machinery for any
 * stateful entity within the AI's context. It unifies the concepts of a
 * stateful resource and a context provider, giving each resource control
 * over its own lifecycle, refresh policy, and position in the prompt.
 *
 * @author anahata-ai
 * @param <R> The type of the underlying raw Java resource (e.g., String, byte[], Process).
 * @param <P> The type of the AbstractPart this resource renders its viewport to.
 */
@Getter
@Setter
public abstract class AbstractResource<R> {

    //<editor-fold defaultstate="collapsed" desc="Identity">
    /** A unique, immutable identifier for this resource instance. */
    private final String id = UUID.randomUUID().toString();

    /** A user-friendly name for the resource (e.g., a filename or a process name). */
    private String name;
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Resource">
    /** The underlying, raw Java resource object. Ignored during serialization. */
    @JsonIgnore
    protected R resource;
    //</editor-fold>
    
    //<editor-fold defaultstate="collapsed" desc="Policy">
    /** The policy that determines when this resource's content should be refreshed. */
    private RefreshPolicy refreshPolicy = RefreshPolicy.SNAPSHOT;

    /** The position where this resource's content should be injected into the prompt. */
    private ContextPosition contextPosition = ContextPosition.PROMPT_AUGMENTATION;
    //</editor-fold>
    
    /**
     * Renders the current state or viewport of this resource into a model-consumable Part.
     * This is the core of the self-rendering resource model. A TextFileResource might
     * return a TextPart with a specific page of content, while a ScreenshotResource
     * would return a BlobPart.
     *
     * @return The AbstractPart representing the resource's current view.
     * @throws java.lang.Exception
     */
    public abstract void populate(RagMessage message) throws Exception;
    
    /**
     * Gets the number of turns remaining before this resource is pruned.
     * The implementation of this method defines the resource's lifecycle policy.
     *
     * @return The number of turns left, or {@code null} if the resource is permanent/stateful.
     */
    public abstract Integer getTurnsRemaining();

    /**
     * Builds the base header string for this resource, including all core metadata.
     * Subclasses should override this to add more specific details, calling super() first.
     * @return The comprehensive base header string.
     */
    protected String buildHeader() {
        Integer turns = getTurnsRemaining();
        String turnsStr = (turns == null) ? "Permanent" : turns.toString();
        
        return String.format(
            "--- Resource: %s ---\n" +
            "Id: %s\n" +
            "Refresh Policy: %s\n" +
            "Context Position: %s\n" +
            "Turns Remaining: %s\n",
            getName(),
            getId(),
            getRefreshPolicy(),
            getContextPosition(),
            turnsStr
        );
    }
}