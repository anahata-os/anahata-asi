package uno.anahata.asi.model.resource;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.context.RefreshPolicy;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;

/**
 * The abstract base class for all managed resources in the V2 framework.
 *
 * This rich, foundational class contains all the common machinery for any
 * stateful entity within the AI's context. It unifies the concepts of a
 * stateful resource and a context provider, giving each resource control
 * over its own lifecycle, refresh policy, and position in the prompt.
 * <p>
 * This class extends {@link BasicPropertyChangeSource} to provide reactive 
 * capabilities while ensuring that UI listeners are not persisted during 
 * serialization.
 * </p>
 *
 * @author anahata-ai
 * @param <R> The type of the underlying raw Java resource handle (e.g., Path, Process).
 * @param <C> The type of the rendered content (e.g., String, byte[]).
 */
@Getter
@Setter
public abstract class AbstractResource<R, C> extends BasicPropertyChangeSource {

    //<editor-fold defaultstate="collapsed" desc="Identity">
    /** A unique, immutable identifier for this resource instance. */
    private final String id = UUID.randomUUID().toString();

    /** A user-friendly name for the resource (e.g., a filename or a process name). */
    private String name;
    //</editor-fold>

    //<editor-fold defaultstate="collapsed" desc="Resource">
    /** The underlying, raw Java resource object. Ignored during JSON serialization. */
    @JsonIgnore
    protected R resource;
    //</editor-fold>
    
    //<editor-fold defaultstate="collapsed" desc="Policy">
    /** The policy that determines when this resource's content should be refreshed. */
    private RefreshPolicy refreshPolicy = RefreshPolicy.LIVE;

    /** The position where this resource's content should be injected into the prompt. */
    private ContextPosition contextPosition = ContextPosition.PROMPT_AUGMENTATION;
    //</editor-fold>
    
    /**
     * Renders the current state or viewport of this resource into a model-consumable Part.
     * This is the core of the self-rendering resource model. A TextFileResource might
     * return a TextPart with a specific page of content, while a ScreenshotResource
     * would return a BlobPart.
     *
     * @param message The message to populate with the resource's content.
     * @throws java.lang.Exception if the population fails.
     */
    public abstract void populate(RagMessage message) throws Exception;
    
    /**
     * Gets the machine-readable content type of this resource (e.g., "text", "image").
     * This is used to determine if a resource is suitable for certain context positions.
     * 
     * @return The content type string.
     */
    public abstract String getContentType();

    /**
     * Gets the latest rendered content of this resource, respecting its refresh policy.
     * For a TextFileResource, this would return a String; for an ImageResource, a byte[].
     * 
     * @return The rendered content object.
     * @throws Exception if rendering or reloading fails.
     */
    public abstract C getContent() throws Exception;
    
    /**
     * Gets the number of turns remaining before this resource is pruned.
     * The implementation of this method defines the resource's lifecycle policy.
     *
     * @return The number of turns left, or {@code null} if the resource is permanent/stateful.
     */
    public abstract Integer getTurnsRemaining();

    /**
     * Gets a user-friendly description of the resource.
     * @return The description string.
     */
    public String getDescription() {
        String type = getContentType();
        if ("text".equalsIgnoreCase(type)) {
            return name;
        }
        return name + " (" + type + ")";
    }

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
