/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.resource;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.context.RefreshPolicy;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.resource.ResourceManager;

/**
 * The abstract base class for all managed resources in the V2 framework.
 *
 * This rich, foundational class contains all the common machinery for any
 * stateful entity within the AI's context. It unifies the concepts of a
 * stateful resource and a context provider, giving each resource control
 * over its own lifecycle, refresh policy, and position in the prompt.
 * <p>
 * By implementing {@link ContextProvider}, resources can be plugged directly 
 * into the {@link uno.anahata.asi.context.ContextManager}'s injection pipeline.
 * </p>
 *
 * @author anahata-ai
 * @param <R> The type of the underlying raw Java resource handle (e.g., Path, Process).
 * @param <C> The type of the rendered content (e.g., String, byte[]).
 */
@Getter
@Setter
@Slf4j
@RequiredArgsConstructor
public abstract class AbstractResource<R, C> extends BasicPropertyChangeSource implements ContextProvider, Rebindable {

    /** A unique, immutable identifier for this resource instance. */
    private final String id = UUID.randomUUID().toString();

    /** 
     * The manager that owns this resource. 
     * Every resource must be associated with a manager to access the chat context.
     */
    @NonNull
    @JsonIgnore
    private final ResourceManager resourceManager;

    /** A user-friendly name for the resource (e.g., a filename or a process name). */
    private String name;

    /** The underlying, raw Java resource object. Ignored during JSON serialization. */
    @JsonIgnore
    protected R resource;
    
    /** The policy that determines when this resource's content should be refreshed. */
    private RefreshPolicy refreshPolicy = RefreshPolicy.LIVE;

    /** The position where this resource's content should be injected into the prompt. */
    private ContextPosition contextPosition = ContextPosition.PROMPT_AUGMENTATION;
    
    /** 
     * An optional icon identifier for this resource, used by the UI to look up 
     * a specialized icon.
     */
    private String iconId;

    /** Whether this resource is currently providing context augmentation. */
    private boolean providing = true;

    /**
     * Gets the parent chat session for this resource.
     * @return The chat session.
     */
    @JsonIgnore
    public Chat getChat() {
        return resourceManager.getChat();
    }

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
     * Reloads the resource's content from its source.
     * 
     * @throws Exception if the reload fails.
     */
    public abstract void reload() throws Exception;

    /**
     * Checks if the resource still exists at its source.
     * 
     * @return true if the resource exists.
     */
    public abstract boolean exists();

    /**
     * Checks if the resource's content has changed since the last load.
     * 
     * @return true if the resource is stale.
     * @throws IOException if the check fails.
     */
    public abstract boolean isStale() throws IOException;

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
    @Override
    public String getDescription() {
        String type = getContentType();
        if ("text".equalsIgnoreCase(type)) {
            return name;
        }
        return name + " (" + type + ")";
    }

    /**
     * Calculates the estimated token count for this resource.
     * 
     * @return The estimated token count.
     */
    public abstract int getTokenCount();

    /**
     * Builds the base header string for this resource, including all core metadata.
     * Subclasses should override this to add more specific details, calling super() first.
     * @return The comprehensive base header string.
     */
    @Override
    public String getHeader() {
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

    /** {@inheritDoc} */
    @Override
    public void rebind() {
        super.rebind();
        log.info("Rebinding resource: {} ({})", getName(), getId());
    }

    /**
     * Performs any necessary cleanup when the resource is removed from the manager.
     * Subclasses should override this to release listeners or system resources.
     */
    public void dispose() {
        log.info("Disposing resource: {} ({})", getName(), getId());
    }

    // --- ContextProvider Implementation ---

    /** {@inheritDoc} */
    @Override
    public String getId() {
        return id;
    }

    /** {@inheritDoc} */
    @Override
    public boolean isProviding() {
        return providing && resourceManager.getResources().contains(this);
    }

    /** {@inheritDoc} */
    @Override
    public void setProviding(boolean enabled) {
        this.providing = enabled;
    }

    /** {@inheritDoc} */
    @Override
    public ContextProvider getParentProvider() {
        return resourceManager;
    }

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS) {
            // For system instructions, we include the full text representation (Header + Content)
            C content = getContent();
            String text = getHeader() + "\n" + (content != null ? content.toString() : "");
            return Collections.singletonList(text);
        }
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        if (contextPosition == ContextPosition.PROMPT_AUGMENTATION) {
            // The header is added by the ContextManager, we just add the content
            populateContent(ragMessage);
        }
    }

    /** {@inheritDoc} */
    @Override
    public List<ContextProvider> getChildrenProviders() {
        return Collections.emptyList();
    }

    /** {@inheritDoc} */
    @Override
    public int getInstructionsTokenCount() {
        if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS) {
            return getTokenCount();
        }
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public int getRagTokenCount() {
        if (contextPosition == ContextPosition.PROMPT_AUGMENTATION) {
            return getTokenCount();
        }
        return 0;
    }

    /**
     * Renders the current state or viewport of this resource into a model-consumable Part.
     *
     * @param message The message to populate with the resource's content.
     * @throws java.lang.Exception if the population fails.
     */
    protected abstract void populateContent(RagMessage message) throws Exception;
}
