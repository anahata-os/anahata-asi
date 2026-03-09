/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import uno.anahata.asi.resource.v2.view.ResourceView;
import uno.anahata.asi.resource.v2.view.MediaView;
import uno.anahata.asi.resource.v2.view.TextView;
import uno.anahata.asi.resource.v2.handle.ResourceHandle;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.internal.TimeUtils;

/**
 * The Universal Resource Orchestrator.
 * <p>
 * Coordinates between a {@link ResourceHandle} (Source) and a
 * {@link ResourceView} (Interpreter). Implements {@link ContextProvider} for
 * seamless integration into the RAG pipeline.
 * </p>
 * <p>
 * <b>Reactivity:</b> This class extends {@link BasicPropertyChangeSource} to
 * provide live state synchronization to UI components and host-aware managers.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class Resource extends BasicPropertyChangeSource implements Rebindable, ContextProvider {

    /**
     * The immutable unique identifier for this resource instance.
     */
    private final String id = UUID.randomUUID().toString();

    /**
     * The source handle providing access to raw data and metadata.
     */
    private final ResourceHandle handle;

    /**
     * The interpreter view responsible for processing and presenting content.
     */
    private ResourceView view;

    /**
     * Policy for when to reload the content from the handle. Defaults to LIVE.
     */
    private RefreshPolicy refreshPolicy = RefreshPolicy.LIVE;

    /**
     * The designated position for this resource in the model's prompt. Defaults
     * to PROMPT_AUGMENTATION.
     */
    private ContextPosition contextPosition = ContextPosition.PROMPT_AUGMENTATION;

    /**
     * Whether this resource is currently providing context to the model.
     */
    private boolean providing = true;

    /**
     * A descriptive summary of how and when the resource was registered.
     */
    private String description;

    /**
     * The epoch timestamp in milliseconds when this resource was registered.
     * Set authoritatively by the {@link ResourceManager2}.
     */
    private long registrationTime;

    /**
     * The timestamp of the last successful content reload from the handle.
     */
    private long lastLoadTimestamp = -1;

    /**
     * Internal flag indicating the resource or its view configuration is dirty
     * and needs re-interpretation.
     */
    private boolean dirty = true;

    /**
     * Constructs a new Resource orchestrator.
     *
     * @param handle The source handle providing physical/virtual connectivity.
     */
    public Resource(ResourceHandle handle) {
        this.handle = handle;
        this.handle.setOwner(this);
    }

    /**
     * Returns the full content of the resource as a String.
     * <p>
     * <b>Handy API:</b> Delegates to the handle's {@code asText()} which
     * manages streams and charsets.
     * </p>
     *
     * @return The full text content.
     * @throws IOException if reading fails.
     */
    public String asText() throws IOException {
        return handle.asText();
    }

    /**
     * Returns the full content of the resource as a byte array.
     *
     * @return The full binary content.
     * @throws IOException if reading fails.
     */
    public byte[] asBytes() throws IOException {
        return handle.asBytes();
    }

    /**
     * Authoritatively writes text content back to the source handle and marks
     * the resource as dirty to ensure a subsequent reload.
     * <p>
     * <b>Technical Purity:</b> This is the singular entry point for mutations,
     * managing both connectivity and state management in one weld.
     * </p>
     *
     * @param content The text to write.
     * @throws IOException if the write fails.
     */
    public void write(String content) throws IOException {
        handle.write(content);
        markDirty();
    }

    /**
     * Checks if the resource is writable in the current environment.
     *
     * @return true if the underlying handle supports writing.
     */
    public boolean isWritable() {
        return handle.isWritable();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Delegates name resolution to the underlying
     * handle.</p>
     */
    @Override
    public String getName() {
        return handle.getName();
    }

    /**
     * Returns an HTML-formatted display name.
     * <p>
     * <b>Purity Note:</b> This allows the Swing layer to display IDE status
     * (Git status, errors) while maintaining context-level identity.
     * </p>
     *
     * @return The HTML display name from the handle, or null.
     */
    public String getHtmlDisplayName() {
        return handle.getHtmlDisplayName();
    }

    /**
     * Marks the resource as dirty, triggering a re-interpretation during the
     * next Turn or manual UI refresh.
     */
    public void markDirty() {
        if (!dirty) {
            this.dirty = true;
            propertyChangeSupport.firePropertyChange("dirty", false, true);
        }
    }

    /**
     * Sets the refresh policy and fires a property change event if different.
     *
     * @param policy The new policy (LIVE or SNAPSHOT).
     */
    public void setRefreshPolicy(RefreshPolicy policy) {
        RefreshPolicy old = this.refreshPolicy;
        if (old != policy) {
            this.refreshPolicy = policy;
            propertyChangeSupport.firePropertyChange("refreshPolicy", old, policy);
        }
    }

    /**
     * Sets the context position and fires a property change event if different.
     *
     * @param position The new position (SYSTEM_INSTRUCTIONS or
     * PROMPT_AUGMENTATION).
     */
    public void setContextPosition(ContextPosition position) {
        ContextPosition old = this.contextPosition;
        if (old != position) {
            this.contextPosition = position;
            propertyChangeSupport.firePropertyChange("contextPosition", old, position);
        }
    }

    /**
     * Sets the providing status and fires a property change event if different.
     *
     * @param providing True to enable context contribution.
     */
    @Override
    public void setProviding(boolean providing) {
        boolean old = this.providing;
        if (old != providing) {
            this.providing = providing;
            propertyChangeSupport.firePropertyChange("providing", old, providing);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Resources are effectively providing if they are
     * explicitly enabled.</p>
     */
    @Override
    public boolean isEffectivelyProviding() {
        return providing;
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs a 'Pure Sense' of the resource view.
     * Freshness must be guaranteed by turn-level orchestration calling
     * {@link #reloadIfNeeded()} prior to RAG generation.
     * </p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        if (contextPosition == ContextPosition.PROMPT_AUGMENTATION) {
            if (view != null) {
                view.populateRag(ragMessage);
            }
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Performs a 'Pure Sense' of instructions.
     * Freshness is guaranteed by turn-level orchestration.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS) {
            if (view != null) {
                List<String> instructions = view.getInstructions();
                if (instructions.isEmpty()) {
                    return Collections.singletonList("**WARNING**: Managed resource '" + getName() + "' (" + getMimeType() + ") cannot be used as SYSTEM_INSTRUCTIONS because it is a binary resource. Please move it to PROMPT_AUGMENTATION.");
                }
                return instructions;
            }
        }
        return Collections.emptyList();
    }

    /**
     * Synchronously reloads the resource content if it is dirty or stale.
     * <p>
     * <b>Loop Protection:</b> This method clears the dirty flag and updates the
     * load timestamp <i>before</i> firing events, authoritatively breaking
     * recursive synchronization cycles.
     * </p>
     *
     * @throws Exception if the reload or MIME detection fails.
     */
    public synchronized void reloadIfNeeded() throws Exception {
        if (!handle.exists()) {
            return;
        }

        if (view == null) {
            autoBindView();
        }

        boolean stale = (refreshPolicy == RefreshPolicy.LIVE && handle.isStale(lastLoadTimestamp));

        if (dirty || stale) {
            log.info("Reloading resource: {} ({}) [Dirty: {}, Stale: {}]",
                    getName(), id, dirty, stale);

            // ATOMIC STATE TRANSITION: Break recursive loop by clearing flag before events
            this.dirty = false;
            this.lastLoadTimestamp = handle.getLastModified();

            if (view != null) {
                view.reload();
            }

            // Notify UI that a semantic interpretation reload occurred
            propertyChangeSupport.firePropertyChange("reloaded", false, true);
        }
    }

    /**
     * Detects the resource capability via MIME type and binds the correct
     * interpreter.
     */
    private void autoBindView() {
        log.info("Auto-binding view for resource '{}' (MIME: {})", getName(), handle.getMimeType());

        // Decoupled decision: let the handle decide if it can be viewed as text
        if (handle.isTextual()) {
            TextView tv = new TextView();
            tv.setOwner(this);
            this.view = tv;
        } else {
            MediaView mv = new MediaView();
            mv.setOwner(this);
            this.view = mv;
        }
    }

    /**
     * Performs a clean shutdown of the resource, disposing of its handle.
     */
    public void dispose() {
        handle.dispose();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getInstructionsTokenCount() {
        try {
            if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS && view != null) {
                return view.getTokenCount();
            }
            return 0;
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getRagTokenCount() {
        try {
            if (contextPosition == ContextPosition.PROMPT_AUGMENTATION && view != null) {
                return view.getTokenCount();
            }
            return 0;
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * Returns the detected MIME type of the underlying source.
     *
     * @return The MIME type string.
     */
    public String getMimeType() {
        return handle.getMimeType();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Implementation details: Delegates to handle and view polymorphic
     * headers.</p>
     */
    @Override
    public String getHeader() {
        StringBuilder sb = new StringBuilder("**Resource: ");
        sb.append(getName()).append("**\n");
        sb.append("Id: ").append(id).append("\n");
        sb.append("Registration Time: ").append(TimeUtils.formatSmartTimestamp(java.time.Instant.ofEpochMilli(registrationTime))).append("\n");
        sb.append("Refresh Policy: ").append(getRefreshPolicy()).append("\n");
        sb.append("Context Position: ").append(getContextPosition()).append("\n");

        sb.append(handle.getHeader()).append("\n");

        if (view != null) {
            sb.append(view.getHeader()).append("\n");
        }

        if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS && view instanceof MediaView) {
            sb.append("Status: **WARNING** (Position set to SYSTEM_INSTRUCTIONS but resource is binary/media)\n");
        }

        return sb.toString();
    }
}
