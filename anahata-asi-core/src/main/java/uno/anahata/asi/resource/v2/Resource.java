/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.context.BasicContextProvider;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.core.Rebindable;
import uno.anahata.asi.model.resource.RefreshPolicy;

/**
 * The Universal Resource Orchestrator.
 * <p>
 * Coordinates between a {@link ResourceHandle} (Source) and a {@link ResourceView} (Interpreter).
 * Implements {@link uno.anahata.asi.context.ContextProvider} for seamless integration into the RAG pipeline.
 * </p>
 * <p>
 * <b>Reactivity:</b> This class fires property change events for its metadata and settings, 
 * allowing UI components like the {@code Resource2Panel} to maintain a live, synchronized 
 * view of the resource's state.
 * </p>
 * 
 * @author anahata
 */
@Slf4j
@Getter
@Setter
public class Resource extends BasicContextProvider implements Rebindable {

    /** The unique identifier for this resource instance. */
    private final String id = UUID.randomUUID().toString();
    
    /** The source handle providing access to raw data and metadata. */
    private final ResourceHandle handle;
    
    /** The interpreter view responsible for processing and presenting content. */
    private ResourceView view;

    /** Policy for when to reload the content from the handle. Defaults to LIVE. */
    private RefreshPolicy refreshPolicy = RefreshPolicy.LIVE;
    
    /** The designated position for this resource in the model's prompt. Defaults to PROMPT_AUGMENTATION. */
    private ContextPosition contextPosition = ContextPosition.PROMPT_AUGMENTATION;
    
    /** The timestamp of the last successful content reload from the handle. */
    private long lastLoadTimestamp = -1;
    
    /** Flag indicating the source content has changed (pushed by reactive handles). */
    private boolean sourceDirty = true;

    /** Flag indicating the view configuration (viewport) has changed and needs reprocessing. */
    private boolean viewDirty = true;

    /**
     * Constructs a new Resource orchestrator.
     * @param handle The source handle providing physical/virtual connectivity.
     */
    public Resource(ResourceHandle handle) {
        super(handle.getUri().toString(), handle.getName(), "Managed resource: " + handle.getUri());
        this.handle = handle;
        this.handle.setOwner(this);
    }

    /** 
     * {@inheritDoc} 
     * <p>Delegates name resolution to the underlying handle.</p>
     */
    @Override
    public String getName() {
        return handle.getName();
    }

    /** 
     * Returns an HTML-formatted display name.
     * <p>
     * <b>Purity Note:</b> This is not part of the core {@code ContextProvider} interface 
     * as it is a UI concern. It allows the Swing layer to display IDE status (Git, errors).
     * </p>
     * @return The HTML display name from the handle, or null.
     */
    public String getHtmlDisplayName() {
        return handle.getHtmlDisplayName();
    }

    /** 
     * Marks the source content as needing a reload. 
     * Typically called by reactive handles when filesystem events are detected.
     */
    public void markSourceDirty() {
        if (!sourceDirty) {
            this.sourceDirty = true;
            propertyChangeSupport.firePropertyChange("sourceDirty", false, true);
        }
    }

    /** 
     * Marks the view as needing a reload due to configuration changes (e.g., viewport tweaks).
     * This signals the orchestrator to re-run the interpreter before the next Turn.
     */
    public void markViewDirty() {
        if (!viewDirty) {
            this.viewDirty = true;
            propertyChangeSupport.firePropertyChange("viewDirty", false, true);
        }
    }

    /**
     * Sets the refresh policy and fires a property change event if different.
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
     * @param position The new position (SYSTEM_INSTRUCTIONS or PROMPT_AUGMENTATION).
     */
    public void setContextPosition(ContextPosition position) {
        ContextPosition old = this.contextPosition;
        if (old != position) {
            this.contextPosition = position;
            propertyChangeSupport.firePropertyChange("contextPosition", old, position);
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Orchestrates the reload and delegates RAG population to the active view.</p>
     */
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        if (contextPosition == ContextPosition.PROMPT_AUGMENTATION) {
            reloadIfNeeded();
            if (view != null) {
                view.populateRag(ragMessage, handle);
            }
        }
    }

    /** 
     * {@inheritDoc} 
     * <p>Orchestrates the reload and delegates instruction generation to the active view.</p>
     * <p>
     * <b>Guideline Protection Shield:</b> If a resource is misconfigured to provide 
     * system instructions but its view is incapable of providing text (e.g., MediaView), 
     * this method returns an explicit warning instruction to the model to prevent 
     * silent information loss.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS) {
            reloadIfNeeded();
            if (view != null) {
                List<String> instructions = view.getInstructions(handle);
                if (instructions.isEmpty()) {
                    return Collections.singletonList("**WARNING**: Managed resource '" + getName() + "' (" + getMimeType() + ") cannot be used as SYSTEM_INSTRUCTIONS because it is a binary resource. Please move it to PROMPT_AUGMENTATION.");
                }
                return instructions;
            }
        }
        return super.getSystemInstructions();
    }

    /**
     * Synchronously reloads the resource content if the source or view is dirty or stale.
     * <p>
     * <b>Auto-Binding:</b> If no view is currently assigned, this method performs 
     * MIME detection via the handle and binds the appropriate interpreter (Text vs Media).
     * </p>
     * @throws Exception if the reload or MIME detection fails.
     */
    public synchronized void reloadIfNeeded() throws Exception {
        if (!handle.exists()) {
            return;
        }

        if (view == null) {
            autoBindView();
        }

        boolean sourceStale = handle.isStale(lastLoadTimestamp);
        if (sourceDirty || viewDirty || (refreshPolicy == RefreshPolicy.LIVE && sourceStale)) {
            log.info("Reloading resource: {} ({}) [SourceDirty: {}, ViewDirty: {}, SourceStale: {}]", 
                    getName(), id, sourceDirty, viewDirty, sourceStale);
            if (view != null) {
                view.reload(handle);
            }
            this.lastLoadTimestamp = handle.getLastModified();
            this.sourceDirty = false;
            this.viewDirty = false;
            // Notify UI that a physical reload occurred
            propertyChangeSupport.firePropertyChange("reloaded", false, true); 
        }
    }

    /**
     * Detects the resource capability via MIME type and binds the correct interpreter.
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
     * {@inheritDoc} 
     * <p>Restores bidirectional owner links after deserialization.</p>
     */
    @Override
    public void rebind() {
        handle.setOwner(this);
        if (view instanceof AbstractResourceView arv) {
            arv.setOwner(this);
        }
    }

    /** 
     * Performs a clean shutdown of the resource, disposing of its handle. 
     */
    public void dispose() {
        handle.dispose();
    }
    
    /** {@inheritDoc} */
    @Override
    public int getInstructionsTokenCount() {
        try {
            if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS && view != null) {
                return view.getTokenCount(handle);
            }
            return 0;
        } catch (Exception e) {
            return 0;
        }
    }

    /** {@inheritDoc} */
    @Override
    public int getRagTokenCount() {
        try {
            if (contextPosition == ContextPosition.PROMPT_AUGMENTATION && view != null) {
                return view.getTokenCount(handle);
            }
            return 0;
        } catch (Exception e) {
            return 0;
        }
    }

    /** 
     * Returns the detected MIME type of the underlying source.
     * @return The MIME type string.
     */
    public String getMimeType() {
        return handle.getMimeType();
    }

    /** 
     * {@inheritDoc} 
     * <p>Appends URI, MIME type, and view-specific details to the base header.</p>
     * <p>Injects a <b>WARNING</b> if a binary resource is assigned to system instructions.</p>
     */
    @Override
    public String getHeader() {
        StringBuilder sb = new StringBuilder(super.getHeader());
        sb.append("Uri: ").append(handle.getUri()).append("\n");
        sb.append("MimeType: ").append(getMimeType()).append("\n");
        sb.append("Refresh Policy: ").append(getRefreshPolicy()).append("\n");
        sb.append("Context Position: ").append(getContextPosition()).append("\n");
        sb.append("Last Modified: ").append(handle.getLastModified()).append("\n");
        
        if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS && view instanceof MediaView) {
            sb.append("Status: **WARNING** (Position set to SYSTEM_INSTRUCTIONS but resource is binary/media)\n");
        }
        
        if (view != null) {
            sb.append("View Details: ").append(view.toString()).append("\n");
        }
        return sb.toString();
    }
}
