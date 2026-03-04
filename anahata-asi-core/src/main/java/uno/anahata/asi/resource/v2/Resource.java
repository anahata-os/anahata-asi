/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import java.util.List;
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
 */
@Slf4j
@Getter
@Setter
public class Resource extends BasicContextProvider implements Rebindable {

    /** The unique identifier for this resource instance. */
    private final String id = UUID.randomUUID().toString();
    
    /** The source handle for the resource. */
    private final ResourceHandle handle;
    
    /** The interpreter view for the resource. */
    private ResourceView view;

    /** Policy for when to reload the content. Defaults to LIVE. */
    private RefreshPolicy refreshPolicy = RefreshPolicy.LIVE;
    
    /** Where to inject the resource in the model context. Defaults to PROMPT_AUGMENTATION. */
    private ContextPosition contextPosition = ContextPosition.PROMPT_AUGMENTATION;
    
    /** The timestamp of the last successful content reload. */
    private long lastLoadTimestamp = -1;
    
    /** Flag indicating the source content has changed (pushed by reactive handles). */
    private boolean sourceDirty = true;

    /** Flag indicating the view configuration has changed and needs reprocessing. */
    private boolean viewDirty = true;

    /**
     * Constructs a new Resource.
     * @param handle The source handle.
     */
    public Resource(ResourceHandle handle) {
        super(handle.getUri().toString(), handle.getName(), "Managed resource: " + handle.getUri());
        this.handle = handle;
        this.handle.setOwner(this);
    }

    /** {@inheritDoc} 
     * Delegates name resolution to the handle.
     */
    @Override
    public String getName() {
        return handle.getName();
    }

    /** 
     * Returns an HTML-formatted display name.
     * <p>Note: This is not part of the ContextProvider interface to keep the core pure.</p>
     * @return The HTML display name from the handle, or null.
     */
    public String getHtmlDisplayName() {
        return handle.getHtmlDisplayName();
    }

    /** 
     * Marks the source content as needing a reload. 
     * Called by reactive handles when filesystem events are detected.
     */
    public void markSourceDirty() {
        this.sourceDirty = true;
    }

    /** 
     * Marks the view as needing a reload due to configuration changes.
     */
    public void markViewDirty() {
        this.viewDirty = true;
    }

    /** 
     * {@inheritDoc} 
     * <p>Orchestrates the reload and delegates population to the active view.</p>
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
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        if (contextPosition == ContextPosition.SYSTEM_INSTRUCTIONS) {
            reloadIfNeeded();
            if (view != null) {
                return view.getInstructions(handle);
            }
        }
        return super.getSystemInstructions();
    }

    /**
     * Synchronously reloads the resource content if the source or view is dirty/stale.
     * <p>
     * <b>Auto-Binding:</b> If no view is assigned, this method performs MIME detection 
     * via the handle and binds the appropriate view (Text vs Media) automatically.
     * </p>
     * @throws Exception if the reload fails.
     */
    private synchronized void reloadIfNeeded() throws Exception {
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
        }
    }

    /**
     * Detects the resource capability and binds the correct interpreter view.
     */
    private void autoBindView() {
        String mime = handle.getMimeType();
        log.info("Auto-binding view for resource '{}' (MIME: {})", getName(), mime);
        
        if (mime.startsWith("text/") || mime.equals("application/octet-stream")) {
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
     * <p>Restores the owner back-reference after deserialization.</p>
     */
    @Override
    public void rebind() {
        handle.setOwner(this);
        if (view instanceof AbstractResourceView arv) {
            arv.setOwner(this);
        }
    }

    /** 
     * Disposes of the handle and its listeners. 
     * Should be called when the resource is removed from management.
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
     * Returns the detected MIME type of the underlying handle.
     * @return The MIME type.
     */
    public String getMimeType() {
        return handle.getMimeType();
    }

    /** 
     * {@inheritDoc} 
     * <p>Appends URI, MIME type, and view-specific metadata to the base header.</p>
     */
    @Override
    public String getHeader() {
        StringBuilder sb = new StringBuilder(super.getHeader());
        sb.append("Uri: ").append(handle.getUri()).append("\n");
        sb.append("MimeType: ").append(getMimeType()).append("\n");
        sb.append("Refresh Policy: ").append(getRefreshPolicy()).append("\n");
        sb.append("Context Position: ").append(getContextPosition()).append("\n");
        sb.append("Last Modified: ").append(handle.getLastModified()).append("\n");
        if (view != null) {
            sb.append("View Details: ").append(view.toString()).append("\n");
        }
        return sb.toString();
    }
}
