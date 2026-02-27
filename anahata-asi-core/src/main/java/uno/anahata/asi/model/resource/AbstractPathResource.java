/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.model.resource;

import com.fasterxml.jackson.annotation.JsonIgnore;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.resource.ResourceManager;

/**
 * An abstract base class for all resources that represent a file on the local filesystem.
 * It extends the universal AbstractResource with file-specific metadata and implements
 * the 'Atomic Reload' architecture for self-healing, live resources.
 * 
 * @author anahata-ai
 * @param <C> The type of the rendered content (e.g., String, byte[]).
 */
@Slf4j
@Getter
@Setter
public abstract class AbstractPathResource<C> extends AbstractResource<Path, C> {
    /** Formatter for displaying timestamps in the resource header. */
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
        .withZone(ZoneId.systemDefault());
    
    /** The absolute path to the file on the local filesystem. */
    private String path;
    
    /** The last modified timestamp of the file on disk at the time of loading. */
    private long loadLastModified;
    
    /** 
     * The cached, rendered content representing the current viewport of the resource. 
     * This cache only contains the content, not the metadata header.
     */
    @JsonIgnore
    protected C cache;

    /**
     * Constructs a new path-based resource.
     * @param manager The parent resource manager.
     * @param path The absolute path to the file.
     */
    public AbstractPathResource(ResourceManager manager, String path) {
        super(manager);
        setPath(path);
    }

    /**
     * Sets the path of the resource and ensures the underlying Path handle 
     * and name are synchronized.
     * 
     * @param path The new absolute path.
     */
    public void setPath(String path) {
        this.path = path;
        if (path != null) {
            this.setName(new File(path).getName());
            this.setResource(Paths.get(path));
        }
    }

    /**
     * {@inheritDoc}
     * Implementation details: Orchestrates the reload logic based on the refresh policy 
     * and then populates the message from the cache.
     */
    @Override
    protected void populateContent(RagMessage ragMessage) throws Exception {
        reloadIfNeeded();
        
        if (cache != null) {
            populateFromCache(ragMessage);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public C getContent() throws Exception {
        reloadIfNeeded();
        return cache;
    }

    /**
     * Checks if the resource needs to be reloaded from disk based on its existence,
     * staleness, and refresh policy.
     * 
     * @throws Exception if an error occurs during the reload process.
     */
    private void reloadIfNeeded() throws Exception {
        if (!exists()) {
            log.warn("Resource file does not exist: {}", path);
            return;
        }
        
        if (cache == null || (isStale() && getRefreshPolicy() == RefreshPolicy.LIVE)) {
            reload();
        }
    }
    
    /**
     * Template method for subclasses to perform the actual population of the 
     * RagMessage using the cached content.
     * 
     * @param rm The message to populate.
     */
    protected abstract void populateFromCache(RagMessage rm);
    
    /**
     * Checks if the underlying file for this resource exists on the filesystem.
     * @return {@code true} if the file exists, {@code false} otherwise.
     */
    @Override
    public boolean exists() {
        return Files.exists(Paths.get(path));
    }
    
    /**
     * Checks if the file on disk has been modified since it was last loaded.
     * @return {@code true} if the file is stale, {@code false} otherwise.
     * @throws IOException if an I/O error occurs.
     */
    @Override
    public boolean isStale() throws IOException {
        return getCurrentLastModified() != getLoadLastModified();
    }
    
    /**
     * Gets the current last modified timestamp from the live file on disk.
     * @return The current last modified timestamp.
     * @throws IOException if an I/O error occurs.
     */
    public long getCurrentLastModified() throws IOException {
        return Files.getLastModifiedTime(Paths.get(path)).toMillis();
    }
    
    /**
     * Atomically reloads the resource's content from disk and updates the cache.
     * @throws Exception if any error occurs during the reload process.
     */
    @Override
    public void reload() throws Exception {
        log.info("Reloading resource: {}", getPath());
        C newContent = reloadContent();
        this.setLoadLastModified(getCurrentLastModified());
        onContentReloaded(newContent);
    }

    /**
     * Subclasses must implement this to read the raw content from the filesystem.
     * 
     * @return The raw content.
     * @throws Exception if the read fails.
     */
    protected abstract C reloadContent() throws Exception;
    
    /**
     * Hook called after content is reloaded. Subclasses should use this to 
     * process the content (e.g. viewport processing) and update the cache.
     * 
     * @param newContent The newly loaded content.
     */
    protected void onContentReloaded(C newContent) {
        C oldCache = this.cache;
        this.cache = newContent;
        if (oldCache != newContent) {
            propertyChangeSupport.firePropertyChange("cache", oldCache, cache);
        }
    }

    /** {@inheritDoc} */
    @Override
    public String getHeader() {
        StringBuilder sb = new StringBuilder(super.getHeader());
        sb.append(String.format("Path: %s\n", getPath()));
        sb.append(String.format("Exists: %s\n", exists()));
        
        if (!exists()) {
            return sb.toString();
        }
        
        try {
            BasicFileAttributes attrs = Files.readAttributes(Paths.get(path), BasicFileAttributes.class);
            String formattedModTime = FORMATTER.format(attrs.lastModifiedTime().toInstant());
            boolean isStale = isStale();
            
            sb.append(String.format("Size: %d bytes\n", attrs.size()));
            sb.append(String.format("Load Last Modified: %d (%s)\n", getLoadLastModified(), FORMATTER.format(Instant.ofEpochMilli(getLoadLastModified()))));
            sb.append(String.format("Disk Last Modified: %d (%s)\n", attrs.lastModifiedTime().toMillis(), formattedModTime));
            sb.append(String.format("Status: %s\n", isStale ? "**STALE**" : "VALID"));

        } catch (IOException e) {
            log.error("Error reading file attributes for header: " + path, e);
            sb.append(String.format("Status: Error reading attributes: %s\n", e.getMessage()));
        }
        return sb.toString();
    }
}
