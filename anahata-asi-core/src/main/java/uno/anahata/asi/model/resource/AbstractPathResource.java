package uno.anahata.asi.model.resource;

import com.fasterxml.jackson.annotation.JsonIgnore;
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
import uno.anahata.asi.model.context.RefreshPolicy;
import uno.anahata.asi.model.core.AbstractPart;
import uno.anahata.asi.model.core.RagMessage;

/**
 * An abstract base class for all resources that represent a file on the local filesystem.
 * It extends the universal AbstractResource with file-specific metadata and implements
 * the 'Atomic Reload' architecture for self-healing, live resources.
 * 
 * @author anahata-ai
 * @param <R> The type of the underlying raw Java resource (e.g., String, byte[]).
 * @param <P> The type of the AbstractPart this resource renders its viewport to.
 */
@Slf4j
@Getter
@Setter
public abstract class AbstractPathResource<R, P> extends AbstractResource<Path> {
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
        .withZone(ZoneId.systemDefault());
    
    //<editor-fold defaultstate="collapsed" desc="File Metadata">
    /** The absolute path to the file on the local filesystem. */
    private String path;
    
    /** The last modified timestamp of the file on disk at the time of loading. */
    private long loadLastModified;
    
    /** The cached, rendered Part representing the current viewport of the resource. Ignored during serialization. */
    @JsonIgnore
    protected P cache;
    //</editor-fold>

    /**
     * The smart accessor for the resource's view. This is the single point of entry
     * for the RAG provider. It acts as an orchestrator, checking for existence and
     * staleness, and triggering a reload only when the refresh policy requires it.
     *
     * @param ragMessage The message to populate with the resource's content.
     * @throws Exception if the reload operation fails.
     */
    @Override
    public void populate(RagMessage ragMessage) throws Exception {
        if (!exists()) {
            log.warn("Resource file does not exist: {}", path);
            this.cache = null; // Invalidate cache
            
        }
        
        if (cache == null || (isStale() && getRefreshPolicy() == RefreshPolicy.LIVE)) {
            reload();
        }
        
        populateFromCache(ragMessage);
    }
    
    protected abstract void populateFromCache(RagMessage rm);
    
    
    /**
     * Checks if the underlying file for this resource exists on the filesystem.
     * @return {@code true} if the file exists, {@code false} otherwise.
     */
    public boolean exists() {
        return Files.exists(Paths.get(path));
    }
    
    /**
     * Checks if the file on disk has been modified since it was first loaded into context.
     * @return {@code true} if the file is stale, {@code false} otherwise.
     * @throws IOException if an I/O error occurs.
     */
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
     * Atomically reloads the resource's content from disk, processes it through the
     * viewport, and updates the cached Part. This method must be implemented by
     * concrete subclasses.
     *
     * @throws Exception if any error occurs during the reload process.
     */
    public abstract void reload() throws Exception;

    @Override
    protected String buildHeader() {
        StringBuilder sb = new StringBuilder(super.buildHeader());
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
            sb.append(String.format("Status: Error reading attributes: %s\n", e.getMessage()));
        }
        return sb.toString();
    }
}
