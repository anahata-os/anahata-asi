/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.Agi;
import uno.anahata.asi.status.AgiStatus;
import uno.anahata.asi.internal.kryo.KryoUtils;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;

import uno.anahata.asi.model.resource.AbstractResource;

/**
 * A hybrid static/instance class for managing global and application-specific configurations.
 * <ul>
 *     <li><b>Static methods</b> provide access to the root Anahata AI working directory and its global subdirectories.</li>
 *     <li><b>An instance</b> of this class represents the configuration for a specific host
 *         application (e.g., "netbeans", "standalone"), managing its unique preferences
 *         and its own application-specific subdirectories.</li>
 * </ul>
 * @author anahata-gemini-pro-2.5
 */
@Getter
@Slf4j
public abstract class AsiContainer extends BasicPropertyChangeSource {
    
    /** The unique identifier for the host application (e.g., "netbeans"). */
    private final String hostApplicationId;
    
    /** The persistent preferences for this container instance. */
    private final AsiContainerPreferences preferences;
    
    /** The list of currently active agi sessions managed by this container. */
    private final List<Agi> activeAgis = new ArrayList<>();
    
    /** 
     * A shared executor for container-level background tasks. 
     * @return the container executor service.
     */
    private final ExecutorService executor;

    /**
     * A JVM-scoped map for tools to store and share objects across all containers, 
     * sessions, and turns. This map is thread-safe.
     */
    public static Map applicationAttributes = new ConcurrentHashMap();
    
    /**
     * A container-scoped map for tools to store objects across all sessions and turns 
     * within this specific host application. This map is thread-safe.
     */
    public Map containerAttributes = new ConcurrentHashMap();


    /**
     * Creates a configuration instance for a specific host application.
     * Upon instantiation, it loads the preferences for that application.
     *
     * @param hostApplicationId A unique identifier for the host application (e.g., "netbeans").
     */
    public AsiContainer(String hostApplicationId) {
        this.hostApplicationId = hostApplicationId;
        this.preferences = AsiContainerPreferences.load(this);
        this.executor = AiExecutors.newCachedThreadPoolExecutor(hostApplicationId);
    }

    /**
     * Saves the current preferences for this host application to disk.
     */
    public void savePreferences() {
        preferences.save(this);
    }
    
    /**
     * Gets the root working directory for this specific host application instance.
     * e.g., ~/.anahata/asi/netbeans
     *
     * @return The application-specific working directory path.
     */
    public Path getAppDir() {
        return getWorkDirSubDir(hostApplicationId);
    }
    
    /**
     * Gets a named subdirectory within this host application's working directory, 
     * creating it if it doesn't exist.
     * e.g., ~/.anahata/asi/netbeans/sessions
     * 
     * @param name The name of the subdirectory.
     * @return The Path to the application-specific subdirectory.
     */
    public Path getAppDirSubDir(String name) {
        Path dir = getAppDir().resolve(name);
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            log.error("Could not create application subdirectory: {}", dir, e);
        }
        return dir;
    }

    /**
     * Registers a new agi session with this configuration and triggers the 
     * {@link #onAgiCreated(Agi)} hook. Fires a property change event for "activeAgis".
     * 
     * @param agi The agi session to register.
     */
    public void register(Agi agi) {
        synchronized (activeAgis) {
            for (Agi existing : activeAgis) {
                if (existing.getConfig().getSessionId().equals(agi.getConfig().getSessionId())) {
                    log.warn("Agi session {} already registered. Skipping.", agi.getConfig().getSessionId());
                    return;
                }
            }
            List<Agi> old = new ArrayList<>(activeAgis);
            activeAgis.add(agi);
            onAgiCreated(agi);
            autoSaveSession(agi); // Ensure the session is persisted in the active directory
            propertyChangeSupport.firePropertyChange("activeAgis", old, Collections.unmodifiableList(activeAgis));
            log.info("Registered agi session: {}", agi.getConfig().getSessionId());
        }
    }

    /**
     * Unregisters a agi session from this configuration. 
     * Fires a property change event for "activeAgis".
     * 
     * @param agi The agi session to unregister.
     */
    public void unregister(Agi agi) {
        synchronized (activeAgis) {
            List<Agi> old = new ArrayList<>(activeAgis);
            if (activeAgis.remove(agi)) {
                propertyChangeSupport.firePropertyChange("activeAgis", old, Collections.unmodifiableList(activeAgis));
                log.info("Unregistered agi session: {}", agi.getConfig().getSessionId());
            }
        }
    }

    /**
     * Gets an unmodifiable list of all active agi sessions.
     * 
     * @return The list of active agis.
     */
    public List<Agi> getActiveAgis() {
        synchronized (activeAgis) {
            return Collections.unmodifiableList(new ArrayList<>(activeAgis));
        }
    }
    
    /**
     * Overridable hook for host-specific initialization when a new agi is created.
     * 
     * @param agi The newly created agi session.
     */
    public void onAgiCreated(Agi agi) {
        // Default implementation does nothing.
    }
    
    /**
     * Creates a new agi session with a default configuration.
     * This method should be overridden by host-specific containers.
     * 
     * @return The newly created agi session.
     */
    public abstract Agi createNewAgi();

    /**
     * Opens the specified resource in the host's preferred viewer/editor.
     * 
     * @param resource The resource to open.
     */
    public abstract void openResource(AbstractResource<?, ?> resource);

    // --- SESSION PERSISTENCE ---

    /**
     * Gets the directory where active agi sessions are stored.
     * 
     * @return The sessions directory path.
     */
    public Path getSessionsDir() {
        return getAppDirSubDir("sessions");
    }

    /**
     * Gets the directory where manually saved agi sessions are stored.
     * 
     * @return The saved sessions directory path.
     */
    public Path getSavedSessionsDir() {
        Path dir = getSessionsDir().resolve("saved");
        ensureDirectory(dir);
        return dir;
    }

    /**
     * Gets the directory where disposed agi sessions are moved.
     * 
     * @return The disposed sessions directory path.
     */
    public Path getDisposedSessionsDir() {
        Path dir = getSessionsDir().resolve("disposed");
        ensureDirectory(dir);
        return dir;
    }

    /**
     * Gets the directory where agi sessions that failed to load are moved.
     * 
     * @return The unloadable sessions directory path.
     */
    public Path getUnloadableSessionsDir() {
        Path dir = getSessionsDir().resolve("unloadable");
        ensureDirectory(dir);
        return dir;
    }

    private void ensureDirectory(Path dir) {
        try {
            if (!Files.exists(dir)) {
                Files.createDirectories(dir);
            }
        } catch (IOException e) {
            log.error("Could not create directory: {}", dir, e);
        }
    }

    /**
     * Performs an automatic backup of the session to the active sessions directory.
     * Logic: Only proceeds if the agi is in a stable state (IDLE, TOOL_PROMPT, etc.)
     * to prevent serialization during volatile operations like streaming.
     * 
     * @param agi The agi session to save.
     */
    public void autoSaveSession(Agi agi) {
        AgiStatus status = agi.getStatusManager().getCurrentStatus();
        
        boolean isStable = status == AgiStatus.IDLE 
                        || status == AgiStatus.TOOL_PROMPT 
                        || status == AgiStatus.CANDIDATE_CHOICE_PROMPT
                        || status == AgiStatus.ERROR
                        || status == AgiStatus.MAX_RETRIES_REACHED;

        if (!isStable) {
            log.debug("Skipping auto-save for session {} - agi is currently in volatile state: {}", 
                    agi.getConfig().getSessionId(), status);
            return;
        }
        
        saveSessionTo(agi, getSessionsDir());
    }

    /**
     * Manually saves the session to the 'saved' directory.
     * 
     * @param agi The agi session to save.
     */
    public void manualSaveSession(Agi agi) {
        saveSessionTo(agi, getSavedSessionsDir());
    }

    /**
     * Serializes and saves a agi session to a specific directory using Kryo.
     * This method is synchronized on the agi instance to prevent concurrent write issues.
     * 
     * @param agi The agi session to save.
     * @param dir The destination directory.
     */
    private void saveSessionTo(Agi agi, Path dir) {
        synchronized (agi) {
            String sessionId = agi.getConfig().getSessionId();
            Path file = dir.resolve(sessionId + ".kryo");
            Path tmpFile = dir.resolve(sessionId + ".kryo.tmp");
            try {
                log.info("Saving session {} to {}", sessionId, file);
                byte[] data = KryoUtils.serialize(agi);
                
                // 1. Write to temporary file
                Files.write(tmpFile, data);
                
                // 2. Atomic move to destination
                try {
                    Files.move(tmpFile, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
                } catch (java.nio.file.AtomicMoveNotSupportedException e) {
                    log.warn("Atomic move not supported on this filesystem, falling back to standard move for: {}", file);
                    Files.move(tmpFile, file, StandardCopyOption.REPLACE_EXISTING);
                }
                
            } catch (IOException e) {
                log.error("Failed to save session: {}", sessionId, e);
                // Attempt to clean up the orphaned temp file
                try {
                    Files.deleteIfExists(tmpFile);
                } catch (IOException ex) {
                    log.debug("Could not delete temporary file: {}", tmpFile);
                }
            }
        }
    }

    /**
     * Permanently disposes of a agi session, shutting it down and moving its 
     * serialized file to the 'disposed' directory.
     * 
     * @param agi The agi session to dispose.
     */
    public void dispose(Agi agi) {
        String sessionId = agi.getConfig().getSessionId();
        log.info("Disposing session: {}", sessionId);
        
        // 1. Shutdown the agi (stops executors, etc.)
        agi.shutdown();
        
        // 2. Move the session file from active to disposed
        Path activeFile = getSessionsDir().resolve(sessionId + ".kryo");
        if (Files.exists(activeFile)) {
            try {
                Path disposedFile = getDisposedSessionsDir().resolve(sessionId + ".kryo");
                Files.move(activeFile, disposedFile, StandardCopyOption.REPLACE_EXISTING);
                log.info("Moved session file to disposed directory: {}", disposedFile);
            } catch (IOException e) {
                log.error("Failed to move session file to disposed directory", e);
            }
        }
        
        // 3. Unregister from active list (fires property change)
        unregister(agi);
    }

    /**
     * Imports a agi session from an external file. The session is assigned a 
     * new ID to avoid collisions and registered as a new active agi.
     * 
     * @param path The path to the serialized session file.
     * @return The imported Agi session, or null if import failed.
     */
    public Agi importSession(Path path) {
        try {
            log.info("Importing session from {}", path);
            byte[] data = Files.readAllBytes(path);
            Agi agi = KryoUtils.deserialize(data, Agi.class);
            
            // Always generate a new session ID for imported sessions to avoid collisions
            agi.getConfig().setSessionId(UUID.randomUUID().toString());
            
            agi.rebind(this);
            register(agi);
            return agi;
        } catch (Exception e) {
            log.error("Failed to import session from {}", path, e);
            return null;
        }
    }

    /**
     * Scan the sessions directory and loads all serialized agi sessions.
     * This is typically called during application startup.
     * 
     * @return The number of sessions that failed to load.
     */
    public int loadSessions() {
        Path sessionsDir = getSessionsDir();
        if (!Files.exists(sessionsDir)) return 0;

        AtomicInteger failedCount = new AtomicInteger(0);
        try (Stream<Path> stream = Files.list(sessionsDir)) {
            stream.filter(p -> !Files.isDirectory(p)) // Only load files from the root (active sessions)
                  .filter(p -> p.toString().endsWith(".kryo"))
                  .parallel()
                  .forEach(p -> {
                      if (!loadSession(p)) {
                          failedCount.incrementAndGet();
                      }
                  });
        } catch (IOException e) {
            log.error("Failed to list sessions in {}", sessionsDir, e);
        }
        return failedCount.get();
    }

    /**
     * Loads a single agi session from a file, rebinds it to this container, 
     * and registers it.
     * 
     * @param path The path to the serialized session file.
     * @return true if the session was loaded successfully, false otherwise.
     */
    private boolean loadSession(Path path) {
        try {
            log.info("Loading session from {}", path);
            byte[] data = Files.readAllBytes(path);
            Agi agi = KryoUtils.deserialize(data, Agi.class);
            agi.rebind(this);
            register(agi);
            return true;
        } catch (Throwable t) {
            log.error("Failed to load session from {}. Moving to unloadable directory.", path, t);
            try {
                Path unloadablePath = getUnloadableSessionsDir().resolve(path.getFileName());
                Files.move(path, unloadablePath, StandardCopyOption.REPLACE_EXISTING);
                log.info("Moved incompatible session to: {}", unloadablePath);
            } catch (IOException e) {
                log.error("Failed to move incompatible session to unloadable directory: {}", path, e);
            }
            return false;
        }
    }

    /**
     * Shuts down the container and its shared executor.
     */
    public void shutdown() {
        log.info("Shutting down AsiContainer: {}", hostApplicationId);
        executor.shutdown();
    }

    // --- STATIC METHODS FOR GLOBAL ACCESS ---
    
    /**
     * Static initializer to ensure the root working directory exists.
     */
    static {
        try {
            Files.createDirectories(getWorkDir());
        } catch (IOException e) {
            throw new RuntimeException("Could not create root work dir: " + getWorkDir(), e);
        }
    }

    /**
     * Gets the root Anahata AI working directory (e.g., ~/.anahata/asi).
     *
     * @return The root working directory path.
     */
    public static Path getWorkDir() {
        return Paths.get(System.getProperty("user.home"), ".anahata", "asi");
    }
    
    /**
     * Gets a named subdirectory within the global root working directory, 
     * creating it if it doesn't exist. This is used for shared resources 
     * like provider configurations.
     * e.g., ~/.anahata/asi/gemini
     *
     * @param name The name of the subdirectory.
     * @return The Path object for the subdirectory.
     */
    public static Path getWorkDirSubDir(String name) {
        Path dir = getWorkDir().resolve(name);
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            log.error("Could not create global subdirectory: {}", dir, e);
        }
        return dir;
    }
}
