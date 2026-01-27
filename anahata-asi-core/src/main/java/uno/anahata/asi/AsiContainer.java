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
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.kryo.KryoUtils;
import uno.anahata.asi.model.core.BasicPropertyChangeSource;

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
public class AsiContainer extends BasicPropertyChangeSource {
    
    /** The unique identifier for the host application (e.g., "netbeans"). */
    private final String hostApplicationId;
    
    /** The persistent preferences for this container instance. */
    private final AsiContainerPreferences preferences;
    
    /** The list of currently active chat sessions managed by this container. */
    private final List<Chat> activeChats = new ArrayList<>();
    
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
    }

    /**
     * Saves the current preferences for this host application to disk.
     */
    public void savePreferences() {
        preferences.save(this);
    }
    
    /**
     * Gets the root working directory for this specific host application instance.
     * e.g., ~/.anahata/ai/netbeans
     *
     * @return The application-specific working directory path.
     */
    public Path getAppDir() {
        return getWorkDirSubDir(hostApplicationId);
    }
    
    /**
     * Gets a named subdirectory within this host application's working directory, 
     * creating it if it doesn't exist.
     * e.g., ~/.anahata/ai/netbeans/sessions
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
     * Registers a new chat session with this configuration and triggers the 
     * {@link #onChatCreated(Chat)} hook. Fires a property change event for "activeChats".
     * 
     * @param chat The chat session to register.
     */
    public void register(Chat chat) {
        synchronized (activeChats) {
            for (Chat existing : activeChats) {
                if (existing.getConfig().getSessionId().equals(chat.getConfig().getSessionId())) {
                    log.warn("Chat session {} already registered. Skipping.", chat.getConfig().getSessionId());
                    return;
                }
            }
            List<Chat> old = new ArrayList<>(activeChats);
            activeChats.add(chat);
            onChatCreated(chat);
            autoSaveSession(chat); // Ensure the session is persisted in the active directory
            propertyChangeSupport.firePropertyChange("activeChats", old, Collections.unmodifiableList(activeChats));
            log.info("Registered chat session: {}", chat.getConfig().getSessionId());
        }
    }

    /**
     * Unregisters a chat session from this configuration. 
     * Fires a property change event for "activeChats".
     * 
     * @param chat The chat session to unregister.
     */
    public void unregister(Chat chat) {
        synchronized (activeChats) {
            List<Chat> old = new ArrayList<>(activeChats);
            if (activeChats.remove(chat)) {
                propertyChangeSupport.firePropertyChange("activeChats", old, Collections.unmodifiableList(activeChats));
                log.info("Unregistered chat session: {}", chat.getConfig().getSessionId());
            }
        }
    }

    /**
     * Gets an unmodifiable list of all active chat sessions.
     * 
     * @return The list of active chats.
     */
    public List<Chat> getActiveChats() {
        synchronized (activeChats) {
            return Collections.unmodifiableList(new ArrayList<>(activeChats));
        }
    }
    
    /**
     * Overridable hook for host-specific initialization when a new chat is created.
     * 
     * @param chat The newly created chat session.
     */
    public void onChatCreated(Chat chat) {
        // Default implementation does nothing.
    }
    
    /**
     * Creates a new chat session with a default configuration.
     * This method should be overridden by host-specific containers.
     * 
     * @return The newly created chat session.
     */
    public Chat createNewChat() {
        throw new UnsupportedOperationException("createNewChat() not implemented for this container.");
    }

    // --- SESSION PERSISTENCE ---

    /**
     * Gets the directory where active chat sessions are stored.
     * 
     * @return The sessions directory path.
     */
    public Path getSessionsDir() {
        return getAppDirSubDir("sessions");
    }

    /**
     * Gets the directory where manually saved chat sessions are stored.
     * 
     * @return The saved sessions directory path.
     */
    public Path getSavedSessionsDir() {
        Path dir = getSessionsDir().resolve("saved");
        ensureDirectory(dir);
        return dir;
    }

    /**
     * Gets the directory where disposed chat sessions are moved.
     * 
     * @return The disposed sessions directory path.
     */
    public Path getDisposedSessionsDir() {
        Path dir = getSessionsDir().resolve("disposed");
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
     * 
     * @param chat The chat session to save.
     */
    public void autoSaveSession(Chat chat) {
        saveSessionTo(chat, getSessionsDir());
    }

    /**
     * Manually saves the session to the 'saved' directory.
     * 
     * @param chat The chat session to save.
     */
    public void manualSaveSession(Chat chat) {
        saveSessionTo(chat, getSavedSessionsDir());
    }

    /**
     * Serializes and saves a chat session to a specific directory using Kryo.
     * This method is synchronized on the chat instance to prevent concurrent write issues.
     * 
     * @param chat The chat session to save.
     * @param dir The destination directory.
     */
    private void saveSessionTo(Chat chat, Path dir) {
        synchronized (chat) {
            Path file = dir.resolve(chat.getConfig().getSessionId() + ".kryo");
            try {
                log.info("Saving session {} to {}", chat.getConfig().getSessionId(), file);
                byte[] data = KryoUtils.serialize(chat);
                Files.write(file, data);
            } catch (IOException e) {
                log.error("Failed to save session: {}", chat.getConfig().getSessionId(), e);
            }
        }
    }

    /**
     * Permanently disposes of a chat session, shutting it down and moving its 
     * serialized file to the 'disposed' directory.
     * 
     * @param chat The chat session to dispose.
     */
    public void dispose(Chat chat) {
        String sessionId = chat.getConfig().getSessionId();
        log.info("Disposing session: {}", sessionId);
        
        // 1. Shutdown the chat (stops executors, etc.)
        chat.shutdown();
        
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
        unregister(chat);
    }

    /**
     * Imports a chat session from an external file. The session is assigned a 
     * new ID to avoid collisions and registered as a new active chat.
     * 
     * @param path The path to the serialized session file.
     * @return The imported Chat session, or null if import failed.
     */
    public Chat importSession(Path path) {
        try {
            log.info("Importing session from {}", path);
            byte[] data = Files.readAllBytes(path);
            Chat chat = KryoUtils.deserialize(data, Chat.class);
            
            // Always generate a new session ID for imported sessions to avoid collisions
            chat.getConfig().setSessionId(UUID.randomUUID().toString());
            
            chat.rebind(this);
            register(chat);
            return chat;
        } catch (Exception e) {
            log.error("Failed to import session from {}", path, e);
            return null;
        }
    }

    /**
     * Scan the sessions directory and loads all serialized chat sessions.
     * This is typically called during application startup.
     */
    public void loadSessions() {
        Path sessionsDir = getSessionsDir();
        if (!Files.exists(sessionsDir)) return;

        try (Stream<Path> stream = Files.list(sessionsDir)) {
            stream.filter(p -> !Files.isDirectory(p)) // Only load files from the root (active sessions)
                  .filter(p -> p.toString().endsWith(".kryo"))
                  .forEach(this::loadSession);
        } catch (IOException e) {
            log.error("Failed to list sessions in {}", sessionsDir, e);
        }
    }

    /**
     * Loads a single chat session from a file, rebinds it to this container, 
     * and registers it.
     * 
     * @param path The path to the serialized session file.
     */
    private void loadSession(Path path) {
        try {
            log.info("Loading session from {}", path);
            byte[] data = Files.readAllBytes(path);
            Chat chat = KryoUtils.deserialize(data, Chat.class);
            chat.rebind(this);
            register(chat);
        } catch (Exception e) {
            log.error("Failed to load session from {}", path, e);
        }
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
     * Gets the root Anahata AI working directory (e.g., ~/.anahata/ai).
     *
     * @return The root working directory path.
     */
    public static Path getWorkDir() {
        return Paths.get(System.getProperty("user.home"), ".anahata", "ai");
    }
    
    /**
     * Gets a named subdirectory within the global root working directory, 
     * creating it if it doesn't exist. This is used for shared resources 
     * like provider configurations.
     * e.g., ~/.anahata/ai/gemini
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
