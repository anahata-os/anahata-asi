/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;
import lombok.Getter;
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
    private final String hostApplicationId;
    private final AsiContainerPreferences preferences;
    private final List<Chat> activeChats = new ArrayList<>();

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
     * @return The application-specific working directory.
     */
    public Path getAppDir() {
        return getWorkDirSubDir(hostApplicationId);
    }
    
    /**
     * Gets a named subdirectory within this host application's working directory, creating it if it doesn't exist.
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
     * Registers a new chat session with this configuration.
     * 
     * @param chat The chat session to register.
     */
    public void register(Chat chat) {
        List<Chat> old = new ArrayList<>(activeChats);
        activeChats.add(chat);
        onChatCreated(chat);
        propertyChangeSupport.firePropertyChange("activeChats", old, Collections.unmodifiableList(activeChats));
        log.info("Registered chat session: {}", chat.getConfig().getSessionId());
    }

    /**
     * Unregisters a chat session from this configuration.
     * 
     * @param chat The chat session to unregister.
     */
    public void unregister(Chat chat) {
        List<Chat> old = new ArrayList<>(activeChats);
        if (activeChats.remove(chat)) {
            propertyChangeSupport.firePropertyChange("activeChats", old, Collections.unmodifiableList(activeChats));
            log.info("Unregistered chat session: {}", chat.getConfig().getSessionId());
        }
    }

    /**
     * Gets an unmodifiable list of all active chat sessions.
     * 
     * @return The list of active chats.
     */
    public List<Chat> getActiveChats() {
        return Collections.unmodifiableList(activeChats);
    }
    
    /**
     * Overridable hook for host-specific initialization when a new chat is created.
     * 
     * @param chat The newly created chat session.
     */
    public void onChatCreated(Chat chat) {
        // Default implementation does nothing.
    }

    // --- SESSION PERSISTENCE ---

    /**
     * Gets the directory where chat sessions are stored for this application.
     * 
     * @return The sessions directory.
     */
    public Path getSessionsDir() {
        return getAppDirSubDir("sessions");
    }

    /**
     * Gets the directory where disposed chat sessions are moved.
     * 
     * @return The disposed sessions directory.
     */
    public Path getDisposedSessionsDir() {
        Path dir = getSessionsDir().resolve("disposed");
        try {
            Files.createDirectories(dir);
        } catch (IOException e) {
            log.error("Could not create disposed sessions directory: {}", dir, e);
        }
        return dir;
    }

    /**
     * Serializes and saves a chat session to the sessions directory.
     * 
     * @param chat The chat session to save.
     */
    public void saveSession(Chat chat) {
        Path file = getSessionsDir().resolve(chat.getConfig().getSessionId() + ".kryo");
        try {
            log.info("Saving session {} to {}", chat.getConfig().getSessionId(), file);
            byte[] data = KryoUtils.serialize(chat);
            Files.write(file, data);
        } catch (IOException e) {
            log.error("Failed to save session: {}", chat.getConfig().getSessionId(), e);
        }
    }

    /**
     * Scans the sessions directory and loads all serialized chat sessions.
     * This is typically called during application startup.
     */
    public void loadSessions() {
        Path sessionsDir = getSessionsDir();
        if (!Files.exists(sessionsDir)) return;

        try (Stream<Path> stream = Files.list(sessionsDir)) {
            stream.filter(p -> p.toString().endsWith(".kryo"))
                  .forEach(this::loadSession);
        } catch (IOException e) {
            log.error("Failed to list sessions in {}", sessionsDir, e);
        }
    }

    /**
     * Loads a single chat session from a file and registers it.
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
     * Gets a named subdirectory within the global root working directory, creating it if it doesn't exist.
     * This is used for shared resources like provider configurations.
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
