/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Fora Bara!
 */
package uno.anahata.ai.chat;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import lombok.extern.slf4j.Slf4j;

/**
 * A global registry for tracking active {@link Chat} sessions within the application.
 * This allows UI components to discover and manage all live AI conversations.
 * 
 * @author anahata-gemini-pro-2.5
 */
@Slf4j
public class ChatRegistry {

    private static final List<Chat> ACTIVE_CHATS = new CopyOnWriteArrayList<>();
    private static final List<ChatRegistryListener> LISTENERS = new CopyOnWriteArrayList<>();

    /**
     * Registers a new chat session.
     * @param chat The chat to register.
     */
    public static void register(Chat chat) {
        if (chat != null && !ACTIVE_CHATS.contains(chat)) {
            ACTIVE_CHATS.add(chat);
            log.info("Chat session registered: {}", chat.getConfig().getSessionId());
            fireChatRegistered(chat);
        }
    }

    /**
     * Unregisters a chat session.
     * @param chat The chat to unregister.
     */
    public static void unregister(Chat chat) {
        if (chat != null && ACTIVE_CHATS.remove(chat)) {
            log.info("Chat session unregistered: {}", chat.getConfig().getSessionId());
            fireChatUnregistered(chat);
        }
    }

    /**
     * Gets an unmodifiable list of all currently active chat sessions.
     * @return The list of active chats.
     */
    public static List<Chat> getActiveChats() {
        return Collections.unmodifiableList(ACTIVE_CHATS);
    }

    public static void addListener(ChatRegistryListener listener) {
        LISTENERS.add(listener);
    }

    public static void removeListener(ChatRegistryListener listener) {
        LISTENERS.remove(listener);
    }

    private static void fireChatRegistered(Chat chat) {
        for (ChatRegistryListener listener : LISTENERS) {
            listener.chatRegistered(chat);
        }
    }

    private static void fireChatUnregistered(Chat chat) {
        for (ChatRegistryListener listener : LISTENERS) {
            listener.chatUnregistered(chat);
        }
    }

    /**
     * Listener interface for chat registry events.
     */
    public interface ChatRegistryListener {
        void chatRegistered(Chat chat);
        void chatUnregistered(Chat chat);
    }
}
