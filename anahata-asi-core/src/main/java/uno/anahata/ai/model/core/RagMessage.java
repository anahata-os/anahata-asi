package uno.anahata.ai.model.core;

import java.util.ArrayList;
import java.util.List;
import uno.anahata.ai.chat.Chat;

/**
 * A specialized UserMessage that represents just-in-time, ephemeral context
 * provided by the environment (Retrieval-Augmented Generation).
 * <p>
 * This message and its parts are not part of the persistent conversation history
 * and are exempt from all pruning logic.
 *
 * @author Anahata
 */
public class RagMessage extends UserMessage {

    public RagMessage(Chat chat) {
        super(chat);
        new TextPart(this, "**RAG Message (Live Workspace)**\n\n This message is dynamically generated at runtime (not written by the user) and populated by all enabled context providers after tool execution");
    }
    
    /**
     * Convenience method to add a text part to this RAG message.
     * 
     * @param text The text content to add.
     */
    public void addPart(String text) {
        new TextPart(this, text);
    }

    /**
     * Overrides the default pruning logic to declare that this message is
     * ephemeral and should never be pruned. This is the key to preventing
     * the pruning system from trying to access the (null) Chat context
     * of this non-history message.
     *
     * @return Always {@code false}.
     */
    @Override
    public boolean isEffectivelyPruned() {
        return false;
    }
    
    /**
     * Overrides the default part filtering logic. Since a RagMessage is always
     * ephemeral and relevant for the current turn, it always returns all of its
     * parts, regardless of the pruning flag.
     *
     * @param includePruned This parameter is ignored.
     * @return A new list containing all parts of this message.
     */
    @Override
    public List<AbstractPart> getParts(boolean includePruned) {
        return getParts();
    }

    @Override
    public String getFrom() {
        return "Java workspace";
    }
    
    
}
