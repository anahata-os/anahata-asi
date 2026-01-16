/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.tool;

import java.util.Collections;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.tool.java.JavaMethodToolResponse;

/**
 * An optional, abstract base class for toolkits that provides a rich,
 * context-aware API for tool execution, similar to a Servlet or EJB.
 * <p>
 * By extending this class, your tool methods can gain access to the current
 * execution context via a {@link ThreadLocal} managed by {@link JavaMethodToolResponse}, 
 * allowing them to log messages, attach files, and access the entire application 
 * state without needing any parameters in their method signatures.
 *
 * @author anahata-gemini-pro-2.5
 */
public abstract class AnahataToolkit extends HandyToolStuff {

    /**
     * Overridable method to provide additional context in the system instructions.
     * This allows a toolkit to inject static or dynamic rules and facts directly
     * into the model's core persona.
     * 
     * @param chat The chat session for which the instructions are being generated.
     * @return A list of system instruction strings.
     * @throws Exception if instruction generation fails.
     */
    public List<String> getSystemInstructionParts(Chat chat) throws Exception {
        return Collections.emptyList();
    }
    
    /**
     * Overridable method to add additional context to the RAG (Augmented Workspace) message.
     * This is called just-in-time before a prompt is sent, allowing the toolkit to
     * inject relevant stateful information.
     * 
     * @param ragMessage The RAG message to populate.
     * @throws Exception if population fails.
     */
    public void populateMessage(RagMessage ragMessage) throws Exception {
        
    }
}
