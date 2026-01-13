/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.tool;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.internal.TikaUtils;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.tool.java.JavaMethodTool;
import uno.anahata.asi.model.tool.java.JavaMethodToolCall;
import uno.anahata.asi.model.tool.java.JavaMethodToolResponse;
import uno.anahata.asi.resource.ResourceManager;

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
     * Overridable method to provide additional context in the system instructions
     * 
     * @param chat the chat for which the system instruction parts are being provided
     * @return the system instruction parts
     * @throws Exception 
     */
    public List<String> getSystemInstructionParts(Chat chat) throws Exception {
        return Collections.emptyList();
}
    
    /**
     * Overridable method to add additional context to the rag message.
     * 
     * @param ragMessage the rag message
     * @throws Exception 
     */
    public void populateMessage(RagMessage ragMessage) throws Exception {
        
    }
}
