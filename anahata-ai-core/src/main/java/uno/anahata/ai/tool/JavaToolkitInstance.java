/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.internal.TikaUtils;
import uno.anahata.ai.model.core.RagMessage;
import uno.anahata.ai.model.tool.java.JavaMethodTool;
import uno.anahata.ai.model.tool.java.JavaMethodToolCall;
import uno.anahata.ai.model.tool.java.JavaMethodToolResponse;
import uno.anahata.ai.resource.ResourceManager;

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
public abstract class JavaToolkitInstance {

    /**
     * Gets the current tool response from the thread-local context.
     *
     * @return The current response.
     * @throws IllegalStateException if called outside the scope of a tool execution.
     */
    protected JavaMethodToolResponse getResponse() {
        JavaMethodToolResponse response = JavaMethodToolResponse.getCurrent();
        if (response == null) {
            throw new IllegalStateException("Cannot access ToolContext outside of a tool execution thread.");
        }
        return response;
    }

    /**
     * Convenience method to get the current tool call.
     * @return The current tool call.
     */
    protected JavaMethodToolCall getCall() {
        return getResponse().getCall();
    }

    /**
     * Convenience method to get the current tool definition.
     * @return The current tool.
     */
    protected JavaMethodTool getTool() {
        return getCall().getTool();
    }

    /**
     * Convenience method to get the application's ToolManager.
     * @return The ToolManager.
     */
    protected ToolManager getToolManager() {
        return getTool().getToolkit().getToolManager();
    }
    
    /**
     * Convenience method to get the application's ResourceManager.
     * @return The ResourceManager.
     */
    protected ResourceManager getResourceManager() {
        return getChat().getResourceManager();
    }

    /**
     * Convenience method to get the parent ToolManager session.
     * @return The ToolManager session.
     */
    protected Chat getChat() {
        return getToolManager().getChat();
    }

    /**
     * Adds a log message to the current tool's response.
     * @param message The log message.
     */
    protected void log(String message) {
        getResponse().addLog(message);
    }
    
    /**
     * Adds a log message to the current tool's response.
     * @param message The log message.
     */
    protected void error(String message) {
        getResponse().addError(message);
    }

    /**
     * Attaches a binary blob to the current tool's response.
     * @param data The binary data.
     * @param mimeType The MIME type of the data.
     */
    protected void addAttachment(byte[] data, String mimeType) {
        getResponse().addAttachment(data, mimeType);
    }
    
    /**
     * Convenience method to attach a file to the current tool's response.
     * The MIME type is detected automatically.
     * @param file The file to attach.
     * @throws IOException if the file cannot be read.
     */
    protected void addAttachment(File file) throws IOException {
        addAttachment(file.toPath());
    }
    
    /**
     * Convenience method to attach a file to the current tool's response.
     * The MIME type is detected automatically.
     * @param path The path to the file to attach.
     * @throws IOException if the file cannot be read.
     */
    protected void addAttachment(Path path) throws IOException {
        byte[] data = Files.readAllBytes(path);
        String mimeType;
        try {
            mimeType = TikaUtils.detectMimeType(path.toFile());
        } catch (Exception e) {
            throw new IOException("Failed to detect MIME type for " + path, e);
        }
        addAttachment(data, mimeType);
    }
    
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
