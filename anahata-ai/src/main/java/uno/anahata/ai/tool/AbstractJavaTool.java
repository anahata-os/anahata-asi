/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.ai.tool;

import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.model.core.BlobPart;
import uno.anahata.ai.model.core.TextPart;
import uno.anahata.ai.model.tool.java.JavaMethodTool;
import uno.anahata.ai.model.tool.java.JavaMethodToolCall;
import uno.anahata.ai.model.tool.java.JavaMethodToolResponse;
import uno.anahata.ai.resource.ResourceManager;

/**
 * An optional, abstract base class for toolkits that provides a rich,
 * context-aware API for tool execution, similar to a Servlet or EJB.
 * <p>
 * By extending this class, your tool methods can gain access to the current
 * execution context via a {@link ThreadLocal}, allowing them to log messages,
 * attach files, and access the entire application state without needing any
 * parameters in their method signatures.
 *
 * @author anahata-gemini-pro-2.5
 */
public abstract class AbstractJavaTool {

    private static final ThreadLocal<JavaMethodToolResponse> context = new ThreadLocal<>();

    /**
     * Sets the context for the current thread. Called by the framework just
     * before a tool method is invoked.
     *
     * @param response The current tool response.
     */
    public void setContext(JavaMethodToolResponse response) {
        context.set(response);
    }

    /**
     * Clears the context for the current thread. Called by the framework in a
     * finally block after the tool method completes.
     */
    public void clearContext() {
        context.remove();
    }

    /**
     * Gets the current tool response from the thread-local context.
     *
     * @return The current response.
     * @throws IllegalStateException if called outside the scope of a tool execution.
     */
    protected JavaMethodToolResponse getResponse() {
        JavaMethodToolResponse response = context.get();
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
     * Attaches a binary part (e.g., an image) to the current tool's response.
     * @param attachment The BlobPart to attach.
     */
    /*
    protected void attach(BlobPart attachment) {
        getResponse().addAttachment(attachment);
    }*/

    /**
     * Attaches a text part (e.g., a code snippet) to the current tool's response.
     * @param attachment The TextPart to attach.
     */
    /*
    protected void attach(TextPart attachment) {
        getResponse().addAttachment(attachment);
    }*/
}
