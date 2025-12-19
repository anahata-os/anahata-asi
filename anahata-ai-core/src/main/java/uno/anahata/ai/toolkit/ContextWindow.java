/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.ai.toolkit;

import java.util.Collections;
import java.util.List;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.context.ContextProvider;
import uno.anahata.ai.context.ContextManager;
import uno.anahata.ai.model.core.RagMessage;
import uno.anahata.ai.model.resource.AbstractResource;
import uno.anahata.ai.model.resource.TextFileResource;
import uno.anahata.ai.resource.ResourceManager;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.AiToolParam;
import uno.anahata.ai.tool.AiToolkit;
import uno.anahata.ai.tool.JavaToolkitInstance;

/**
 *
 * @author pablo
 */
@AiToolkit("Tools for managing the context window: ")
public class ContextWindow extends JavaToolkitInstance{
    
    @AiTool(value = "Removes the provided managed resources from the active workspace (RAG message).", retention = 0)
    public void unregisterResource(
            @AiToolParam("The absolute paths to the resources to unload.") List<String> resourceIds) throws Exception {
        ResourceManager rm = getResourceManager();
        for (String resourceId : resourceIds) {
            log("Unregistering... " + resourceId);
            AbstractResource resource = rm.unregister(resourceId);
            if (resource != null) {
                log("Unregistered OK " + resourceId);
            } else {
                error("Resource not found " + resourceId);
            }
        }
    }
    
    @AiTool(value = "Enables / disables context providers", retention = 0)
    public void updateContextProviders(
            @AiToolParam("Whether to enable or disable the providers.") boolean enabled, 
            @AiToolParam("The IDs of the context providers to update.") List<String> providerIds) {
        ContextManager cm = getChat().getContextManager();
        for (ContextProvider cp : cm.getProviders()) {
            if (providerIds.contains(cp.getId())) {
                cp.setEnabled(enabled);
                log((enabled ? "Enabled" : "Disabled") + " provider: " + cp.getName());
            }
        }
    }
    
    
    //@AiTool(value = "Loads a text file into the context as a managed resource.", retention = 0)
    private String getConversationSummary(Chat chat) {
        ContextManager cm = chat.getContextManager();
        return "";
    }
    
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        String chatHistory = "**Chat History**\n\n";
        
    }

    @Override
    public List<String> getSystemInstructionParts(Chat chat) throws Exception {
        return Collections.emptyList();
    }
    
}
