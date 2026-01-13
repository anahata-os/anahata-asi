/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.ai.toolkit;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.ai.chat.Chat;
import uno.anahata.ai.context.ContextProvider;
import uno.anahata.ai.context.ContextManager;
import uno.anahata.ai.context.ContextPosition;
import uno.anahata.ai.model.core.RagMessage;
import uno.anahata.ai.model.resource.AbstractResource;
import uno.anahata.ai.model.resource.TextFileResource;
import uno.anahata.ai.resource.ResourceManager;
import uno.anahata.ai.tool.AiTool;
import uno.anahata.ai.tool.AiToolParam;
import uno.anahata.ai.tool.AiToolkit;
import uno.anahata.ai.tool.AnahataToolkit;

/**
 *
 * @author anahata
 */
@AiToolkit("Tools for loading and managing local resources")
@Slf4j
public class Resources extends AnahataToolkit {

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

    
    @Override
    public void populateMessage(RagMessage ragMessage) throws Exception {
        for (AbstractResource resource : ragMessage.getChat().getResourceManager().getResources()) {
            try {
                if (resource.getContextPosition() == ContextPosition.PROMPT_AUGMENTATION) {
                    // Delegate rendering to the resource itself
                    resource.populate(ragMessage);
                }
            } catch (Exception e) {
                log.error("Error processing managed resource {} for prompt augmentation", resource.getName(), e);
            }
        }
    }

}
