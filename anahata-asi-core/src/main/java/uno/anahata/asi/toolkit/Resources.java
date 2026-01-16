/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package uno.anahata.asi.toolkit;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.chat.Chat;
import uno.anahata.asi.context.ContextProvider;
import uno.anahata.asi.context.ContextManager;
import uno.anahata.asi.context.ContextPosition;
import uno.anahata.asi.model.core.RagMessage;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for managing local resources within the AI's context.
 * It provides tools for unregistering resources and implements the
 * {@link AnahataToolkit} lifecycle to populate the RAG message.
 *
 * @author anahata
 */
@AiToolkit("Tools for loading and managing local resources")
@Slf4j
public class Resources extends AnahataToolkit {

    /**
     * Removes the provided managed resources from the active workspace (RAG message).
     * 
     * @param resourceIds The absolute paths to the resources to unload.
     * @throws Exception if an error occurs during unregistration.
     */
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

    /**
     * Populates the RAG message with the content of all managed resources
     * that are configured for prompt augmentation.
     * 
     * @param ragMessage The RAG message to populate.
     * @throws Exception if population fails.
     */
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
