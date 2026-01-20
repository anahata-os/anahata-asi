/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
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
 * {@link AnahataToolkit} lifecycle to populate the RAG message and system instructions.
 *
 * @author anahata
 */
@AiToolkit("Tools for loading and managing local resources. Note: Resources with a LIVE refresh policy (the default) are automatically refreshed from disk right before the API call (when generating the RAG message), so there is no need to re-load them if they are already in the context.")
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
     * Injects managed resources configured for the SYSTEM_INSTRUCTIONS position.
     * Only resources with a 'text' content type are included.
     * 
     * @param chat The current chat session.
     * @return A list of rendered resource strings for the system instructions.
     * @throws Exception if rendering fails.
     */
    @Override
    public List<String> getSystemInstructions(Chat chat) throws Exception {
        List<String> parts = new ArrayList<>();
        for (AbstractResource resource : chat.getResourceManager().getResources()) {
            if (resource.getContextPosition() == ContextPosition.SYSTEM_INSTRUCTIONS) {
                if ("text".equals(resource.getContentType())) {
                    try {
                        Object content = resource.getContent();
                        if (content instanceof String text) {
                            parts.add(text);
                        }
                    } catch (Exception e) {
                        log.error("Error processing managed resource {} for system instructions", resource.getName(), e);
                    }
                } else {
                    log.error("Resource {} has position SYSTEM_INSTRUCTIONS but content type is not 'text' (it is '{}')", resource.getName(), resource.getContentType());
                }
            }
        }
        return parts;
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
