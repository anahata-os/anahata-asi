/*
 * Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça!
 */
package uno.anahata.asi.toolkit;

import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.resource.ResourceManager;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolParam;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;

/**
 * A toolkit for managing local resources (files, screen devices, etc.) within 
 * the AI's context.
 * <p>
 * It provides tools for unregistering resources. The actual context injection 
 * is handled by the {@link ResourceManager} which acts as a {@link uno.anahata.asi.context.ContextProvider}.
 * </p>
 *
 * @author anahata
 */
@AiToolkit("Tools for managing local resources (files, screen devices, etc.). Note: Resources with a LIVE refresh policy (the default) are automatically refreshed from disk right before the API call, so there is no need to re-load them if they are already in the context.")
@Slf4j
public class Resources extends AnahataToolkit {

    /**
     * Removes the provided managed resources from the active workspace.
     * 
     * @param resourceIds The unique IDs of the resources to unload.
     * @throws Exception if an error occurs during unregistration.
     */
    @AiTool(value = "Removes the provided managed resources from the active workspace.", maxDepth = 12)
    public void unregisterResource(
            @AiToolParam("The unique IDs of the resources to unload.") List<String> resourceIds) throws Exception {
        ResourceManager rm = getResourceManager();
        for (String resourceId : resourceIds) {
            log("Unregistering... " + resourceId);
            AbstractResource<?, ?> resource = rm.unregister(resourceId);
            if (resource != null) {
                log("Unregistered OK " + resourceId);
            } else {
                error("Resource not found " + resourceId);
            }
        }
    }
}
