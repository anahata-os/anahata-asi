/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.resource.v2;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.tool.AiToolParam;

/**
 * The definitive V2 toolkit for managing multimodal resources.
 * <p>
 * This toolkit is URI-centric and supports both local and remote sources 
 * through the V2 Universal Resource Pipeline (URP).
 * </p>
 */
@Slf4j
@AiToolkit("A toolkit for managing V2 URI-centric resources.")
public class Resources extends AnahataToolkit {

    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
            "**Resources (V2) Toolkit Instructions**:\n" +
            "- Use `loadResources` to bring local or remote assets into context.\n" +
            "- Use `updateViewport` to toggle line numbers, grep patterns, or tailing."
        );
    }

    /**
     * Loads multiple resources into the agi context in a single turn.
     * 
     * @param uriStrings The list of full URIs to load.
     * @param initialSettings Optional initial viewport settings for text resources.
     * @return The list of unique resource IDs for the loaded sources.
     * @throws Exception if loading fails for any resource.
     */
    @AiTool(value = "Loads multiple resources into the context by their URIs.", maxDepth = 12)
    public List<String> loadResources(
            @AiToolParam("The full URIs of the resources.") List<String> uriStrings,
            @AiToolParam(value = "Initial viewport settings for text resources.", required = false) TextViewportSettings initialSettings) throws Exception {
        
        List<String> ids = new ArrayList<>();
        ResourceManager2 manager = getAgi().getResourceManager2();
        
        for (String uriString : uriStrings) {
            Optional<Resource> existing = manager.findByUri(uriString);
            if (existing.isPresent()) {
                if (initialSettings != null && existing.get().getView() instanceof TextView tv) {
                    tv.setViewportSettings(initialSettings);
                }
                ids.add(existing.get().getId());
                continue;
            }

            URI uri = URI.create(uriString);
            ResourceHandle handle = getAgi().getConfig().createResourceHandle(uri);
            Resource resource = new Resource(handle);
            
            if (initialSettings != null) {
                resource.setView(new TextView(resource, initialSettings));
            }
            
            manager.register(resource);
            ids.add(resource.getId());
        }
        return ids;
    }

    /**
     * Updates the viewport configuration for an existing text resource.
     * 
     * @param resourceId The unique identifier of the resource.
     * @param settings The new settings to apply.
     * @throws AiToolException if the resource is not found or is not textual.
     */
    @AiTool("Updates the viewport configuration for a text resource.")
    public void updateViewport(
            @AiToolParam("The unique resource identifier.") String resourceId, 
            @AiToolParam("The new viewport settings.") TextViewportSettings settings) throws Exception {
        Resource res = getAgi().getResourceManager2().getResources().get(resourceId);
        if (res != null && res.getView() instanceof TextView tv) {
            tv.setViewportSettings(settings);
            log("Updated viewport for: " + res.getName());
        } else {
            throw new AiToolException("Resource not found or not textual: " + resourceId);
        }
    }

    /**
     * Unloads multiple resources from the context.
     * 
     * @param resourceIds The list of unique identifiers to unload.
     */
    @AiTool("Unloads multiple resources from the context.")
    public void unloadResources(@AiToolParam("The list of resource identifiers.") List<String> resourceIds) {
        for (String id : resourceIds) {
            getAgi().getResourceManager2().unregister(id);
        }
    }
}
