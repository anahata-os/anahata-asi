/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit;

import uno.anahata.asi.agi.resource.view.TextViewportSettings;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolException;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.ResourceManager2;
import uno.anahata.asi.toolkit.files.FullTextFileCreate;
import uno.anahata.asi.toolkit.files.FullTextFileUpdate;
import uno.anahata.asi.toolkit.files.TextFileReplacements;
import uno.anahata.asi.toolkit.files.TextReplacement;

/**
 * The definitive V2 toolkit for managing multimodal resources.
 * <p>
 * This toolkit is URI-centric and handles both reading (RAG) and writing 
 * (persistent mutations). It leverages the Handy Resource API for elegant 
 * content management.
 * </p>
 * 
 * @author anahata
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
            "- Use `updateViewport` to toggle line numbers, grep patterns, or tailing.\n" +
            "- Use `createTextFile` and `updateTextFile` for persistent storage operations.\n" +
            "- **CRITICAL**: You must load a file using `loadResources` before attempting to update it."
        );
    }

    /**
     * Intelligently resolves the actor string for registration heritage.
     * <p>
     * <b>Technical Purity:</b> Inspects the current tool execution context to 
     * identify the model and tool responsible for the registration.
     * </p>
     * 
     * @return The actor description string.
     */
    private String getActor() {
        String toolName = getResponse().getCall().getTool().getName();
        return getModelId() + " via @AiTool " + toolName;
    }

    /**
     * Loads multiple resources into the agi context in a single turn.
     * 
     * @param uriStrings The URIs to load.
     * @param initialSettings Optional initial viewport configuration.
     * @return The list of unique resource identifiers.
     * @throws Exception if loading fails.
     */
    @AiTool(value = "Loads multiple resources into the context by their URIs.", maxDepth = 12)
    public List<String> loadResources(
            @AiToolParam("The full URIs of the resources.") List<String> uriStrings,
            @AiToolParam(value = "Initial viewport settings for text resources.", required = false) TextViewportSettings initialSettings) throws Exception {
        
        List<Resource> toRegister = new ArrayList<>();
        List<String> ids = new ArrayList<>();
        ResourceManager2 manager = getAgi().getResourceManager2();
        
        for (String uriString : uriStrings) {
            Optional<Resource> existing = manager.findByUri(uriString);
            if (existing.isPresent()) {
                if (initialSettings != null && existing.get().getView() instanceof TextView tv) {
                    tv.getViewport().setSettings(initialSettings);
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
            
            toRegister.add(resource);
            ids.add(resource.getId());
        }
        
        manager.registerAll(toRegister, getActor());
        return ids;
    }

    /**
     * Updates the viewport configuration for an existing text resource.
     * 
     * @param resourceId The UUID of the resource.
     * @param settings The new settings.
     * @throws Exception if the resource is not found.
     */
    @AiTool("Updates the viewport configuration for a text resource.")
    public void updateViewport(
            @AiToolParam("The unique resource identifier.") String resourceId, 
            @AiToolParam("The new viewport settings.") TextViewportSettings settings) throws Exception {
        Resource res = getAgi().getResourceManager2().getResources().get(resourceId);
        if (res != null && res.getView() instanceof TextView tv) {
            tv.getViewport().setSettings(settings);
            tv.markDirty(); // Explicitly trigger re-interpretation
            log("Updated viewport for: " + res.getName());
        } else {
            throw new AiToolException("Resource not found or not textual: " + resourceId);
        }
    }

    /**
     * Unloads multiple resources from the context.
     * 
     * @param resourceIds The UUIDs to unregister.
     */
    @AiTool("Unloads multiple resources from the context.")
    public void unloadResources(@AiToolParam("The list of resource identifiers.") List<String> resourceIds) {
        for (String id : resourceIds) {
            getAgi().getResourceManager2().unregister(id);
        }
    }

    /**
     * Creates a new text file on the host filesystem and automatically registers it as a resource.
     * 
     * @param create The creation DTO.
     * @return The new resource UUID.
     * @throws Exception if creation fails.
     */
    @AiTool("Creates a new text file and registers it as a resource.")
    public String createTextFile(@AiToolParam("The file creation details.") FullTextFileCreate create) throws Exception {
        create.validate(getAgi());
        
        Path path = Paths.get(create.getPath());
        Files.createDirectories(path.getParent());
        
        // Final write always uses UTF-8 unless otherwise specified
        Files.writeString(path, create.getContent(), StandardCharsets.UTF_8);
        
        ResourceHandle handle = getAgi().getConfig().createResourceHandle(path.toUri());
        Resource resource = new Resource(handle);
        getAgi().getResourceManager2().register(resource, getActor());
        
        log("Created text file: " + create.getPath());
        return resource.getId();
    }

    /**
     * Updates an existing text file with full new content.
     * 
     * @param update The update DTO.
     * @throws Exception if the update fails.
     */
    @AiTool("Updates an existing text file using full content replacement.")
    public void updateTextFile(@AiToolParam("The update details.") FullTextFileUpdate update) throws Exception {
        update.validate(getAgi());
        
        Path path = Paths.get(update.getPath());
        Optional<Resource> res = getAgi().getResourceManager2().findByPath(path.toString());
        if (res.isPresent()) {
            // SINGULAR ENTRY POINT: The Resource orchestrator now manages both 
            // connectivity (disk write) and state (dirty marking).
            res.get().write(update.getNewContent());
            log("Updated text file: " + update.getPath());
        }
    }

    /**
     * Performs surgical text replacements in an existing file.
     * 
     * @param replacements The replacements DTO.
     * @throws Exception if replacements fail.
     */
    @AiTool("Performs multiple text replacements in a file.")
    public void replaceInTextFile(@AiToolParam("The set of replacements.") TextFileReplacements replacements) throws Exception {
        replacements.validate(getAgi());
        
        Path path = Paths.get(replacements.getPath());
        Optional<Resource> res = getAgi().getResourceManager2().findByPath(path.toString());
        
        if (res.isPresent()) {
            String content = res.get().asText();
            String updated = replacements.performReplacements(content);
            
            // SINGULAR ENTRY POINT: Management through the orchestrator API.
            res.get().write(updated);
            
            log("Performed replacements in: " + replacements.getPath());
        }
    }
}
