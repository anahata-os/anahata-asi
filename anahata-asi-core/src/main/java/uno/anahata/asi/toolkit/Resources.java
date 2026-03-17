/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit;

import uno.anahata.asi.agi.resource.view.TextViewportSettings;
import uno.anahata.asi.agi.resource.view.TextView;
import uno.anahata.asi.agi.resource.handle.ResourceHandle;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.agi.tool.AiTool;
import uno.anahata.asi.agi.tool.AiToolException;
import uno.anahata.asi.agi.tool.AiToolkit;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.tool.AiToolParam;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.ResourceManager;
import uno.anahata.asi.internal.AnahataDiffUtils;
import uno.anahata.asi.toolkit.files.AbstractTextResourceWrite;
import uno.anahata.asi.toolkit.files.FullTextFileCreate;
import uno.anahata.asi.toolkit.files.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.files.TextResourceReplacements;
import uno.anahata.asi.toolkit.files.TextResourceLineBasedUpdates;

/**
 * The definitive V2 URI-centric toolkit for managed multimodal resources.
 * <p>
 * This toolkit provides a unified interface for RAG-based context augmentation 
 * and persistent, surgical mutations of text-based resources. It abstracts 
 * the complexities of different storage protocols (File, URL, String) while 
 * enforcing optimistic locking and context integrity.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AiToolkit("A URI-centric toolkit for managing resources.")
public class Resources extends AnahataToolkit {

    /** 
     * {@inheritDoc} 
     * <p>
     * Injects critical surgical precision rules into the model's system prompt, 
     * ensuring environmental awareness during file mutations.
     * </p>
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList(
                "**Resources Toolkit** (Surgical Precision Rules):\n"
                + "1. **Context Integrity**: Only modify resources currently in context. Always use the `lastModified` timestamp from the LATEST RAG message.\n"
                + "2. **Line Reference**: Line numbers are 1-based and must be verified against the RAG message before every call.\n"
                + "3. **Reasoning**: Always provide a meaningful `reason` for each replacement; it will be displayed as an AI comment in the UI.\n"
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
            @AiToolParam(value = "Initial viewport settings for text resources. If not provided, it uses the system default viewport (65K chars 1024 col width)", required = false) TextViewportSettings initialSettings) throws Exception {

        List<Resource> toRegister = new ArrayList<>();
        List<String> ids = new ArrayList<>();
        ResourceManager manager = getAgi().getResourceManager();

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
        Resource res = getAgi().getResourceManager().getResources().get(resourceId);
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
    @AiTool("Unloads multiple resources from the context (from the RAG Message).")
    public void unloadResources(@AiToolParam("The list of resource identifiers.") List<String> resourceIds) {
        for (String id : resourceIds) {
            Resource r = getAgi().getResourceManager().unregister(id);
            log("Unregistered " + r);
        }
    }

    /**
     * Creates a new text file on the host filesystem and automatically
     * registers it as a resource.
     *
     * @param create The creation DTO.
     * @return The new resource UUID.
     * @throws Exception if creation fails.
     */
    @AiTool("Creates a new text file and registers it as a resource (Will appear on the RAG message).")
    public String createTextFile(@AiToolParam("The file creation details.") FullTextFileCreate create) throws Exception {
        create.validate(getAgi());

        Path path = Paths.get(create.getPath());
        Files.createDirectories(path.getParent());

        // Final write always uses UTF-8 unless otherwise specified
        Files.writeString(path, create.getContent(), StandardCharsets.UTF_8);

        ResourceHandle handle = getAgi().getConfig().createResourceHandle(path.toUri());
        Resource resource = new Resource(handle);
        getAgi().getResourceManager().register(resource, getActor());

        log("Created text file: " + create.getPath());
        return resource.getId();
    }

    /**
     * Updates an existing text file using full content replacement.
     *
     * @param update The update DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if the update fails.
     */
    @AiTool("Updates an existing text file using full content replacement. Returns a standard unified diff of the changes applied.")
    public String updateTextResource(@AiToolParam("The update details.") FullTextResourceUpdate update) throws Exception {
        try {
            update.validate(getAgi());

            Resource res = getAgi().getResourceManager().getResources().get(update.getResourceUuid());
            if (res != null) {
                String original = res.asText();
                String revised = update.getNewContent();
                
                res.write(revised);
                log("Updated text file: " + res.getName());
                
                return AnahataDiffUtils.generateUnifiedDiff(res.getName(), original, revised);
            }
            return "";
        } catch (Exception e) {
            throw wrapWithDiff(update, e);
        }
    }

    /**
     * Performs surgical text replacements in an existing file.
     *
     * @param replacements The replacements DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if replacements fail.
     */
    @AiTool("Performs multiple text replacements in a text resource. Returns a standard unified diff of the changes applied.")
    public String findAndReplaceInTextResource(@AiToolParam("The set of replacements.") TextResourceReplacements replacements) throws Exception {
        try {
            replacements.validate(getAgi());

            Resource res = getAgi().getResourceManager().getResources().get(replacements.getResourceUuid());
            if (res != null) {
                String original = res.asText();
                String revised = replacements.performReplacements(original);

                res.write(revised);
                log("Performed replacements in: " + res.getName());
                
                return AnahataDiffUtils.generateUnifiedDiff(res.getName(), original, revised);
            }
            return "";
        } catch (Exception e) {
            throw wrapWithDiff(replacements, e);
        }
    }

    /**
     * Performs line-based replacements in an existing file.
     *
     * @param replacements The line replacements DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if replacements fail.
     */
    @AiTool("The worlds most token efficient tool. Only for text resources loaded with includeLineNumbers=true. "
            + " Performs multiple line-based updates strictly using 1-based line numbers. Unlike git-patch tools, **it requires NO anchors or context lines and it will fail if used in that manner**. It features 'Structural Newline Absorption' to handle model intuition. Returns a standard unified diff of the changes applied.")
    public String updateLinesInTextResource(@AiToolParam("The line-number based updates for the given resource.") TextResourceLineBasedUpdates replacements) throws Exception {
        try {
            replacements.validate(getAgi());

            Resource res = getAgi().getResourceManager().getResources().get(replacements.getResourceUuid());
            if (res != null) {
                String original = res.asText();
                String revised = replacements.performUpdates(original);

                res.write(revised);
                log("Performed replacements in: " + res.getName());
                
                return AnahataDiffUtils.generateUnifiedDiff(res.getName(), original, revised);
            }
            return "";
        } catch (Exception e) {
            throw wrapWithDiff(replacements, e);
        }
    }

    /**
     * Helper to wrap exceptions with a unified diff of the failed intent.
     * 
     * @param update The update operation.
     * @param e The original exception.
     * @return A new exception enriched with diff context.
     */
    private Exception wrapWithDiff(AbstractTextResourceWrite update, Exception e) {
        try {
            Resource res = getAgi().getResourceManager().getResources().get(update.getResourceUuid());
            if (res != null) {
                String current = res.asText();
                String proposed = update.calculateResultingContent(current);
                String diff = AnahataDiffUtils.generateUnifiedDiff(res.getName(), current, proposed);
                if (!diff.isBlank()) {
                    return new AiToolException(e.getMessage() + "\n\nProposed Diff (Not Applied):\n" + diff, e);
                }
            }
        } catch (Exception inner) {
            log.error("Failed to generate intent diff for error context", inner);
        }
        return e;
    }
}
