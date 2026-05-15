package uno.anahata.asi.toolkit.resources;

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
import uno.anahata.asi.agi.tool.AgiToolException;
import uno.anahata.asi.agi.tool.AnahataToolkit;
import uno.anahata.asi.agi.resource.Resource;
import uno.anahata.asi.agi.resource.ResourceManager;
import uno.anahata.asi.agi.tool.ToolPermission;
import uno.anahata.asi.toolkit.resources.text.FullTextFileCreate;
import uno.anahata.asi.toolkit.resources.text.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.resources.text.TextResourceReplacements;
import uno.anahata.asi.toolkit.resources.text.lines.TextResourceLineEdits;
import uno.anahata.asi.agi.tool.AgiToolkit;
import uno.anahata.asi.agi.tool.AgiToolParam;
import uno.anahata.asi.agi.tool.AgiTool;

/**
 * The definitive V2 URI-centric toolkit for managed multimodal resources.
 * <p>
 * This toolkit provides a unified interface for RAG-based context augmentation
 * and persistent, surgical mutations of text-based resources. It abstracts the
 * complexities of different storage protocols (File, URL, String) while
 * enforcing optimistic locking and context integrity.
 * </p>
 *
 * @author anahata
 */
@Slf4j
@AgiToolkit("A URI-centric toolkit for managing resources.")
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
                "**Resources Toolkit**:\n"
                + "In The Anahata ASI platform, resources can be local or remote, and they have a ContextPosition attribute that can take the following values:\n"
                + "\tSYSTEM_INSTRUCTIONS: They are loaded directly into the system instructions (only the user or the platform can do this).\n"
                + "\tPROMPT_AUGMENTATION: Their are included on the RAG message.\n"
                + "Regardless of the position, resources are always preceeded by a beautiful header containing all the metadata about the resource and they can "
                + "be updated regarless of the position for as long as they are 'writable'. You don't need to re-load a resource positioned as SYSTEM_INSTRUCTIONS "
                + "to make updates to it. All you need is the uuid and the lastModified timestamp.\n\n "
                + "*\nVery important rules for working with resources:*\n"
                + "1. **Context Integrity**: You can only modify resources currently in context and only the ones that are 'providing' and only if you have that part of the resource visible in the viewport. Always use the `lastModified` timestamp from the resource header in the RAG message. This applies to all tools that modify resources **Do not rely on your internal memory of the previous successful edit**\n"
                + "2. **Reasoning**: Always provide a meaningful `reason` each time you update a resource; it will be displayed as an AI comment in the UI.\n"
                + "3. **Updating resources**: All update resource tools flush the changes to disk inmediatly when `EXECUTED`.\n "
                + "4. **Rag Message**: The Rag Message is the source of truth for resource modifications, it gets freshly generated when the user completes his turn (i.e. after all tools in the batch have been executed or declined). "
                + "All resources registered with `LIVE` refresh policy are garanteed to be up to date (in sync) with the underlying storage.\n"
                + "5. **Your risponsability**: You are risponsible for managing what resources are in context, if the user wants to switch task or there is not enough context window left, it is your risponsability to set them to not providing (if you think you may need them later) or to unload the from context all together (unless instructed by the user to keep them in context). A true ASI can hold infinite sessions without burning input tokens with resources that are not longer relevant.\n"
                + "6. **Using the Resources toolkit for coding java**: If you are going to use findAndReplaceInTextResource for coding java, always make sure that you include an extra replacement for any neccesary imports regardless of wether other toolktis can add imports, avoid fqns in class members whenever you can."
        /*+ "5. **Resources.editTextResource tool**: This is not a git style tool that requires surrounding anchor lines. It is a strict, surgical 1-based line number tool with optimistic locking validation for text resources loaded with includeLineNumbers=true."
                        + " The UI for this tool shows the user a rich graphical diff visualizer with the edits you intend to make to the text resource and overlays comic-style annotations with the reasons for your edits on the right hand side of the diff viewer. "
                        + "\n\tUse this tool **paying careful attention to the line numbers in the RAG message** and use it in a **user-oriented way** choosing the appropiate type of edit (insert / replace / delete) for each logical change you intend to make."
                        + " **\n\tDo not use replacements with existing surrounding lines that need no change when you can perform the edits with pure inserts**. This is very important for the following reasons: "
                        + "\n\ta)Providing surrounding anchor/context lines is prone to fail as this is not the way the tool is designed (its based on optimstic locking, not in matching anchors)."
                        + "\n\tb)The graphical diff visualizer will not display the changes you intend to make to the text resource (because it will not highlight surrounding lines that have not changed)"
                        + "\n\tc)Will cause the comic-style bubbles to be offset (on a line that has no changes)."
                        + "\n\tFor these reasons, when using the editTextResource tool, **you MUST always choose 'inserts' over 'replacements' when possible**.\n"
                        + "\n\tWhen adding Javadoc or comments, always use LineInsertion unless you are explicitly correcting an existing (and poorly formatted) comment. Replacing a line with 'itself plus more' is a common source of coordinate errors."
                        + "\n\tBoundary Syntax Check: Before finalizing a range, check the lines immediately above (startLine - 1) and below (endLine + 1). If they contain syntax markers like , {, or }, ensure you aren't accidentally orphaning them or creating duplicates.";*/
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
        return getModelId() + " via @AgiTool " + toolName;
    }

    /**
     * Loads multiple resources into the agi context in a single turn.
     *
     * @param uriStrings The URIs to load.
     * @param initialSettings Optional initial viewport configuration.
     * @return The list of unique resource identifiers.
     * @throws Exception if loading fails.
     */
    @AgiTool(value = "Loads multiple resources into the context by their URIs.", permission = ToolPermission.APPROVE_ALWAYS)
    public List<String> loadResources(
            @AgiToolParam(value = "The full URIs of the resources.", rendererId = "uri") List<String> uriStrings,
            @AgiToolParam(value = "Initial viewport settings for text resources. If not provided, it uses the system default viewport (0-65K chars, 1024 chars col width incluedLines=true)", required = false) TextViewportSettings initialSettings) throws Exception {

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

        manager.registerAll(toRegister, "Resource Loaded by " + getActor());
        return ids;
    }

    /**
     * Updates the viewport configuration for an existing text resource.
     *
     * @param resourceId The UUID of the resource.
     * @param settings The new settings.
     * @throws Exception if the resource is not found.
     */
    @AgiTool("Updates the viewport configuration for a text resource. This is not a resource reload tool. Do not updateViewport without any changes as a means to re-read a resource from disk. This is just to update the viewport settings if the resource is too large, the viewport doesn't include the desired range, toggle line numbers, etc.")
    public void updateViewport(
            @AgiToolParam(value = "The unique resource identifier.", rendererId = "resource") String resourceId,
            @AgiToolParam("The new viewport settings.") TextViewportSettings settings) throws Exception {
        Resource res = getAgi().getResourceManager().getResources().get(resourceId);
        if (res != null && res.getView() instanceof TextView tv) {
            tv.getViewport().setSettings(settings);
            tv.markDirty(); // Explicitly trigger re-interpretation
            log("Updated viewport for: " + res.getName());
        } else {
            throw new AgiToolException("Resource not found or not textual: " + resourceId);
        }
    }

    /**
     * Unloads multiple resources from the context.
     *
     * @param resourceIds The UUIDs to unregister.
     */
    @AgiTool(value = "Unloads multiple resources from the context (from the RAG Message).", maxDepth = 2)
    public void unloadResources(@AgiToolParam(value = "The list of resource identifiers.", rendererId = "resource") List<String> resourceIds) {
        List<Resource> unregistered = getAgi().getResourceManager().unregisterAll(resourceIds);
        for (Resource r : unregistered) {
            log("Unregistered resource: " + r.getName());
        }

        if (unregistered.size() < resourceIds.size()) {
            List<String> unregisteredIds = unregistered.stream().map(Resource::getId).toList();
            for (String id : resourceIds) {
                if (!unregisteredIds.contains(id)) {
                    error("Failed to unregister resource: UUID '" + id + "' not found in registry.");
                }
            }
        }
    }

    /**
     * Unloads resources by their URIs.
     *
     * @param uris The list of URIs to unload.
     * @throws Exception if a URI matches multiple resources.
     */
    @AgiTool(value = "Unloads multiple resources from the context by their URI for as long as only one resource is registered for that URI.", maxDepth = 2)
    public void unloadResourcesByUri(@AgiToolParam(value = "The list of resource URIs.", rendererId = "uri") List<String> uris) throws Exception {
        ResourceManager manager = getAgi().getResourceManager();
        List<String> uuidsToUnload = new ArrayList<>();
        
        for (String uri : uris) {
            List<Resource> matches = manager.getResourcesList().stream()
                    .filter(r -> r.getHandle().getUri().toString().equals(uri))
                    .toList();
            if (matches.size() > 1) {
                throw new AgiToolException("Cannot unload by URI. Multiple resources found for URI: " + uri);
            } else if (matches.size() == 1) {
                uuidsToUnload.add(matches.get(0).getId());
            } else {
                log.warn("No resource found in context for URI: {}", uri);
            }
        }
        
        if (!uuidsToUnload.isEmpty()) {
            unloadResources(uuidsToUnload);
        }
    }

    /**
     * Sets the 'providing' flag of various resources by their UUID.
     * 
     * @param resourceIds The list of resource UUIDs.
     * @param providing True to enable, false to disable.
     */
    @AgiTool(value = "Sets the 'providing' flag of various resources by their UUID. Use this to hide/show resources from the prompt without unloading them.", maxDepth = 2)
    public void setProviding(
            @AgiToolParam(value = "The list of resource UUIDs.", rendererId = "resource") List<String> resourceIds, 
            @AgiToolParam("True to enable, false to disable.") boolean providing) {
        ResourceManager manager = getAgi().getResourceManager();
        for (String id : resourceIds) {
            Resource res = manager.get(id);
            if (res != null) {
                res.setProviding(providing);
                log("Set providing=" + providing + " for resource: " + res.getName());
            } else {
                error("Resource not found: " + id);
            }
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
    @AgiTool(value = "Creates a new text file and registers it as a resource (Will appear on the RAG message).", maxDepth = 2)
    public String createTextFile(@AgiToolParam("The file creation details.") FullTextFileCreate create) throws Exception {
        create.validate(getAgi());

        Path path = Paths.get(create.getPath());
        Files.createDirectories(path.getParent());

        // Final write always uses UTF-8 unless otherwise specified
        Files.writeString(path, create.getContent(), StandardCharsets.UTF_8);

        ResourceHandle handle = getAgi().getConfig().createResourceHandle(path.toUri());
        Resource resource = new Resource(handle);
        getAgi().getResourceManager().register(resource, "Text File Created by " + getActor());

        log("Created text file: " + create.getPath());
        return "File created and resource registered with id " + resource.getId() + " lastModified=" + resource.getLastLoadTimestamp();
    }

    /**
     * Updates an existing text file using full content replacement.
     *
     * @param update The update DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if the update fails.
     */
    @AgiTool(value = "Updates an existing text resource in the RAG message using full content replacement. Returns a standard unified diff of the changes applied. Use only in emergency scenarios if findAndReaplceInTextResource can't do. If you do, Do not quietly remove inline comments or javadocs when using this tool.", maxDepth = 4)
    public String updateTextResource(@AgiToolParam("The update details.") FullTextResourceUpdate update) throws Exception {
        update.validate(getAgi());
        Resource res = getAgi().getResourceManager().getResources().get(update.getResourceUuid());
        String revised = update.calculateResultingContent(getAgi());
        res.write(revised);
        update.setResultingContent(res.asText());
        log("Updated text file: " + res.getName());
        return update.getUnifiedDiff(getAgi()) + "\n---END OF DIFF---\nResource saved. New Last Modified: " + res.getLastLoadTimestamp();
    }

    /**
     * Performs surgical text replacements in an existing file.
     *
     * @param replacements The replacements DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if replacements fail.
     */
    @AgiTool("Performs surgical text replacements in a text resource. "
            + "\n**1. Mandatory Checksum**: You MUST provide the exact `totalOccurrences` of the `target` string found in the file to prove that you know how many occurrences are in the file. If you don't provide it, provide 0 or the provided value doesn't match, the tool will automatically get declined. "
            + "\n**2. Surgical Targeting**: Use `occurrenceIndexes` (a list of 1-based indices) to replace specific matches (e.g., [1, 3]). If the list is null or empty, ALL occurrences are replaced. "
            + "\n**3. Turn Sequencing**: On any given turn, you can only use this tool ONCE per resource. Batch multiple replacements into a single call. "
            + "\n**4. Validation**: Requires `resourceUuid` and the latest `lastModified` timestamp from the RAG message."
            + "\n**5. Coding java**: If you are using this tool for coding java, don't use fqn in method bodies, simply add another replacement for the imports section and import whatever fqns you need.")
    public String findAndReplaceInTextResource(@AgiToolParam("The set of replacements.") TextResourceReplacements replacements) throws Exception {
        replacements.validate(getAgi());
        Resource res = getAgi().getResourceManager().getResources().get(replacements.getResourceUuid());
        String revised = replacements.calculateResultingContent(getAgi());
        res.write(revised);
        replacements.setResultingContent(res.asText());
        log("Performed replacements in: " + res.getName());
        return replacements.getUnifiedDiff(getAgi()) + "\n---END OF DIFF---\nResource saved. New Last Modified: " + res.getLastLoadTimestamp();
    }

    /**
     * Performs a set of semantic line edits (insertions, replacements,
     * deletions) in an existing file.
     * <p>
     * This is the next-generation surgical editor that targets absolute
     * coordinates from the RAG message without requiring mental arithmetic.
     * </p>
     *
     * @param edits The semantic line edits DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if application fails.
     */
//    
//    @AgiTool(value = "An ultra-precise, surgical text resource editor for text resources in the RAG message with 'includeLineNumbers' enabled that no LLM on this day and age can use properly.\n\n "
//            + "Targets absolute 1-based line numbers from the RAG message using semantic intent (Insert, Replace, Delete). "
//            + "You must target the static line numbers of the RAG message, **don't calculate line shifts manually** for a batch of edits, the tool does this."
//            + "\n**Vertification**: is based on **otpimistic locking** with the **lastModified** timestamp in the RAG message and  "
//            + "**not based on surrounding anchors like git-patch style tools**. "
//            + "\n**Boundary Syntax Check**: Before finalizing a range, check the lines immediately above (startLine - 1) and below (endLine + 1). If they contain syntax markers like /**, */, {, or }, ensure you aren't accidentally orphaning them or creating duplicates."
//            + "\n**One tool call per resource per turn**: In any given turn, you can only use this tool once for each resource (you can't call this tool twice for the same resource on the same turn) "
//            + "\n**Do not use LineReplacements when you can achvie the same result with LineInserts**: if you can perform a change using pure insertions do it, never use replacements that start with one or two lines of existing 'anchor' content of the above lines or below lines, just the lines that need changing: For example, if you need to add javadoc to two fields and a constructor that are next to each other, always use 3 inserts rather than 1 big replacement."
//            + "\n\n"
//            + "All line numbers you use when calling this tool must correspond to the exact line numbers in the text resource in the RAG message, all changes are performed based on the line numbers of the resource in the RAG message, the tool handles index shifting automatically.\n\n"
//            + "\n\n**UI**:Your intended edits are presented to the user in a graphical diff viewer where the user reviews your proposed changes and sees the lines that have changed highlighted along with comic-style bubbles (annotations) with your comments / resons on the right hand side of the diff (the tool already works out the line numbers where the annotations on the right hand side of the diff are ment to be shown). "
//            + "\nWhen adding Javadoc or comments, always use LineInsertion unless you are explicitly correcting an existing (and poorly formatted) comment. Replacing a line with 'itself plus more' is a common source of coordinate errors."
//            + "\n\n**Tip**: Before submitting, always check the content of startLine - 1 and endLine + 1 in the RAG message to ensure you are not creating redundant syntax (e.g., double brackets, double javadoc markers, or broken indentation).",
//            permission = ToolPermission.DENY)
    public String editTextResource(
            @AgiToolParam("Contains the resource uuid, the lastModified timestamp and a set of line modifications targeting the absolute 1 based line numbers of a text resource in the RAG message.") TextResourceLineEdits edits) throws Exception {
        edits.validate(getAgi());
        Resource res = getAgi().getResourceManager().getResources().get(edits.getResourceUuid());
        String revised = edits.calculateResultingContent(getAgi());
        res.write(revised);
        edits.setResultingContent(res.asText());
        log("Applied semantic line edits to: " + res.getName());
        return edits.getUnifiedDiff(getAgi()) + "\n---END OF DIFF---\nResource saved. New Last Modified: " + res.getLastLoadTimestamp();
    }
}
