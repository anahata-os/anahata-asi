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
import uno.anahata.asi.toolkit.files.FullTextFileCreate;
import uno.anahata.asi.toolkit.files.FullTextResourceUpdate;
import uno.anahata.asi.toolkit.files.TextResourceReplacements;
import uno.anahata.asi.toolkit.files.lines.TextResourceLineEdits;

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
                + "2. **Reasoning**: Always provide a meaningful `reason` each time you update a resource; it will be displayed as an AI comment in the UI.\n"
                + "3. **Updating text resources**: All update text resource tools flush the changes to disk inmediatly when `EXECUTED`.\n "
                + "4. **Rag Message**: The Rag Message is the source of truth for resource modifications, it gets freshly generated when the user completes his turn (i.e. after all tools in the batch have been executed or declined). "
                        + "All resources registered with `LIVE` refresh policy are garanteed to be up to date (in sync) with the underlying storage.\n"
                + "5. **Resources.editTextResource tool**: This is not a git style tool that requires surrounding anchor lines. It is a strict, surgical 1-based line number tool with optimistic locking validation for text resources loaded with includeLineNumbers=true."
                        + " The UI for this tool shows the user a rich graphical diff visualizer with the edits you intend to make to the text resource and overlays comic-style annotations with the reasons for your edits on the right hand side of the diff viewer. "
                        + "\n\tUse this tool **paying careful attention to the line numbers in the RAG message** and use it in a **user-oriented way** choosing the appropiate type of edit (insert / replace / delete) for each logical change you intend to make."
                        + " **\n\tDo not use replacements with existing surrounding lines that need no change when you can perform the edits with pure inserts**. This is very important for the following reasons: "
                        + "\n\ta)Providing surrounding anchor/context lines is prone to fail as this is not the way the tool is designed (its based on optimstic locking, not in matching anchors)."
                        + "\n\tb)The graphical diff visualizer will not display the changes you intend to make to the text resource (because it will not highlight surrounding lines that have not changed)"
                        + "\n\tc)Will cause the comic-style bubbles to be offset (on a line that has no changes)."
                        + "\n\tFor these reasons, when using the editTextResource tool, **you MUST always choose 'inserts' over 'replacements' when possible**.\n"
                        + "\n\tWhen adding Javadoc or comments, always use LineInsertion unless you are explicitly correcting an existing (and poorly formatted) comment. Replacing a line with 'itself plus more' is a common source of coordinate errors."
                        + "\n\tBoundary Syntax Check: Before finalizing a range, check the lines immediately above (startLine - 1) and below (endLine + 1). If they contain syntax markers like /**, */, {, or }, ensure you aren't accidentally orphaning them or creating duplicates."
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
    @AiTool("Updates an existing text resource in the RAG message using full content replacement. Returns a standard unified diff of the changes applied.")
    public String updateTextResource(@AiToolParam("The update details.") FullTextResourceUpdate update) throws Exception {
        update.validate(getAgi());
        Resource res = getAgi().getResourceManager().getResources().get(update.getResourceUuid());
        String revised = update.calculateResultingContent();
        res.write(revised);
        log("Updated text file: " + res.getName());
        return update.getUnifiedDiff();
    }

    /**
     * Performs surgical text replacements in an existing file.
     *
     * @param replacements The replacements DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if replacements fail.
     */
    @AiTool("Performs multiple text replacements in a text resource in the RAG message. Returns a standard unified diff of the changes applied.")
    public String findAndReplaceInTextResource(@AiToolParam("The set of replacements.") TextResourceReplacements replacements) throws Exception {
        replacements.validate(getAgi());
        Resource res = getAgi().getResourceManager().getResources().get(replacements.getResourceUuid());
        String revised = replacements.calculateResultingContent();
        res.write(revised);
        log("Performed replacements in: " + res.getName());
        return replacements.getUnifiedDiff();
    }

    /**
     * Performs a set of semantic line edits (insertions, replacements, deletions) 
     * in an existing file.
     * <p>
     * This is the next-generation surgical editor that targets absolute 
     * coordinates from the RAG message without requiring mental arithmetic.
     * </p>
     * 
     * @param edits The semantic line edits DTO.
     * @return A standard unified diff of the changes applied.
     * @throws Exception if application fails.
     */
    @AiTool("An ultra-precise, surgical text resource editor for text resources in the RAG message with 'includeLineNumbers' enabled. "
            + "Targets absolute 1-based line numbers from the RAG message using semantic intent (Insert, Replace, Delete). "
            + "Vertification is based on line numbers and the lastModified timestamp in the RAG message and  "
            + "not based on surrounding anchors like git patch style tools so do not use replacements (using existing lines as anchors) for changes that can and should be implemented with pure inserts. "
            + "If you need to insert new lines into the resource, use pure insertions instead of replacements that start with one or two lines of existing 'anchor' content."
            + "All line numbers you use when calling this tool must correspond to the line numbers in the text resource in the RAG message, all changes are performed based on the line numbers of the resource in the RAG message at the time of making the call, the tool handles index shifting automatically.\n\n"
            + "This is presented to the user in a graphical diff viewer where he reviews your proposed changes with a comic style bubble overlayed over the code showing your comment/reason for each edit when the user hovers over it. "
            + "Always make sure that each edit (regardless of wether it is an insert a replacement or a delete correspond to a single 'intent' that the user is going to review: for example, if you need to add javadoc to two fields and a constructor that are next to each other, always use 3 inserts rather than 1 big replacement."
            + "When doing replacements, do not include content that it is already in the file. Always be as minimal and surgical as possible and try to no include lines that do not need to change. "
            + "When adding Javadoc or comments, always use LineInsertion unless you are explicitly correcting an existing (and poorly formatted) comment. Replacing a line with 'itself plus more' is a common source of coordinate errors."
            + "Tip: Before submitting, always check the content of startLine - 1 and endLine + 1 in the RAG message to ensure you are not creating redundant syntax (e.g., double brackets, double javadoc markers, or broken indentation).")
    public String editTextResource(
            @AiToolParam("Contains the resource uuid, the lastModified timestamp and a set of line modifications targeting the absolute 1 based line numbers of a text resource in the RAG message.") TextResourceLineEdits edits) throws Exception {
        edits.validate(getAgi());
        Resource res = getAgi().getResourceManager().getResources().get(edits.getResourceUuid());
        String revised = edits.calculateResultingContent();
        res.write(revised);
        log("Applied semantic line edits to: " + res.getName());
        return edits.getUnifiedDiff();
    }
}
