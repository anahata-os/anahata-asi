/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.resource.RefreshPolicy;
import uno.anahata.asi.model.resource.files.TextFileResource;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.tool.AiToolParam;

/**
 * The definitive V2 toolkit for interacting with file-based resources.
 * It provides tools for loading, writing, and surgically editing text files.
 * <p>
 * This toolkit is built on the hierarchical context provider architecture, 
 * allowing files to be managed as stateful resources with configurable 
 * viewports and refresh policies.
 * </p>
 *
 * @author anahata-ai
 */
@AiToolkit("A toolkit for loading and managing file-based resources.")
@Slf4j
public class Files extends AnahataToolkit {
    
    /**
     * Returns the mandatory system instructions for file operations.
     * @return List of instructions.
     * @throws Exception on error.
     */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList("**Files toolkit instructions**:\n "
                + "When writing files, always use the lastModified timestamp from the resource in question on the RAG message, its the source of truth."
                + "Only update text files that you have in context, never attemp to modify a file or any other resource that is not listed in the resources section of the RAG message.");
    }

    /**
     * Updates the viewport settings for a TextFileResource.
     * <p>
     * Implementation details:
     * This method retrieves the resource by ID, updates its internal viewport 
     * settings, and triggers a reload to apply any new filtering or tailing logic.
     * </p>
     * 
     * @param resourceId The resource identifier.
     * @param newSettings The new viewport settings for the text file.
     * @throws Exception if the resource is not found or reload fails.
     */
    @AiTool(value = "Updates the viewport of a TextFileResource ", maxDepth = 12)
    public void updateTextViewportSettings(
            @AiToolParam("The resourceId (not the path).") String resourceId, 
            @AiToolParam("The new viewport settings for the text file") TextViewportSettings newSettings) throws Exception {
        TextFileResource tfr = getResourceManager().getResource(resourceId);
        if (tfr != null) {
            tfr.getViewport().setSettings(newSettings);
            tfr.reload();
        }
    }

    /**
     * Loads one or more text files into the context as managed resources.
     * <p>
     * Implementation details:
     * This method is optimized for bulk operations. It prepares each resource 
     * (checking existence and initializing viewports) and then performs a single 
     * batch registration via {@link uno.anahata.asi.resource.ResourceManager#registerAll(Collection)}. 
     * This prevents the UI from performing redundant rebuilds for every file.
     * </p>
     *
     * @param resourcePaths The absolute paths to the text files.
     * @throws Exception if no files were successfully loaded.
     */
    @AiTool(value = "Loads a text file into the context as a managed resource. By default, files are loaded with a LIVE refresh policy, which means they are automatically refreshed from disk right before the API call starts. You do not need to re-load a file if it is already present in the context.", maxDepth = 12)
    public void loadTextFile(
            @AiToolParam("The absolute paths to the text files.") List<String> resourcePaths) throws Exception {

        if (resourcePaths == null || resourcePaths.isEmpty()) {
            return;
        }

        List<AbstractResource<?, ?>> toRegister = new ArrayList<>();
        int successCount = 0;
        
        for (String path : resourcePaths) {
            try {
                TextFileResource resource = prepareTextFileResource(path, new TextViewportSettings());
                if (resource != null) {
                    toRegister.add(resource);
                    successCount++;
                }
            } catch (Exception e) {
                log.error("Exception preparing text file resource: {}", path, e);
                error("Failed to prepare " + path + ": " + e.getMessage());
            }
        }

        if (!toRegister.isEmpty()) {
            getResourceManager().registerAll(toRegister);
            log("Batch registered " + toRegister.size() + " resources.");
        }

        if (successCount == 0) {
            throw new AiToolException("No files were successfully loaded.");
        }
    }

    /**
     * Loads a text file into the context with specific viewport settings.
     * <p>
     * Implementation details:
     * A convenience wrapper for single-file loading with custom viewport parameters.
     * </p>
     *
     * @param path The absolute path to the file.
     * @param settings The viewport settings to apply.
     * @throws Exception if the file cannot be loaded.
     */
    @AiTool(value = "Loads a text file into the context with specific viewport settings. This is ideal for tailing logs or filtering large files upon loading.", maxDepth = 12)
    public void loadTextFileWithSettings(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The viewport settings.") TextViewportSettings settings) throws Exception {
        loadTextFileInternal(path, settings);
    }

    /**
     * Internal logic for loading or updating a single text file.
     * <p>
     * Implementation details:
     * Checks if a resource for the given path already exists. If so, updates 
     * its settings and reloads it. Otherwise, creates and registers a new instance.
     * </p>
     * 
     * @param path The absolute path to the file.
     * @param settings The viewport settings.
     * @return The created or updated resource.
     * @throws Exception if the file operation fails.
     */
    public TextFileResource loadTextFileInternal(String path, TextViewportSettings settings) throws Exception {

        Optional<TextFileResource> existing = getResourceManager().findByPath(path)
                .filter(r -> r instanceof TextFileResource)
                .map(r -> (TextFileResource) r);
        
        if (existing.isPresent()) {
            updateExistingResource(existing.get(), settings);
            return existing.get();
        }

        TextFileResource resource = prepareTextFileResource(path, settings);
        getResourceManager().register(resource);
        return resource;
    }
    
    /**
     * Prepares a text file resource instance without registering it.
     * <p>
     * Implementation details:
     * Verifies file existence, creates the resource instance, applies 
     * viewport settings, and performs the initial reload.
     * </p>
     * 
     * @param path The absolute path.
     * @param settings The viewport settings.
     * @return The prepared resource instance.
     * @throws Exception if the file is missing or reload fails.
     */
    protected TextFileResource prepareTextFileResource(String path, TextViewportSettings settings) throws Exception {
        if (!java.nio.file.Files.exists(Paths.get(path))) {
            throw new AiToolException("File not found: " + path);
        }
        
        log("Preparing TextFileResource for: " + path );
        TextFileResource resource = createResourceInstance(path);
        
        if (settings != null) {
            log("Applying text view port settings for: " + path);
            resource.getViewport().setSettings(settings);
        }
        
        resource.setRefreshPolicy(RefreshPolicy.LIVE);
        resource.reload();
        return resource;
    }

    /**
     * Hook to update an existing resource during a load request.
     * <p>
     * Implementation details:
     * Updates the viewport and triggers a reload of the file content.
     * </p>
     * 
     * @param resource The existing resource.
     * @param settings The new settings.
     * @throws Exception if the update fails.
     */
    protected void updateExistingResource(TextFileResource resource, TextViewportSettings settings) throws Exception {
        if (settings != null) {
            resource.getViewport().setSettings(settings);
        }
        resource.reload();
        log("Updating existing text file resource: " + resource.getPath());
    }

    /**
     * Factory method for creating the specific resource instance.
     * <p>
     * Implementation details:
     * Instantiates a new {@link TextFileResource}. Subclasses can override this 
     * to provide environment-specific implementations (e.g., {@code NbTextFileResource}).
     * </p>
     * 
     * @param path The absolute path.
     * @return The resource instance.
     * @throws Exception if instantiation fails.
     */
    protected TextFileResource createResourceInstance(String path) throws Exception {
        return new TextFileResource(getResourceManager(), Paths.get(path));
    }

    /**
     * Creates a new file with the provided content.
     * <p>
     * Implementation details:
     * Ensures parent directories exist before delegating to the actual write operation.
     * </p>
     * 
     * @param create The rich creation object for the new file.
     * @param message A message describing the change.
     * @throws Exception if the file already exists or an I/O error occurs.
     */
    @AiTool(value = "Creates a new file with the provided content.", maxDepth = 12)
    public void createTextFile(
            @AiToolParam("The creation details.") FullTextFileCreate create,            
            @AiToolParam("A message describing the change.") String message) throws Exception {
        
        validateCreate(create);
        
        java.nio.file.Path filePath = Paths.get(create.getPath());
        if (filePath.getParent() != null) {
            java.nio.file.Files.createDirectories(filePath.getParent());
        }
        
        performCreate(create, message);
    }

    /**
     * Performs a pre-flight validation for a file creation request.
     * 
     * @param create The creation DTO.
     * @throws Exception if the request is invalid.
     */
    public void validateCreate(FullTextFileCreate create) throws Exception {
        // 1. Physical existence check
        java.nio.file.Path filePath = Paths.get(create.getPath());
        if (java.nio.file.Files.exists(filePath)) {
            throw new AiToolException("File already exists: " + create.getPath());
        }

        // 2. Resource manager context check: file should not be managed if it doesn't exist
        if (getResourceManager().findByPath(create.getPath()).isPresent()) {
            throw new AiToolException("Logic Error: Attempting to create a file that is already registered as a managed resource: " + create.getPath());
        }
    }
    
    /**
     * Performs the actual file creation on disk.
     * <p>
     * Implementation details:
     * Uses standard NIO {@link java.nio.file.Files#writeString} for the operation.
     * </p>
     * 
     * @param create The creation DTO.
     * @param message Descriptive message.
     * @throws Exception on I/O error.
     */
    protected void performCreate(FullTextFileCreate create, String message) throws Exception {
        java.nio.file.Files.writeString(Paths.get(create.getPath()), create.getContent());
        log("Successfully created file: " + create.getPath() + " (" + message + ")");
    }

    /**
     * Overwrites an existing file using a rich update object. 
     * <p>
     * Implementation details:
     * Orchestrates validation (context checks and optimistic locking) before 
     * performing the update.
     * </p>
     * 
     * @param update The update details (path, content, locking, comments).
     * @param message A message describing the change.
     * @throws Exception if validation or update fails.
     */
    @AiTool(value = "Overwrites an existing file. **Never attempt for files that are not in context**. Uses a **filthy rich** update object where you for line comments so the ASI's diff viewer displays the coments in comic style bubbles so you dont need to add inline comments / explanations to the code to later have to remove them.", maxDepth = 12)
    public void updateTextFile(
            @AiToolParam("The update details.") FullTextFileUpdate update,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        
        validateWrite(update);
        performUpdate(update, message);
    }
    
    /**
     * Validates an update or replacement request against the current state of 
     * the resource.
     * <p>
     * Implementation details:
     * 1. Verifies that the file is already a managed resource.
     * 2. Checks physical existence on disk.
     * 3. Performs optimistic locking using the lastModified timestamp.
     * </p>
     * 
     * @param write The file write DTO.
     * @throws Exception if validation fails.
     */
    public void validateWrite(AbstractTextFileWrite write) throws Exception {
        // 1. Context Check
        getResourceManager().findByPath(write.getPath())
                .filter(r -> r instanceof TextFileResource)
                .orElseThrow(() -> new AiToolException("Update rejected: '" + write.getPath() + "' is not a managed resource in the current context. You must load the file first."));

        java.nio.file.Path filePath = Paths.get(write.getPath());
        
        // 2. Existence Check
        if (!java.nio.file.Files.exists(filePath)) {
            throw new AiToolException("File does not exist: " + write.getPath());
        }

        // 3. Optimistic Locking
        long current = java.nio.file.Files.getLastModifiedTime(filePath).toMillis();
        if (write.getLastModified() != 0 && current != write.getLastModified()) {
            throw new AiToolException("Optimistic locking failure: File state has changed in context. \nYou gave: " + write.getLastModified() + ", Current: " + current + "\n. For LIVE resources, always use the lastModified from the resource header in the RAG message or the system instructions if it is a resource with SYSTEM_INSTRUCTIONS Context Position");
        }
    }
    
    /**
     * Performs the actual file update on disk.
     * <p>
     * Implementation details:
     * Uses standard NIO {@link java.nio.file.Files#writeString} for the update.
     * </p>
     * 
     * @param update The update details.
     * @param message Descriptive message.
     * @throws Exception on I/O error.
     */
    protected void performUpdate(FullTextFileUpdate update, String message) throws Exception {
        java.nio.file.Files.writeString(Paths.get(update.getPath()), update.getNewContent());
        log("Successfully updated file: " + update.getPath() + " (" + message + ")");
    }


    /**
     * Performs multiple text replacements in a file. Ideal for surgical code edits.
     * <p>
     * Implementation details:
     * Performs an optimistic locking check before reading the file, applying 
     * replacements, and then delegating to {@link #updateTextFile(FullTextFileUpdate, String)}.
     * </p>
     * 
     * @param replacements The list of replacements to perform.
     * @param message A message describing the change.
     * @throws Exception if replacements fail or locking mismatch occurs.
     */
    @AiTool(value = "Performs multiple text replacements in a file. Ideal for surgical edits but tends to fail on source code files. Shows the 'reason' of each replacement as comic-style bubble on the ASI's diff viewer.", maxDepth = 12)
    public void replaceInTextFile(
            @AiToolParam("The replacements.") TextFileReplacements replacements,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        
        validateWrite(replacements);
        
        java.nio.file.Path filePath = Paths.get(replacements.getPath());
        String content = java.nio.file.Files.readString(filePath);
        String newContent = replacements.performReplacements(content);

        FullTextFileUpdate fullUpdate = new FullTextFileUpdate(replacements.getPath(), replacements.getLastModified(), newContent, Collections.EMPTY_LIST);
        updateTextFile(fullUpdate, message);
    }

    /**
     * Appends text to the end of an existing file.
     * <p>
     * Implementation details:
     * Uses NIO {@link java.nio.file.StandardOpenOption#APPEND}.
     * </p>
     * 
     * @param path The absolute path to the file.
     * @param content The text content to append.
     * @throws Exception if an I/O error occurs.
     */
    @AiTool(value = "Appends text to the end of an existing file.", maxDepth = 12)
    public void appendTextToFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam(value = "The text content to append.", rendererId = "code") String content) throws Exception {
        java.nio.file.Files.writeString(Paths.get(path), content, java.nio.file.StandardOpenOption.APPEND);
        log("Successfully appended to file: " + path);
    }
}
