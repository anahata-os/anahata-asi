/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
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
 *
 * @author anahata-ai
 */
@AiToolkit("A toolkit for loading and managing file-based resources.")
@Slf4j
public class Files extends AnahataToolkit {
    
    /** {@inheritDoc} */
    @Override
    public List<String> getSystemInstructions() throws Exception {
        return Collections.singletonList("When writing files, always use the lastModified timestamp from the RAG message");
    }

    /**
     * Updates the viewport settings for a TextFileResource.
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
        tfr.getViewport().setSettings(newSettings);
        tfr.reload();
    }

    /**
     * Loads one or more text files into the context as managed resources.
     *
     * @param resourcePaths The absolute paths to the text files.
     * @return The list of newly created TextFileResources.
     * @throws Exception if no files were successfully loaded.
     */
    @AiTool(value = "Loads a text file into the context as a managed resource. By default, files are loaded with a LIVE refresh policy, which means they are automatically refreshed from disk right before the API call starts. You do not need to re-load a file if it is already present in the context.", maxDepth = 12)
    public void loadTextFile(
            @AiToolParam("The absolute paths to the text files.") List<String> resourcePaths) throws Exception {

        int successCount = 0;
        for (String path : resourcePaths) {
            try {
                log("Loading " + path + "...");
                loadTextFileInternal(path, new TextViewportSettings());
                log("Loaded OK " + path);
                successCount++;
            } catch (Exception e) {
                log.error("Exception loading text file resource: {}", path, e);
                error("Failed to load " + path + ": " + e.getMessage());
            }
        }

        if (successCount == 0) {
            throw new AiToolException("No files were successfully loaded.");
        }
    }

    /**
     * Loads a text file into the context with specific viewport settings.
     *
     * @param path The absolute path to the file.
     * @param settings The viewport settings to apply.
     * @return The newly created TextFileResource.
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
     * Unlike the tool methods, this returns the rich object for internal Java callers.
     * 
     * @param path The absolute path to the file.
     * @param settings The viewport settings.
     * @return The created or updated resource.
     * @throws Exception if the file is missing.
     */
    public TextFileResource loadTextFileInternal(String path, TextViewportSettings settings) throws Exception {

        Optional<TextFileResource> existing = getResourceManager().findByPath(path)
                .filter(r -> r instanceof TextFileResource)
                .map(r -> (TextFileResource) r);
        
        if (existing.isPresent()) {
            updateExistingResource(existing.get(), settings);
            return existing.get();
        }

        return createAndRegisterResource(path, settings);
    }
    
    /**
     * Hook to update an existing resource during a load request.
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
     * Creates, reloads, and registers a new resource.
     * @param path The path.
     * @param settings The settings.
     * @return The new resource.
     * @throws Exception if creation fails.
     */
    protected TextFileResource createAndRegisterResource(String path, TextViewportSettings settings) throws Exception {
        if (!java.nio.file.Files.exists(Paths.get(path))) {
            throw new AiToolException("File not found: " + path);
        }

        TextFileResource resource = createResourceInstance(path);
        if (settings != null) {
            resource.getViewport().setSettings(settings);
        }
        resource.setRefreshPolicy(RefreshPolicy.LIVE);
        resource.reload();
        getResourceManager().register(resource);
        log("Successfully loaded and registered text file: " + path);
        return resource;
    }

    /**
     * Factory method for creating the specific resource instance.
     * @param path The path.
     * @return The instance.
     * @throws Exception if instantiation fails.
     */
    protected TextFileResource createResourceInstance(String path) throws Exception {
        return new TextFileResource(getResourceManager(), Paths.get(path));
    }

    /**
     * Creates a new file with the provided content.
     * 
     * @param path The absolute path to the file.
     * @param content The text content to write.
     * @param message A message describing the change.
     * @throws Exception if the file already exists or an I/O error occurs.
     */
    @AiTool(value = "Creates a new file with the provided content.", maxDepth = 12)
    public void createTextFile(
            @AiToolParam(value = "The text content to write.", rendererId = "code") String content,
            @AiToolParam("The absolute path to the file.") String path,            
            @AiToolParam("A message describing the change.") String message) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);
        
        if (java.nio.file.Files.exists(filePath)) {
            throw new AiToolException("File already exists: " + path);
        }
        
        if (filePath.getParent() != null) {
            java.nio.file.Files.createDirectories(filePath.getParent());
        }
        
        performCreate(path, content, message);
    }
    
    /**
     * Hook to perform the actual file creation.
     */
    protected void performCreate(String path, String content, String message) throws Exception {
        java.nio.file.Files.writeString(Paths.get(path), content);
        log("Successfully created file: " + path + " (" + message + ")");
    }

    /**
     * Overwrites an existing file using a rich update object. 
     * Implements optimistic locking and is optimized for the ASI's diff viewer.
     * 
     * @param update The update details (path, content, locking, comments).
     * @param message A message describing the change.
     * @throws Exception if the file does not exist, locking fails, or an I/O error occurs.
     */
    @AiTool(value = "Overwrites an existing file using a rich update object. Optimized for the ASI's diff viewer.", maxDepth = 12)
    public void updateTextFile(
            @AiToolParam("The update details.") FullTextFileUpdate update,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        
        validateUpdate(update);
        performUpdate(update, message);
    }
    
    /**
     * Validates an update request against the current state of the resource.
     * @param update The update.
     * @throws Exception if validation fails.
     */
    protected void validateUpdate(FullTextFileUpdate update) throws Exception {
        // 1. Context Check
        getResourceManager().findByPath(update.getPath())
                .filter(r -> r instanceof TextFileResource)
                .orElseThrow(() -> new AiToolException("Update rejected: '" + update.getPath() + "' is not a managed resource in the current context. You must load the file first."));

        java.nio.file.Path filePath = Paths.get(update.getPath());
        
        // 2. Existence Check
        if (!java.nio.file.Files.exists(filePath)) {
            throw new AiToolException("File does not exist: " + update.getPath());
        }

        // 3. Optimistic Locking
        long current = java.nio.file.Files.getLastModifiedTime(filePath).toMillis();
        if (update.getLastModified() != 0 && current != update.getLastModified()) {
            throw new AiToolException("Optimistic locking failure: File has been modified on disk. Given: " + update.getLastModified() + ", Actual: " + current);
        }
    }
    
    /**
     * Hook to perform the actual file update.
     */
    protected void performUpdate(FullTextFileUpdate update, String message) throws Exception {
        java.nio.file.Files.writeString(Paths.get(update.getPath()), update.getNewContent());
        log("Successfully updated file: " + update.getPath() + " (" + message + ")");
    }


    /**
     * Performs multiple text replacements in a file. Ideal for surgical code edits.
     * Implements optimistic locking and occurrence count validation.
     * 
     * @param replacements The list of replacements to perform.
     * @param message A message describing the change.
     * @throws Exception if a target string is not found, count mismatch occurs, I/O error occurs, or locking fails.
     */
    @AiTool(value = "Performs multiple text replacements in a file. Ideal for surgical code edits.", maxDepth = 12)
    public void replaceInTextFile(
            @AiToolParam("The replacements.") TextFileReplacements replacements,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        java.nio.file.Path filePath = Paths.get(replacements.getPath());
        
        long current = java.nio.file.Files.getLastModifiedTime(filePath).toMillis();
        if (replacements.getLastModified() != 0 && current != replacements.getLastModified()) {
            throw new AiToolException("Optimistic locking failure: File has been modified on disk. Given: " + replacements.getLastModified() + ", Actual: " + current);
        }
        
        String content = java.nio.file.Files.readString(filePath);
        String newContent = replacements.performReplacements(content);

        FullTextFileUpdate fullUpdate = new FullTextFileUpdate(replacements.getPath(), replacements.getLastModified(), newContent, Collections.EMPTY_LIST);
        updateTextFile(fullUpdate, message);
    }

    /**
     * Appends text to the end of an existing file.
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
