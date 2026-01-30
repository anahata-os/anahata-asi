/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import uno.anahata.asi.model.context.RefreshPolicy;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.model.resource.TextViewportSettings;
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

    /**
     * Updates the viewport settings for a TextFileResource.
     * 
     * @param resourceId The resource identifier.
     * @param newSettings The new viewport settings for the text file.
     * @throws Exception if the resource is not found or reload fails.
     */
    @AiTool(value = "Updates the viewport of a TextFileResource ", retention = 0)
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
    @AiTool(value = "Loads a text file into the context as a managed resource. By default, files are loaded with a LIVE refresh policy, which means they are automatically refreshed from disk right before the API call starts. You do not need to re-load a file if it is already present in the context.", retention = 0)
    public List<TextFileResource> loadTextFile(
            @AiToolParam("The absolute paths to the text files.") List<String> resourcePaths) throws Exception {

        List<TextFileResource> ret = new ArrayList<>(resourcePaths.size());
        for (String path : resourcePaths) {
            try {
                log("Loading " + path + "...");
                ret.add(loadTextFile(path));
                log("Loaded OK " + path);
            } catch (Exception e) {
                log.error("Exception loading text file resource: {}", path, e);
                error("Failed to load " + path + ": " + e.getMessage());
            }
        }

        if (ret.isEmpty()) {
            throw new AiToolException("No files were successfully loaded.");
        }

        return ret;
    }

    /**
     * Loads a single text file into the context.
     * 
     * @param path The absolute path to the file.
     * @return The created resource.
     * @throws Exception if the file is missing or already loaded.
     */
    protected TextFileResource loadTextFile(String path) throws Exception {

        if (getResourceManager().findByPath(path).isPresent()) {
            throw new AiToolException("Resource already loaded for path: " + path);
        }

        if (!java.nio.file.Files.exists(Paths.get(path))) {
            throw new AiToolException("File not found: " + path);
        }

        TextFileResource resource = new TextFileResource(getResourceManager(), Paths.get(path));
        resource.setRefreshPolicy(RefreshPolicy.LIVE);
        getResourceManager().register(resource);
        log("Successfully loaded and registered text file: " + path);
        return resource;
    }

    /**
     * Creates a new file or overwrites an existing one with the provided content.
     * Implements optimistic locking to prevent overwriting concurrent changes.
     * 
     * @param path The absolute path to the file.
     * @param content The text content to write.
     * @param lastModified The expected last modified timestamp (0 for new files).
     * @param message A message describing the change.
     * @throws Exception if an I/O error occurs or locking fails.
     */
    @AiTool(value = "Creates a new file or overwrites an existing one with the provided content.", retention = 0)
    public void writeTextFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The text content to write.") String content,
            @AiToolParam("Optimistic locking: the expected last modified timestamp of the file on disk. Use 0 for new files.") long lastModified,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);
        
        if (java.nio.file.Files.exists(filePath)) {
            long current = java.nio.file.Files.getLastModifiedTime(filePath).toMillis();
            if (lastModified != 0 && current != lastModified) {
                throw new AiToolException("Optimistic locking failure: File has been modified on disk. Expected: " + lastModified + ", Actual: " + current);
            }
        }
        
        if (filePath.getParent() != null) {
            java.nio.file.Files.createDirectories(filePath.getParent());
        }
        java.nio.file.Files.writeString(filePath, content);
        log("Successfully wrote to file: " + path + " (" + message + ")");
    }

    /**
     * Replaces a specific string with another in a file. Ideal for surgical code edits.
     * Implements optimistic locking.
     * 
     * @param path The absolute path to the file.
     * @param target The exact string to be replaced.
     * @param replacement The replacement string.
     * @param lastModified The expected last modified timestamp.
     * @param message A message describing the change.
     * @throws Exception if the target string is not found, I/O error occurs, or locking fails.
     */
    @AiTool(value = "Replaces a specific string with another in a file. Ideal for surgical code edits.", retention = 0)
    public void replaceInFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The exact string to be replaced.") String target,
            @AiToolParam("The replacement string.") String replacement,
            @AiToolParam("Optimistic locking: the expected last modified timestamp of the file on disk.") long lastModified,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);
        
        long current = java.nio.file.Files.getLastModifiedTime(filePath).toMillis();
        if (lastModified != 0 && current != lastModified) {
            throw new AiToolException("Optimistic locking failure: File has been modified on disk. Expected: " + lastModified + ", Actual: " + current);
        }
        
        String content = java.nio.file.Files.readString(filePath);

        if (!content.contains(target)) {
            throw new AiToolException("Target string not found in file: " + path);
        }

        String newContent = content.replace(target, replacement);
        java.nio.file.Files.writeString(filePath, newContent);
        log("Successfully updated file: " + path + " (" + message + ")");

    }

    /**
     * Appends text to the end of an existing file.
     * 
     * @param path The absolute path to the file.
     * @param content The text content to append.
     * @throws Exception if an I/O error occurs.
     */
    @AiTool(value = "Appends text to the end of an existing file.", retention = 0)
    public void appendTextToFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The text content to append.") String content) throws Exception {
        java.nio.file.Files.writeString(Paths.get(path), content, java.nio.file.StandardOpenOption.APPEND);
        log("Successfully appended to file: " + path);
    }
}
