/* Licensed under the Anahata Software License (ASL) v 108. See the LICENSE file for details. Força Barça! */
package uno.anahata.asi.toolkit.files;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
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
    public List<TextFileResource> loadTextFile(
            @AiToolParam("The absolute paths to the text files.") List<String> resourcePaths) throws Exception {

        List<TextFileResource> ret = new ArrayList<>(resourcePaths.size());
        for (String path : resourcePaths) {
            try {
                log("Loading " + path + "...");
                ret.add(loadTextFile(path, new TextViewportSettings()));
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
     * Loads a text file into the context with specific viewport settings.
     *
     * @param path The absolute path to the file.
     * @param settings The viewport settings to apply.
     * @return The newly created TextFileResource.
     * @throws Exception if the file cannot be loaded.
     */
    @AiTool(value = "Loads a text file into the context with specific viewport settings. This is ideal for tailing logs or filtering large files upon loading.", maxDepth = 12)
    public TextFileResource loadTextFileWithSettings(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The viewport settings.") TextViewportSettings settings) throws Exception {
        return loadTextFile(path, settings);
    }

    /**
     * Loads a single text file into the context. 
     * If the resource already exists, it updates its settings and reloads it.
     * 
     * @param path The absolute path to the file.
     * @param settings The viewport settings.
     * @return The created or updated resource.
     * @throws Exception if the file is missing.
     */
    protected TextFileResource loadTextFile(String path, TextViewportSettings settings) throws Exception {

        Optional<TextFileResource> existing = getResourceManager().findByPath(path)
                .filter(r -> r instanceof TextFileResource)
                .map(r -> (TextFileResource) r);
        
        if (existing.isPresent()) {
            TextFileResource resource = existing.get();
            resource.getViewport().setSettings(settings);
            resource.reload();
            log("Updating existing text file resource: " + path);
            return resource;
        }

        if (!java.nio.file.Files.exists(Paths.get(path))) {
            throw new AiToolException("File not found: " + path);
        }

        TextFileResource resource = new TextFileResource(getResourceManager(), Paths.get(path));
        resource.getViewport().setSettings(settings);
        resource.setRefreshPolicy(RefreshPolicy.LIVE);
        resource.reload();
        getResourceManager().register(resource);
        log("Successfully loaded and registered text file: " + path);
        return resource;
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
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam(value = "The text content to write.", rendererId = "code") String content,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);
        
        if (java.nio.file.Files.exists(filePath)) {
            throw new AiToolException("File already exists: " + path);
        }
        
        if (filePath.getParent() != null) {
            java.nio.file.Files.createDirectories(filePath.getParent());
        }
        java.nio.file.Files.writeString(filePath, content);
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
            @AiToolParam("The update details.") TextFileUpdate update,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        writeFile(update.getPath(), update.getNewContent(), update.getLastModified(), message);
    }

    /**
     * Internal helper to perform a thread-safe, locked write to a file.
     * 
     * @param path The path to the file.
     * @param content The new content.
     * @param lastModified Expected timestamp for optimistic locking.
     * @param message The change message.
     * @throws Exception if validation or write fails.
     */
    protected void writeFile(String path, String content, long lastModified, String message) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);

        if (!java.nio.file.Files.exists(filePath)) {
            throw new AiToolException("File does not exist: " + path);
        }

        long current = java.nio.file.Files.getLastModifiedTime(filePath).toMillis();
        if (lastModified != 0 && current != lastModified) {
            throw new AiToolException("Optimistic locking failure: File has been modified on disk. Expected: " + lastModified + ", Actual: " + current);
        }

        java.nio.file.Files.writeString(filePath, content);
        log("Successfully updated file: " + path + " (" + message + ")");
    }

    /**
     * Performs multiple text replacements in a file. Ideal for surgical code edits.
     * Implements optimistic locking and occurrence count validation.
     * 
     * @param path The absolute path to the file.
     * @param replacements The list of replacements to perform.
     * @param lastModified The expected last modified timestamp.
     * @param message A message describing the change.
     * @throws Exception if a target string is not found, count mismatch occurs, I/O error occurs, or locking fails.
     */
    @AiTool(value = "Performs multiple text replacements in a file. Ideal for surgical code edits.", maxDepth = 12)
    public void replaceInTextFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The list of replacements to perform.") List<TextReplacement> replacements, 
            @AiToolParam("Optimistic locking: the expected last modified timestamp of the file on disk.") long lastModified,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);
        
        long current = java.nio.file.Files.getLastModifiedTime(filePath).toMillis();
        if (lastModified != 0 && current != lastModified) {
            throw new AiToolException("Optimistic locking failure: File has been modified on disk. Expected: " + lastModified + ", Actual: " + current);
        }
        
        String content = java.nio.file.Files.readString(filePath);
        String newContent = performReplacements(content, replacements);

        java.nio.file.Files.writeString(filePath, newContent);
        log("Successfully updated file with " + replacements.size() + " replacements: " + path + " (" + message + ")");
    }

    /**
     * Performs multiple text replacements across multiple files in a single tool call.
     * 
     * @param fileReplacements The list of files and their respective replacements.
     * @param message A message describing the overall change.
     * @throws Exception if any replacement fails.
     */
    @AiTool(value = "Performs multiple text replacements across multiple files in a single tool call.", maxDepth = 12)
    public void replaceInMultipleTextFiles(
            @AiToolParam("The list of files and their replacements.") List<FileTextReplacements> fileReplacements,
            @AiToolParam("A message describing the change.") String message) throws Exception {
        
        for (FileTextReplacements fr : fileReplacements) {
            replaceInTextFile(fr.getPath(), fr.getReplacements(), fr.getLastModified(), message);
        }
    }

    /**
     * Helper method to perform string replacements with validation.
     * 
     * @param content The original content.
     * @param replacements The list of replacements.
     * @return The updated content.
     * @throws AiToolException if a replacement fails.
     */
    protected String performReplacements(String content, List<TextReplacement> replacements) throws AiToolException {
        String newContent = content;
        for (TextReplacement replacement : replacements) {
            String target = replacement.getTarget();
            int count = StringUtils.countMatches(newContent, target);
            
            if (replacement.getExpectedCount() > 0 && count != replacement.getExpectedCount()) {
                throw new AiToolException("Replacement failed for '" + target + "'. Expected " + replacement.getExpectedCount() + " occurrences, but found " + count);
            }
            
            if (count == 0 && replacement.getExpectedCount() != 0) {
                 throw new AiToolException("Target string not found in file: " + target);
            }

            newContent = newContent.replace(target, replacement.getReplacement());
        }
        return newContent;
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
