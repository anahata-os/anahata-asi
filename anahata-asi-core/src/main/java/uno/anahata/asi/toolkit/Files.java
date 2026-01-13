package uno.anahata.asi.toolkit;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import uno.anahata.asi.model.resource.AbstractPathResource;
import uno.anahata.asi.model.resource.AbstractResource;
import uno.anahata.asi.model.resource.TextFileResource;
import uno.anahata.asi.model.resource.TextViewport;
import uno.anahata.asi.tool.AiTool;
import uno.anahata.asi.tool.AiToolException;
import uno.anahata.asi.tool.AiToolkit;
import uno.anahata.asi.tool.AnahataToolkit;
import uno.anahata.asi.tool.AiToolParam;

/**
 * The definitive V2 toolkit for interacting with file-based resources.
 *
 * @author anahata-ai
 */
@AiToolkit("A toolkit for loading and managing file-based resources.")
@Slf4j
public class Files extends AnahataToolkit {

    @AiTool(value = "Updates the viewport of a TextFileResource ", retention = 0)
    public void updateTextFileViewport(
            @AiToolParam("The absolute paths to the text files.") String resourceId, 
            @AiToolParam("The new view port for the text file") TextViewport newViewPort) throws Exception {
        TextFileResource tfr = getResourceManager().getResource(resourceId);
        tfr.setViewport(newViewPort);
        tfr.reload();
    }

    /**
     * Loads a text file into the context as a managed resource. The tool's
     * response is ephemeral and will be pruned from the context on the next
     * turn.
     *
     * @param path The absolute path to the text file.
     * @return The newly created TextFileResource.
     * @throws Exception if the file does not exist, is already loaded, or an
     * I/O error occurs.
     */
    @AiTool(value = "Loads a text file into the context as a managed resource.", retention = 0)
    public List<TextFileResource> loadTextFile(
            @AiToolParam("The absolute paths to the text files.") List<String> resourcePaths) throws Exception {

        List<TextFileResource> ret = new ArrayList<>(resourcePaths.size());
        List<String> errors = new ArrayList<>();
        for (String path : resourcePaths) {
            try {
                log("Loading " + path + "...");
                ret.add(Files.this.loadTextFile(path));
                log("Loaded OK " + path);
            } catch (Exception e) {
                log.error("Exception loading text file resource", e);
                log(ExceptionUtils.getStackTrace(e));
                errors.add(e.getMessage());
                getResponse().addError(ExceptionUtils.getStackTrace(e));
            }
        }
        if (!errors.isEmpty()) {
            super.getResponse().setError(errors.toString());
        }

        if (ret.isEmpty()) {
            throw new AiToolException("Nothing got loaded");
        }

        return ret;
    }

    /**
     * Loads a text file into the context as a managed resource. The tool's
     * response is ephemeral and will be pruned from the context on the next
     * turn.
     *
     * @param path The absolute path to the text file.
     * @return The newly created TextFileResource.
     * @throws Exception if the file does not exist, is already loaded, or an
     * I/O error occurs.
     */
    private TextFileResource loadTextFile(String path) throws Exception {

        if (findByPath(path).isPresent()) {
            throw new AiToolException("Resource already loaded for path: " + path);
        }

        if (!java.nio.file.Files.exists(Paths.get(path))) {
            throw new AiToolException("File not found: " + path);
        }

        TextFileResource resource = new TextFileResource(Paths.get(path));
        getResourceManager().register(resource);
        log("Successfully loaded and registered text file: " + path);
        return resource;
    }

    /**
     * Finds a managed resource by its absolute file path. This is a private
     * helper method that encapsulates the logic specific to this toolkit.
     *
     * @param path The path to search for.
     * @return An Optional containing the resource if found, otherwise empty.
     */
    private Optional<AbstractPathResource> findByPath(String path) {
        return getResourceManager().getResources().stream()
                .filter(r -> r instanceof AbstractPathResource)
                .map(r -> (AbstractPathResource) r)
                .filter(r -> r.getPath().equals(path))
                .findFirst();
    }

    @AiTool(value = "Creates a new file or overwrites an existing one with the provided content.", retention = 0)
    public void writeTextFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The text content to write.") String content) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);
        if (filePath.getParent() != null) {
            java.nio.file.Files.createDirectories(filePath.getParent());
        }
        java.nio.file.Files.writeString(filePath, content);
        log("Successfully wrote to file: " + path);

        findByPath(path).ifPresent(r -> {
            try {
                r.reload();
            } catch (Exception ex) {
                log("Error reloading resource after write: " + ex.getMessage());
            }
        });
    }

    @AiTool(value = "Replaces a specific string with another in a file. Ideal for surgical code edits.", retention = 0)
    public void replaceInFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The exact string to be replaced.") String target,
            @AiToolParam("The replacement string.") String replacement) throws Exception {
        java.nio.file.Path filePath = Paths.get(path);
        String content = java.nio.file.Files.readString(filePath);

        if (!content.contains(target)) {
            throw new AiToolException("Target string not found in file: " + path);
        }

        String newContent = content.replace(target, replacement);
        java.nio.file.Files.writeString(filePath, newContent);
        log("Successfully updated file: " + path);

        findByPath(path).ifPresent(r -> {
            try {
                r.reload();
            } catch (Exception ex) {
                log("Error reloading resource after replacement: " + ex.getMessage());
            }
        });
    }

    @AiTool(value = "Appends text to the end of an existing file.", retention = 0)
    public void appendTextToFile(
            @AiToolParam("The absolute path to the file.") String path,
            @AiToolParam("The text content to append.") String content) throws Exception {
        java.nio.file.Files.writeString(Paths.get(path), content, java.nio.file.StandardOpenOption.APPEND);
        log("Successfully appended to file: " + path);

        findByPath(path).ifPresent(r -> {
            try {
                r.reload();
            } catch (Exception ex) {
                log("Error reloading resource after append: " + ex.getMessage());
            }
        });
    }
}
